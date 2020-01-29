data_node <- R6Class(
  "data_node",
  inherit = node,
  public = list(

    initialize = function(data) {

      # coerce to an array with 2+ dimensions
      data <- as_2d_array(data)

      # update and store array and store dimension
      super$initialize(dim = dim(data), value = data)

    },

    tf = function(dag) {

      tfe <- dag$tf_environment
      tf_name <- dag$tf_name(self)

      value <- self$value()
      shape <- to_shape(c(1, dim(value)))
      value <- add_first_dim(value)

      # under some circumstances we define data as constants, but normally as
      # placeholders
      using_constants <- !is.null(greta_stash$data_as_constants)

      if (using_constants) {

        tensor <- tf$constant(value = value,
                              dtype = tf_float(),
                              shape = shape)

      } else {

        tensor <- tf$compat$v1$placeholder(shape = shape,
                                           dtype = tf_float())
        tfe$data_list[[tf_name]] <- value

      }

      # create placeholder
      assign(tf_name, tensor, envir = tfe)

    }
  )
)

# a node for applying operations to values
operation_node <- R6Class(
  "operation_node",
  inherit = node,
  public = list(

    operation_name = NA,
    operation = NA,
    operation_args = NA,
    arguments = list(),
    tf_function_env = NA,

    # named greta arrays giving different representations of the greta array
    # represented by this node that have already been calculated, to be used for
    # computational speedups or numerical stability. E.g. a logarithm or a
    # cholesky factor
    representations = list(),

    initialize = function(operation,
                          ...,
                          dim = NULL,
                          operation_args = list(),
                          tf_operation = NULL,
                          value = NULL,
                          representations = list(),
                          tf_function_env = parent.frame(3),
                          expand_scalars = FALSE) {

      # coerce all arguments to nodes, and remember the operation
      dots <- lapply(list(...), as.greta_array)

      # work out the dimensions of the new greta array, if NULL assume an
      # elementwise operation and get the largest number of each dimension,
      # otherwise expect a function to be passed which will calculate it from
      # the provided list of nodes arguments
      if (is.null(dim)) {
        dim_list <- lapply(dots, dim)
        dim_lengths <- vapply(dim_list, length, numeric(1))
        dim_list <- lapply(dim_list, pad_vector, to_length = max(dim_lengths))
        dim <- do.call(pmax, dim_list)
      }

      # expand scalar arguments to match dim if needed
      if (!identical(dim, c(1L, 1L)) & expand_scalars) {
        dots <- lapply(dots, `dim<-`, dim)
      }

      for (greta_array in dots) {
        self$add_argument(get_node(greta_array))
      }

      self$operation_name <- operation
      self$operation <- tf_operation
      self$operation_args <- operation_args
      self$representations <- representations
      self$tf_function_env <- tf_function_env

      # assign empty value of the right dimension, or the values passed via the
      # operation
      if (is.null(value))
        value <- unknowns(dim = dim)
      else if (!all.equal(dim(value), dim))
        stop("values have the wrong dimension so cannot be used")

      super$initialize(dim, value)

    },

    add_argument = function(argument) {

      # guess at a name, coerce to a node, and add as a parent
      parameter <- to_node(argument)
      self$add_parent(parameter)

    },

    tf = function(dag) {

      # fetch the tensors for the environment
      arg_tf_names <- lapply(self$parents, dag$tf_name)
      args <- lapply(arg_tf_names, get, envir = dag$tf_environment)

      # fetch additional (non-tensor) arguments, if any
      if (length(self$operation_args) > 0)
        args <- c(args, self$operation_args)

      # if there are multiple args, reconcile batch dimensions now
      args <- match_batches(args)

      # get the tensorflow function
      operation <- eval(parse(text = self$operation),
                        envir = self$tf_function_env)

      # apply the function on tensors
      node <- do.call(operation, args)

      # assign it in the environment
      assign(dag$tf_name(self), node, envir = dag$tf_environment)

    }
  )
)

variable_node <- R6Class(
  "variable_node",
  inherit = node,
  public = list(

    constraint = NULL,
    lower = -Inf,
    upper = Inf,
    free_value = NULL,

    initialize = function(lower = -Inf, upper = Inf, dim = NULL, free_dim = prod(dim)) {

      if (!is.numeric(lower) | ! is.numeric(upper)) {

        stop("lower and upper must be numeric",
             call. = FALSE)

      }

      # check and assign limits
      universal_limits <- length(lower) == 1 & length(upper) == 1

      # find constraint type if they are all the same
      if (!universal_limits) {

        self$constraint <- "scalar_mixed"

      } else {

        lower_limit <- lower != -Inf
        upper_limit <- upper != Inf

        if (!lower_limit & !upper_limit)
          self$constraint <- "scalar_none"
        else if (!lower_limit & upper_limit)
          self$constraint <- "scalar_all_low"
        else if (lower_limit & !upper_limit)
          self$constraint <- "scalar_all_high"
        else if (lower_limit & upper_limit)
          self$constraint <- "scalar_all_low_high"

      }

      bad_limits <- switch(self$constraint,
                           low = !is.finite(upper),
                           high = !is.finite(lower),
                           both = !is.finite(lower) | !is.finite(upper),
                           FALSE)

      if (bad_limits) {

        stop("lower and upper must either be -Inf (lower only), ",
             "Inf (upper only) or finite",
             call. = FALSE)

      }

      if (any(lower >= upper)) {

        stop("upper bounds must be greater than lower bounds",
             call. = FALSE)

      }

      # replace values of lower and upper with finite values for dimension
      # checking (this is pain, but necessary because check_dims coerces to
      # greta arrays, which must be finite)
      lower_for_dim <- lower
      lower_for_dim[] <- 0
      upper_for_dim <- upper
      upper_for_dim[] <- 0
      dim <- check_dims(lower_for_dim, upper_for_dim, target_dim = dim)

      # add parameters
      super$initialize(dim)
      self$lower <- lower
      self$upper <- upper

      # set the free state version of value
      self$free_value <- unknowns(dim = free_dim)

    },

    # handle two types of value for variables
    value = function(new_value = NULL, free = FALSE, ...) {

      if (free) {

        if (is.null(new_value)) {
          self$free_value
        } else {
          self$free_value <- new_value
        }

      } else {

        super$value(new_value, ...)

      }

    },

    tf = function(dag) {

      # get the names of the variable and (already-defined) free state version
      tf_name <- dag$tf_name(self)
      free_name <- sprintf("%s_free", tf_name)

      # create the log jacobian adjustment for the free state
      tf_adj <- self$tf_adjustment(dag)
      adj_name <- sprintf("%s_adj", tf_name)
      assign(adj_name,
             tf_adj,
             envir = dag$tf_environment)

      # get the free tensor
      tf_free <- get(free_name, envir = dag$tf_environment)

      # map from the free to constrained state (including reshaping) in a new tensor
      tf_transformed <- self$tf_from_free(tf_free)

      # assign as constrained variable
      assign(tf_name,
             tf_transformed,
             envir = dag$tf_environment)

    },

    create_tf_bijector = function() {

      switch(
        self$constraint,
          scalar_none = tf_scalar_bijector(
          self$dim
        ),
        scalar_all_low = tf_scalar_neg_bijector(
          self$dim,
          self$upper
        ),
        scalar_all_high = tf_scalar_pos_bijector(
          self$dim,
          self$lower
        ),
        scalar_all_low_high = tf_scalar_neg_pos_bijector(
          self$dim,
          self$lower,
          self$upper
        ),
        scalar_mixed = tf_scalar_neg_pos_bijector(
          self$dim,
          self$lower,
          self$upper
        ),
        correlation_matrix = tf_correlation_cholesky_bijector(),
        covariance_matrix = tf_covariance_cholesky_bijector(),
        simplex = tf_simplex_bijector(self$dim)
      )

    },

    tf_from_free = function(x) {

      tf_bijector <- self$create_tf_bijector()
      tf_bijector$forward(x)

    },

    # adjustments for univariate variables
    tf_log_jacobian_adjustment = function(free) {

      tf_bijector <- self$create_tf_bijector()

      event_ndims <- tf_bijector$forward_min_event_ndims
      ljd <- tf_bijector$forward_log_det_jacobian(
        x = free,
        event_ndims = event_ndims
      )

      # sum across all dimensions of jacobian
      already_summed <-
        identical(dim(ljd), list(NULL)) | identical(dim(ljd), list())

      if (!already_summed) {
        ljd <- tf_sum(ljd, drop = TRUE)
      }

      # make sure there's something in the batch dimension
      if (identical(dim(ljd), list())) {
        ljd <- tf$expand_dims(ljd, 0L)
      }

      ljd

    },

    # create a tensor giving the log jacobian adjustment for this variable
    tf_adjustment = function(dag) {

      # find free version of node
      free_tensor_name <- paste0(dag$tf_name(self), "_free")
      free_tensor <- get(free_tensor_name, envir = dag$tf_environment)

      # apply jacobian adjustment to it
      self$tf_log_jacobian_adjustment(free_tensor)

    }

  )
)

distribution_node <- R6Class(
  "distribution_node",
  inherit = node,
  public = list(
    distribution_name = "no distribution",
    discrete = NA,
    multivariate = NA,
    truncatable = NA,
    target = NULL,
    user_node = NULL,
    bounds = c(-Inf, Inf),
    truncation = NULL,
    parameters = list(),

    initialize = function(name = "no distribution",
                          dim = NULL,
                          truncation = NULL,
                          discrete = FALSE,
                          multivariate = FALSE,
                          truncatable = TRUE) {

      super$initialize(dim)

      # for all distributions, set name, store dims, and set whether discrete
      self$distribution_name <- name
      self$discrete <- discrete
      self$multivariate <- multivariate
      self$truncatable <- truncatable

      # initialize the target values of this distribution
      self$add_target(self$create_target(truncation))

      # if there's a truncation, it's different from the bounds, and it's
      # truncatable (currently that's only univariate and continuous-discrete
      # distributions) set the truncation
      can_be_truncated <- !self$multivariate & !self$discrete & self$truncatable

      if (!is.null(truncation) &
          !identical(truncation, self$bounds) &
          can_be_truncated) {

        self$truncation <- truncation

      }

      # set the target as the user node (user-facing representation) by default
      self$user_node <- self$target

    },

    # create a target variable node (unconstrained by default)
    create_target = function(truncation) {
      vble(truncation, dim = self$dim)
    },

    # create target node, add as a parent, and give it this distribution
    add_target = function(new_target) {

      # add as x and as a parent
      self$target <- new_target
      self$add_parent(new_target)

      # get its values
      self$value(new_target$value())

      # give self to x as its distribution
      self$target$set_distribution(self)

      # optionally reset any distribution flags relating to the previous target
      self$reset_target_flags()

    },

    # optional function to reset the flags for target representations whenever a
    # target is changed
    reset_target_flags = function() {

    },

    # replace the existing target node with a new one
    remove_target = function() {

      # remove x from parents
      self$remove_parent(self$target)
      self$target <- NULL

    },

    # change this to instead assign the distribution object, with the density
    # being defined by the dag class

    tf = function(dag) {

      # assign the distribution object constructor function to the environment
      assign(dag$tf_name(self),
             self$tf_distrib,
             envir = dag$tf_environment)

    },

    # which node to use as the *tf* target (overwritten by some distributions)
    get_tf_target_node = function() {
      self$target
    },

    add_parameter = function(parameter, name, expand_scalar_to = self$dim) {

      # expand out a scalar parameter if needed
      if (!is.null(expand_scalar_to) &&
          is_scalar(parameter) & !identical(expand_scalar_to, c(1L, 1L))) {
        parameter <- greta_array(parameter, dim = expand_scalar_to)
      }

      parameter <- to_node(parameter)
      self$add_parent(parameter)
      self$parameters[[name]] <- parameter

    }

  )
)

# modules for export via .internals
node_classes_module <- module(node,
                              distribution_node,
                              data_node,
                              variable_node,
                              operation_node)


# shorthand for distribution parameter constructors
distrib <- function(distribution, ...) {

  check_tf_version("error")

  # get and initialize the distribution, with a default value node
  constructor <- get(paste0(distribution, "_distribution"),
                     envir = parent.frame())
  distrib <- constructor$new(...)

  # return the user-facing representation of the node as a greta array
  value <- distrib$user_node
  as.greta_array(value)

}

# shorthand to speed up op definitions
op <- function(...) {
  as.greta_array(operation_node$new(...))
}

# helper function to create a variable node
# by default, make x (the node
# containing the value) a free parameter of the correct dimension
vble <- function(truncation, dim = 1, free_dim = prod(dim)) {

  if (is.null(truncation))
    truncation <- c(-Inf, Inf)

  variable_node$new(lower = truncation[1],
                    upper = truncation[2],
                    dim = dim,
                    free_dim = free_dim)

}

node_constructors_module <- module(distrib,
                                   op,
                                   vble)
