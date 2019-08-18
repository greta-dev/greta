data_node <- R6Class(
  "data_node",
  inherit = node,
  public = list(

    initialize = function(data) {

      # coerce to an array with 2+ dimensions
      data <- as_2D_array(data)

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
        dag$set_tf_data_list(tf_name, value)

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
                          tf_function_env = parent.frame(3)) {

      # coerce all arguments to nodes, and remember the operation
      dots <- lapply(list(...), as.greta_array)
      for (greta_array in dots)
        self$add_argument(get_node(greta_array))

      self$operation_name <- operation
      self$operation <- tf_operation
      self$operation_args <- operation_args
      self$representations <- representations
      self$tf_function_env <- tf_function_env

      # work out the dimensions of the new greta array, if NULL assume an
      # elementwise operation and get the largest number of each dimension,
      # otherwise expect a function to be passed which will calculate it from
      # the provided list of nodes arguments
      if (is.null(dim))
        dim <- do.call(pmax, lapply(dots, dim))

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
      arg_tf_names <- lapply(self$list_parents(dag), dag$tf_name)
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

    initialize = function(lower = -Inf, upper = Inf, dim = 1) {

      good_types <- is.numeric(lower) && length(lower) == 1 &
        is.numeric(upper) && length(upper) == 1

      if (!good_types) {

        stop("lower and upper must be numeric vectors of length 1",
             call. = FALSE)

      }

      # check and assign limits
      bad_limits <- TRUE

      # find constraint type
      if (lower == -Inf & upper == Inf) {

        self$constraint <- "none"
        bad_limits <- FALSE

      } else if (lower == -Inf & upper != Inf) {

        self$constraint <- "low"
        bad_limits <- !is.finite(upper)

      } else if (lower != -Inf & upper == Inf) {

        self$constraint <- "high"
        bad_limits <- !is.finite(lower)

      } else if (lower != -Inf & upper != Inf) {

        self$constraint <- "both"
        bad_limits <- !is.finite(lower) | !is.finite(upper)

      }

      if (bad_limits) {

        stop("lower and upper must either be -Inf (lower only), ",
             "Inf (upper only) or finite scalars",
             call. = FALSE)

      }

      if (lower >= upper) {

        stop("upper bound must be greater than lower bound",
             call. = FALSE)

      }

      # add parameters
      super$initialize(dim)
      self$lower <- lower
      self$upper <- upper

    },

    # flip the distribution node from being a child to a parent if sampling
    list_parents = function(dag) {

      parents <- self$parents
      distribution_node <- self$distribution

      # if we're in sampling mode and this variable has a distribution, consider
      # that a parent node too
      if (dag$mode == "sampling" & !is.null(distribution_node)) {
        parents <- c(parents, list(distribution_node))
      }

      parents

    },

    # flip the distribution node from being a child to a parent if sampling
    list_children = function(dag) {

      children <- self$children
      distribution_node <- self$distribution

      # if we're in sampling mode and this variable has a distribution, do not consider
      # that a child node
      if (dag$mode == "sampling" & !is.null(distribution_node)) {
        child_names <- vapply(children, member, "unique_name", FUN.VALUE = character(1))
        keep <- child_names != distribution_node$unique_name
        children <- children[keep]
      }

      children

    },

    tf = function(dag) {

      # get the names of the variable and (already-defined) free state version
      tf_name <- dag$tf_name(self)

      if (dag$mode == "sampling") {

        distrib_node <- self$distribution

        if (is.null(distrib_node)) {

          # if the variable has no distribution create a placeholder instead
          # (the value must be passed in via values when using simulate)
          shape <- to_shape(c(1, self$dim))
          tensor <- tf$compat$v1$placeholder(shape = shape, dtype = tf_float())

        } else {

          # otherwise find the sampling function for the distribution, and apply it to
          # get the tensor for the sample
          distrib_tf_name <- dag$tf_name(distrib_node)
          sampling <- get(distrib_tf_name, envir = dag$tf_environment)
          tensor <- sampling()

        }

      }

      # if we're defining the forward mode graph, get the free state, transform,
      # and compute any transformation density
      if (dag$mode == "forward") {

        free_name <- sprintf("%s_free", tf_name)

        # create the log jacobian adjustment for the free state
        tf_adj <- self$tf_adjustment(dag)
        adj_name <- sprintf("%s_adj", tf_name)
        assign(adj_name,
               tf_adj,
               envir = dag$tf_environment)

        # map from the free to constrained state in a new tensor
        tf_free <- get(free_name, envir = dag$tf_environment)
        tensor <- self$tf_from_free(tf_free, dag$tf_environment)

        # reshape the tensor to the match the variable
        tensor <- tf$reshape(tensor,
                             shape = to_shape(c(-1, dim(self))))

      }

      # assign to environment variable
      assign(tf_name,
             tensor,
             envir = dag$tf_environment)



    },

    tf_from_free = function(x, env) {

      upper <- self$upper
      lower <- self$lower

      if (self$constraint == "none") {

        y <- x

      } else if (self$constraint == "both") {

        y <- tf$nn$sigmoid(x) * fl(upper - lower) + fl(lower)

      } else if (self$constraint == "low") {

        y <- fl(upper) - tf$exp(x)

      } else if (self$constraint == "high") {

        y <- tf$exp(x) + fl(lower)

      } else {

        y <- x

      }

      y

    },

    # adjustments for univariate variables
    tf_log_jacobian_adjustment = function(free) {

      ljac_none <- function(x) {
        zero <- tf$constant(0, dtype = tf_float(), shape = shape(1))
        tf$tile(zero, multiples = list(tf$shape(x)[[0]]))
      }

      ljac_exp <- function(x) {
        tf_sum(x, drop = TRUE)
      }

      ljac_logistic <- function(x) {
        lrange <- log(self$upper - self$lower)
        tf_sum(x - fl(2) * tf$nn$softplus(x) + lrange,
               drop = TRUE)
      }

      ljac_corr_mat <- function(x) {

        # find dimension
        K <- dim(x)[[2]]
        n <- (1 + sqrt(8 * K + 1)) / 2

        # convert to correlation-scale (-1, 1) & get log jacobian
        z <- tf$tanh(x)

        free_to_correl_lp <- tf_sum(log(fl(1) - tf$square(z)))
        free_to_correl_lp <- tf$squeeze(free_to_correl_lp, 1L)

        # split z up into rows
        z_rows <- tf$split(z, 1:(n - 1), axis = 1L)

        # accumulate log prob within each row
        lps <- lapply(z_rows, tf_corrmat_row, which = "ljac")
        correl_to_mat_lp <- tf$add_n(lps)

        free_to_correl_lp + correl_to_mat_lp

      }

      ljac_cov_mat <- function(x) {

        # find dimension
        n <- dim(x)[[2]]
        K <- (sqrt(8 * n + 1) - 1) / 2

        k <- seq_len(K)
        powers <- tf$constant(K - k + 2, dtype = tf_float(), shape = shape(K))
        fl(K * log(2)) + tf_sum(powers * x[, k - 1], drop = TRUE)

      }

      fun <- switch(self$constraint,
                    none = ljac_none,
                    high = ljac_exp,
                    low = ljac_exp,
                    both = ljac_logistic,
                    correlation_matrix = ljac_corr_mat,
                    covariance_matrix = ljac_cov_mat)

      fun(free)

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
    target = NULL,
    user_node = NULL,
    bounds = c(-Inf, Inf),
    truncation = NULL,
    parameters = list(),

    initialize = function(name = "no distribution",
                          dim = NULL,
                          truncation = NULL,
                          discrete = FALSE) {

      super$initialize(dim)

      # for all distributions, set name, store dims, and set whether discrete
      self$distribution_name <- name
      self$discrete <- discrete

      # initialize the target values of this distribution
      self$add_target(self$create_target(truncation))

      # if there's a truncation, it's different from the bounds, and it's a
      # truncatable distribution, set the truncation
      if (!is.null(truncation) &
          !identical(truncation, self$bounds) &
          !is.null(self$tf_cdf_function)) {

        self$truncation <- truncation

      }

      # set the target as the user node (user-facing representation) by default
      self$user_node <- self$target

    },

    # create a target variable node (unconstrained by default)
    create_target = function(truncation) {
      vble(truncation, dim = self$dim)
    },

    # flip the target node from being a parent to a child when sampling
    list_parents = function(dag) {

      parents <- self$parents
      target_node <- self$target

      # if we're in sampling mode and this variable has a target, do not consider
      # that a parent node
      if (dag$mode == "sampling" & !is.null(target_node)) {
        parent_names <- vapply(parents, member, "unique_name", FUN.VALUE = character(1))
        keep <- parent_names != target_node$unique_name
        parents <- parents[keep]
      }

      parents

    },

    # flip the target node from being a parent to a child when sampling
    list_children = function(dag) {

      children <- self$children
      target_node <- self$distribution

      # if we're in sampling mode and this variable has a target, consider
      # that a child node too
      if (dag$mode == "sampling" & !is.null(target_node)) {
        children <- c(children, list(target_node))
      }

      children

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

    tf = function(dag) {

      # for distributions, tf assigns a *function* to execute either the density
      # or sampling

      # log density of the distribution, given parameter values
      density <- function(tf_target) {

        # fetch inputs
        tf_parameters <- self$tf_fetch_parameters(dag)

        # ensure x and the parameters are all expanded to have the correct batch
        # size, if any have it
        target_params <- match_batches(c(list(tf_target), tf_parameters))
        tf_target <- target_params[[1]]
        tf_parameters <- target_params[-1]

        # calculate log density
        ld <- self$tf_log_density_function(tf_target, tf_parameters, dag)

        # check for truncation
        if (!is.null(self$truncation))
          ld <- ld - self$tf_log_density_offset(tf_parameters)

        ld

      }

      # random sample of the variable from the distribution
      sampling <- function () {

        # fetch inputs
        tf_parameters <- self$tf_fetch_parameters(dag)

        # ensure the parameters are all expanded to have the correct batch
        # size, if any have it
        batch_dummy <- self$tfe$batch_dummy
        dummy_params <- match_batches(c(list(batch_dummy), tf_parameters))
        tf_parameters <- dummy_params[-1]

        # sample
        self$tf_sample(tf_parameters, dag)

      }

      fun <- switch(
        dag$mode,
        forward = density,
        sampling = sampling
      )

      # assign the function to the environment
      assign(dag$tf_name(self),
             fun,
             envir = dag$tf_environment)

    },

    # which node to use as the *tf* target (overwritten by some distributions)
    get_tf_target_node = function() {
      self$target
    },

    tf_fetch_parameters = function(dag) {
      # fetch the tensors corresponding to this node's parameters from the
      # environment, and return them in a named list

      # find names
      tf_names <- lapply(self$parameters, dag$tf_name)

      # fetch tensors
      lapply(tf_names, get, envir = dag$tf_environment)

    },

    tf_log_density_offset = function(parameters) {

      # calculate the log-adjustment to the truncation term of the density
      # function i.e. the density of a distribution, truncated between a and b,
      # is the non truncated density, divided by the integral of the density
      # function between the truncation bounds. This can be calculated from the
      # distribution's CDF

      lower <- self$truncation[1]
      upper <- self$truncation[2]

      if (lower == self$bounds[1]) {

        # if only upper is constrained, just need the cdf at the upper
        offset <- self$tf_log_cdf_function(fl(upper), parameters)

      } else if (upper == self$bounds[2]) {

        # if only lower is constrained, get the log of the integral above it
        offset <- tf$math$log(fl(1) - self$tf_cdf_function(fl(lower),
                                                           parameters))

      } else {

        # if both are constrained, get the log of the integral between them
        offset <- tf$math$log(self$tf_cdf_function(fl(upper), parameters) -
                                self$tf_cdf_function(fl(lower), parameters))

      }

      offset

    },

    add_parameter = function(parameter, name) {

      parameter <- to_node(parameter)
      self$add_parent(parameter)
      self$parameters[[name]] <- parameter

    },

    tf_log_density_function = function(x, parameters, dag) {

      self$tf_distrib(parameters, dag)$log_prob(x)

    },

    tf_cdf_function = function(x, parameters, dag) {

      self$tf_distrib(parameters, dag)$cdf(x)

    },

    tf_log_cdf_function = function(x, parameters, dag) {

      self$tf_distrib(parameters, dag)$log_cdf(x)

    },

    tf_sample = function(parameters, dag) {

      self$tf_distrib(parameters, dag)$sample()

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
vble <- function(truncation, dim = 1) {

  if (is.null(truncation))
    truncation <- c(-Inf, Inf)

  variable_node$new(lower = truncation[1],
                    upper = truncation[2],
                    dim = dim)

}

node_constructors_module <- module(distrib,
                                   op,
                                   vble)
