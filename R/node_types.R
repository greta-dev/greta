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
      unbatched_name <- glue::glue("{tf_name}_unbatched")

      mode <- dag$how_to_define(self)

      # if we're in sampling mode, get the distribution constructor and sample
      if (mode == "sampling") {
        batched_tensor <- dag$draw_sample(self$distribution)
      }

      # if we're defining the forward mode graph, create either a constant or a
      # placeholder
      if (mode == "forward") {
        value <- self$value()
        ndim <- n_dim(value)
        shape <- to_shape(c(1, dim(value)))
        value <- add_first_dim(value)

        # under some circumstances we define data as constants, but normally as
        # placeholders
        using_constants <- !is.null(greta_stash$data_as_constants)

        if (using_constants) {
          unbatched_tensor <- tf$constant(
            value = value,
            dtype = tf_float(),
            shape = shape
          )
        } else {
          # TF1/2 check
          # We can pass tensors directly into ops and layers
          # tf.function arguments do the job of placeholders
          # or we can use tf$keras$Input ?
          # unbatched_tensor <- tf$keras$Input(
          # for data - find yourself so it can be substituted in
          # we need to fetch the data from the DAG
          # what is the TF2 method for casting data into a tensor
          # we can probably just use `as_tensor`
          unbatched_tensor <- tensorflow::as_tensor(
            x = value,
            shape = shape,
            dtype = tf_float()
          )
          # TF1/2 check
          # note - we might not need this anymore as it was to do with
          # stashing things for use in the feed_dict later
          dag$set_tf_data_list(unbatched_name, value)
        }

        # expand up to batch size - so we can run multiple chains
        tiling <- c(tfe$batch_size, rep(1L, ndim))
        batched_tensor <- tf$tile(unbatched_tensor, tiling)

        # put unbatched tensor in environment so it can be set
        assign(unbatched_name, unbatched_tensor, envir = tfe)
      }

      assign(tf_name, batched_tensor, envir = tfe)
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
        dim_lengths <- lengths(dim_list)
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
      self$make_antirepresentations(representations)
      self$tf_function_env <- tf_function_env

      # assign empty value of the right dimension, or the values passed via the
      # operation
      check_values_dim(value, dim)

      value <- value %||% unknowns(dim = dim)

      super$initialize(dim, value)
    },
    add_argument = function(argument) {

      # guess at a name, coerce to a node, and add as a parent
      parameter <- to_node(argument)
      self$add_parent(parameter)
    },
    tf = function(dag) {
      # where to put it
      tfe <- dag$tf_environment
      # what to call the tensor object
      tf_name <- dag$tf_name(self)

      # cholesky
      # maybe put this warning inside the calculate part
      # !! check whether the change to define tf will break
      mode <- dag$how_to_define(self)
      # if sampling get the distribution constructor and sample this
      if (mode == "sampling") {
        tensor <- dag$draw_sample(self$distribution)
      }

      if (mode == "forward") {

        # fetch the tensors from the environment
        arg_tf_names <- lapply(self$list_parents(dag), dag$tf_name)
        tf_args <- lapply(arg_tf_names, get, envir = tfe)

        # fetch additional (non-tensor) arguments, if any
        multiple_operation_args <- length(self$operation_args) > 0
        if (multiple_operation_args) {
          tf_args <- c(tf_args, self$operation_args)
        }

        # get the tensorflow function and apply it to the args
        operation <- eval(parse(text = self$operation),
          envir = self$tf_function_env
        )
        tensor <- do.call(operation, tf_args)
      }

      # assign it in the environment
      assign(tf_name, tensor, envir = dag$tf_environment)
    }
  )
)

variable_node <- R6Class(
  "variable_node",
  inherit = node,
  public = list(
    constraint = NULL,
    constraint_array = NULL,
    lower = -Inf,
    upper = Inf,
    free_value = NULL,
    initialize = function(lower = -Inf,
                          upper = Inf,
                          dim = NULL,
                          free_dim = prod(dim)) {
      check_if_lower_upper_numeric(lower, upper)

      # replace values of lower and upper with finite values for dimension
      # checking (this is pain, but necessary because check_dims coerces to
      # greta arrays, which must be finite)
      lower_for_dim <- lower
      lower_for_dim[] <- 0
      upper_for_dim <- upper
      upper_for_dim[] <- 0
      dim <- check_dims(lower_for_dim, upper_for_dim, target_dim = dim)

      # vectorise these tests, to get a matrix of constraint types - then test
      # at the end whether it's mixed

      lower_limit <- lower != -Inf
      upper_limit <- upper != Inf

      # create a matrix of elemntwise constraints
      constraint_array <- array(NA, check_dims(lower_for_dim, upper_for_dim))
      constraint_array[!lower_limit & !upper_limit] <- "none"
      constraint_array[!lower_limit & upper_limit] <- "low"
      constraint_array[lower_limit & !upper_limit] <- "high"
      constraint_array[lower_limit & upper_limit] <- "both"

      # pass a string depending on whether they are all the same
      constraint_arrays_are_same <- all(constraint_array == constraint_array[1])
      if (constraint_arrays_are_same) {
        self$constraint <- glue::glue("scalar_all_{constraint_array[1]}")
      } else {
        self$constraint <- "scalar_mixed"
      }

      bad_limits <- switch(self$constraint,
        scalar_all_low = any(!is.finite(upper)),
        scalar_all_high = any(!is.finite(lower)),
        scalar_all_both = any(!is.finite(lower)) | any(!is.finite(upper)),
        FALSE
      )

      check_if_lower_upper_has_bad_limits(bad_limits)

      check_if_upper_gt_lower(lower, upper)

      # add parameters
      super$initialize(dim)
      self$lower <- array(lower, dim)
      self$upper <- array(upper, dim)
      self$constraint_array <- constraint_array
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

      mode <- dag$how_to_define(self)

      if (mode == "sampling") {
        distrib_node <- self$distribution

        if (is.null(distrib_node)) {
          # does it have an anti-representation where it is the cholesky?
            # the antirepresentation of cholesky is chol2symm
            # if yes, we take antirep and get it to `tf`, then get the tf_name
          chol2symm_ga <- self$anti_representations$chol2symm
          chol2symm_existing <- !is.null(chol2symm_ga)

          if (chol2symm_existing) {

            chol2symm_node <- get_node(chol2symm_ga)
            chol2symm_name <- dag$tf_name(chol2symm_node)
            chol2symm_tensor <- get(chol2symm_name, envir = dag$tf_environment)
            tensor <- tf_chol(chol2symm_tensor)

          }

        } else {
          tensor <- dag$draw_sample(self$distribution)
        }
      }

      # if we're defining the forward mode graph, get the free state, transform,
      # and compute any transformation density
      if (mode == "forward") {
        free_name <- glue::glue("{tf_name}_free")
        # create the log jacobian adjustment for the free state
        tf_adj <- self$tf_adjustment(dag)
        adj_name <- glue::glue("{tf_name}_adj")
        assign(adj_name,
          tf_adj,
          envir = dag$tf_environment
        )

        # map from the free to constrained state in a new tensor
        tf_free <- get(free_name, envir = dag$tf_environment)
        tensor <- self$tf_from_free(tf_free)
      }

      # assign to environment variable
      assign(tf_name,
        tensor,
        envir = dag$tf_environment
      )
    },
    create_tf_bijector = function() {
      dim <- self$dim
      lower <- flatten_rowwise(self$lower)
      upper <- flatten_rowwise(self$upper)
      constraints <- flatten_rowwise(self$constraint_array)

      switch(self$constraint,
        scalar_all_none = tf_scalar_bijector(dim),
        scalar_all_low = tf_scalar_neg_bijector(dim, upper = upper),
        scalar_all_high = tf_scalar_pos_bijector(dim, lower = lower),
        scalar_all_both = tf_scalar_neg_pos_bijector(dim,
          lower = lower,
          upper = upper
        ),
        scalar_mixed = tf_scalar_mixed_bijector(dim,
          lower = lower,
          upper = upper,
          constraints = constraints
        ),
        correlation_matrix = tf_correlation_cholesky_bijector(),
        covariance_matrix = tf_covariance_cholesky_bijector(),
        simplex = tf_simplex_bijector(dim),
        ordered = tf_ordered_bijector(dim)
      )
    },
    tf_from_free = function(x) {
      tf_bijector <- self$create_tf_bijector()
      tf_bijector$forward(x)
    },

    # adjustments for univariate variables
    tf_log_jacobian_adjustment = function(free) {
      tf_bijector <- self$create_tf_bijector()

      event_ndims <- as.integer(tf_bijector$forward_min_event_ndims)
      ljd <- tf_bijector$forward_log_det_jacobian(
        x = free,
        event_ndims = as.integer(event_ndims)
      )

      # sum across all dimensions of jacobian
      already_summed <-
        identical(dim(ljd), NA_integer_) | identical(dim(ljd), integer(0))

      if (!already_summed) {
        ljd <- tf_sum(ljd, drop = TRUE)
      }

      # make sure there's something in the batch dimension
      no_batch_dimension <- identical(dim(ljd), integer(0))
      if (no_batch_dimension) {
        ljd <- tf$expand_dims(ljd, 0L)
        tiling <- tf$stack(
          list(tf$shape(free)[0]),
          axis = 0L)
        ljd <- tf$tile(ljd, tiling)
      }

      ljd
    },

    # create a tensor giving the log jacobian adjustment for this variable
    tf_adjustment = function(dag) {

      # find free version of node
      free_tensor_name <- glue::glue("{dag$tf_name(self)}_free")
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
    parameter_shape_matches_output = logical(),
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
    list_parents = function(dag) {
      parents <- self$parents

      # if this node is being used for sampling and has a target, do not
      # consider that a parent node
      mode <- dag$how_to_define(self)
      if (mode == "sampling" & !is.null(self$target)) {
        parent_names <- extract_unique_names(parents)
        keep <- parent_names != self$target$unique_name
        parents <- parents[keep]
      }

      parents
    },
    list_children = function(dag) {
      children <- self$children

      # if this node is being used for sampling and has a target, consider that
      # a child node
      mode <- dag$how_to_define(self)
      if (mode == "sampling" & !is.null(self$target)) {
        children <- c(children, list(self$target))
      }

      children
    },

    # create target node, add as a parent, and give it this distribution
    add_target = function(new_target) {
      # add as target and as a parent
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

      # assign the distribution object constructor function to the environment
      assign(dag$tf_name(self),
        self$tf_distrib,
        envir = dag$tf_environment
      )
    },

    # which node to use as the *tf* target (overwritten by some distributions)
    get_tf_target_node = function() {
      self$target
    },

    # shape_matches_output indicates whether the array for the parameter can
    # have the same shape as the output (e.g. this is true for binomial's prob
    # parameter, but not for size) by default, assume a scalar (row) parameter
    # can be expanded up to the distribution size
    add_parameter = function(parameter,
                             name,
                             shape_matches_output = TRUE,
                             expand_now = TRUE) {

      # record whether this parameter can be scaled up
      self$parameter_shape_matches_output[[name]] <- shape_matches_output

      # try to do it now if required
      if (shape_matches_output & expand_now) {
        parameter <- self$expand_parameter(parameter, self$dim)
      }

      # record it in the right places
      parameter <- to_node(parameter)
      self$add_parent(parameter)
      self$parameters[[name]] <- parameter
    },

    # try to expand a greta array for a parameter up to the required dimension
    expand_parameter = function(parameter, dim) {

      # can this realisation of the parameter be expanded?
      expandable_shape <- ifelse(self$multivariate,
        is_row(parameter),
        is_scalar(parameter)
      )

      # should we expand it now?
      expanded_target <- ifelse(self$multivariate,
        !identical(dim[1], 1L),
        !identical(dim, c(1L, 1L))
      )

      # expand now if needed (and remove flag)
      if (expandable_shape & expanded_target) {
        if (self$multivariate) {
          n_realisations <- self$dim[1]
          reps <- replicate(n_realisations, parameter, simplify = FALSE)
          parameter <- do.call(rbind, reps)
        } else {
          parameter <- greta_array(parameter, dim = self$dim)
        }
      }

      parameter
    },

    # try to expand all expandable (scalar for univariate, or row for
    # multivariate) parameters to the required dimension
    expand_parameters_to = function(dim) {
      parameter_names <- names(self$parameters)

      for (name in parameter_names) {
        if (self$parameter_shape_matches_output[[name]]) {
          parameter <- as.greta_array(self$parameters[[name]])
          expanded <- self$expand_parameter(parameter, dim)

          self$add_parameter(expanded,
            name,
            self$parameter_shape_matches_output[[name]],
            expand_now = FALSE
          )
        }
      }
    }
  )
)

# modules for export via .internals
node_classes_module <- module(
  node,
  distribution_node,
  data_node,
  variable_node,
  operation_node
)


# shorthand for distribution parameter constructors
distrib <- function(distribution, ...) {
  check_tf_version("error")

  # get and initialize the distribution, with a default value node
  constructor <- get(
    x = glue::glue("{distribution}_distribution"),
    envir = parent.frame()
  )
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
  if (is.null(truncation)) {
    truncation <- c(-Inf, Inf)
  }

  truncation <- as.list(truncation)

  variable_node$new(
    lower = truncation[[1]],
    upper = truncation[[2]],
    dim = dim,
    free_dim = free_dim
  )
}

node_constructors_module <- module(
  distrib,
  op,
  vble
)
