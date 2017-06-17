data_node <- R6Class(
  'data_node',
  inherit = node,
  public = list(

    initialize = function (data) {

      # coerce data from common formats to an array here
      data <- as.array(data)

      # coerce 1D arrays to column vectors
      if (length(dim(data)) == 1)
        dim(data) <- c(dim(data), 1)

      # update and store array and store dimension
      super$initialize(dim = dim(data), value = data)

    },

    tf = function (dag) {
      assign(dag$tf_name(self),
             tf$constant(self$value(), dtype = tf_float()),
             envir = dag$tf_environment)
    }
  )
)

# a node for applying operations to values
operation_node <- R6Class(
  'operation_node',
  inherit = node,
  public = list(

    operation_name = NA,
    operation = NA,
    operation_args = NA,
    arguments = list(),

    initialize = function (operation,
                           ...,
                           dimfun = NULL,
                           operation_args = list(),
                           tf_operation = NULL,
                           value = NULL) {

      # coerce all arguments to nodes, and remember the operation
      dots <- lapply(list(...), to_node)
      for(node in dots)
        self$add_argument(node)

      # default to the same name for the op in R as in TF
      if (is.null(tf_operation))
        tf_operation <- paste0('tf$', operation)

      self$operation_name <- operation
      self$operation <- tf_operation
      self$operation_args <- operation_args

      # work out the dimensions of the new node, if NULL assume an elementwise
      # operation and get the largest number of each dimension, otherwise expect
      # a function to be passed which will calculate it from the provided list
      # of nodes arguments
      if (is.null(dimfun))
        dim <- do.call(pmax, lapply(dots, member, 'dim'))
      else
        dim <- dimfun(dots)

      # assign empty value of the right dimension, or the values passed via the
      # operation
      if (is.null(value))
        value <- unknowns(dim = dim)
      else if (!all.equal(dim(value), dim))
        stop ('values have the wrong dimension so cannot be used')

      super$initialize(dim, value)

    },

    add_argument = function (argument) {

      # guess at a name, coerce to a node, and add as a child
      parameter <- to_node(argument)
      self$add_child(parameter)

    },

    switch_op = function (op) {
      # look up the operation in this table to see if there is a more stable
      # name
      op_list <- list("`*`" = 'tf$multiply')

      idx <- match(op, names(op_list))

      # only change if there is a swap to make
      if (!is.na(idx))
        op <- op_list[[idx]]

      op
    },

    tf = function (dag) {

      # switch out the op for non-sugared variety
      op <- self$switch_op(self$operation)

      # get the function
      fun <- eval(parse(text = op))

      # fetch the tensors for the environment
      arg_tf_names <- lapply(self$children, dag$tf_name)
      args <- lapply(arg_tf_names, get, envir = dag$tf_environment)

      # fetch additional (non-tensor) arguments, if any
      if (length(self$operation_args) > 0)
        args <- c(args, self$operation_args)

      # apply function on tensors
      node <- do.call(fun, args)

      # assign it in the environment
      assign(dag$tf_name(self), node, envir = dag$tf_environment)

    }
  )
)

# shorthand to speed up op definitions
op <- function (...) {
  as.greta_array(operation_node$new(...))
}

variable_node <- R6Class (
  'variable_node',
  inherit = node,
  public = list(

    constraint = NULL,
    lower = -Inf,
    upper = Inf,

    initialize = function (lower = -Inf, upper = Inf, dim = 1) {

      good_types <- is.numeric(lower) && length(lower) == 1 &
        is.numeric(upper) && length(upper) == 1

      if (!good_types) {

        stop ('lower and upper must be numeric vectors of length 1',
              call. = FALSE)

      }

      # check and assign limits
      bad_limits <- TRUE

      # find constraint type
      if (lower == -Inf & upper == Inf) {

        self$constraint <- 'none'
        bad_limits <- FALSE

      } else if (lower == -Inf & upper != Inf) {

        self$constraint <- 'low'
        bad_limits <- !is.finite(upper)

      } else if (lower != -Inf & upper == Inf) {

        self$constraint <- 'high'
        bad_limits <- !is.finite(lower)

      } else if (lower != -Inf & upper != Inf) {

        self$constraint <- 'both'
        bad_limits <- !is.finite(lower) | !is.finite(upper)

      }

      if (bad_limits) {

        stop ('lower and upper must either be -Inf (lower only), ',
              'Inf (upper only) or finite scalars',
              call. = FALSE)

      }

      if (lower >= upper) {

        stop ('upper bound must be greater than lower bound',
              call. = FALSE)

      }

      # add parameters
      super$initialize(dim)
      self$lower <- lower
      self$upper <- upper

    },

    tf = function (dag) {

      # make a Variable tensor to hold the free state
      tf_obj <- tf$Variable(initial_value = self$value(),
                            dtype = tf_float())

      # assign this as the free state
      tf_name <- dag$tf_name(self)
      free_name <- sprintf('%s_free', tf_name)
      assign(free_name,
             tf_obj,
             envir = dag$tf_environment)

      # get the log jacobian adjustment for the free state
      tf_adj <- self$tf_adjustment(dag)
      adj_name <- sprintf('%s_adj', tf_name)
      assign(adj_name,
             tf_adj,
             envir = dag$tf_environment)

      # map from the free to constrained state in a new tensor
      tf_free <- get(free_name, envir = dag$tf_environment)
      node <- self$tf_from_free(tf_free, dag$tf_environment)
      assign(tf_name,
             node,
             envir = dag$tf_environment)

    },

    tf_from_free = function (x, env) {

      upper <- self$upper
      lower <- self$lower

      if (self$constraint == 'none') {

        y <- x

      } else if (self$constraint == 'both') {

        y <- tf_ilogit(x) * fl(upper - lower) + fl(lower)

      } else if (self$constraint == 'low') {

        y <- fl(upper) - tf$exp(x)

      } else if (self$constraint == 'high') {

        y <- tf$exp(x) + fl(lower)

      } else {

        y <- x

      }

      y

    },

    # adjustments for univariate variables
    tf_log_jacobian_adjustment = function (free) {

      ljac_none <- function (x)
        fl(0)

      ljac_exp <- function (x)
        tf$reduce_sum(x)

      ljac_logistic <- function (x) {
        lrange <- log(self$upper - self$lower)
        tf$reduce_sum(x - fl(2) * tf_log1pe(x) + lrange)
      }

      ljac_corr_mat <- function (x) {

        # find dimension
        n <- x$get_shape()$as_list()[1]
        K <- (1 + sqrt(8 * n + 1)) / 2

        # draw the rest of the owl
        l1mz2 <- tf$log(1 - tf$square(tf$tanh(x)))
        i <- rep(1:(K - 1), (K - 1) : 1)
        a <- fl(K - i - 1) * l1mz2
        fl(0.5) * tf$reduce_sum(a) + tf$reduce_sum(l1mz2)

      }

      ljac_cov_mat <- function (x) {

        # find dimension
        n <- x$get_shape()$as_list()[1]
        K <- (sqrt(8 * n + 1) - 1) / 2

        k <- seq_len(K)
        fl(K * log(2)) + tf$reduce_sum(fl(K - k + 2) * x[k - 1])

      }

      fun <- switch (self$constraint,
                     none = ljac_none,
                     high = ljac_exp,
                     low = ljac_exp,
                     both = ljac_logistic,
                     correlation_matrix = ljac_corr_mat,
                     covariance_matrix = ljac_cov_mat)

      fun(free)

    },

    # create a tensor giving the log jacobian adjustment for this variable
    tf_adjustment = function (dag) {

      # find free version of node
      free_tensor_name <- paste0(dag$tf_name(self), '_free')
      free_tensor <- get(free_tensor_name, envir = dag$tf_environment)

      # apply jacobian adjustment to it
      self$tf_log_jacobian_adjustment(free_tensor)

    }

  )
)

# helper function to create a variable node
# by default, make x (the node
# containing the value) a free parameter of the correct dimension
vble = function(...)
  variable_node$new(...)

distribution_node <- R6Class (
  'distribution_node',
  inherit = node,
  public = list(
    distribution_name = 'no distribution',
    discrete = NA,
    target = NULL,
    user_node = NULL,
    truncation = NULL,
    parameters = list(),

    initialize = function (name = 'no distribution', dim = NULL, discrete = FALSE) {

      super$initialize(dim)

      # for all distributions, set name, store dims and set whether discrete
      self$distribution_name <- name
      self$discrete <- discrete

      # initialize the target values of this distribution
      self$add_target(self$create_target())

      # set the target as the user node (user-facing representation) by default
      self$user_node <- self$target

    },

    # create target node, add as a child, and give it this distribution
    add_target = function (new_target) {

      # add as x and as a child
      self$target <- new_target
      self$add_child(new_target)

      # get its values
      self$value(new_target$value())

      # give self to x as its distribution
      self$target$set_distribution(self)

    },

    # replace the existing target node with a new one
    replace_target = function (new_target) {

      # remove x from children
      self$remove_child(self$target)

      # add the new one in
      self$add_target(new_target)

    },

    tf = function (dag) {

      # define a tensor with this node's log density in env

      # run the TF version of the density function
      tf_obj <- self$tf_log_density(dag)

      # assign the result back to env
      assign(dag$tf_name(self),
             tf_obj,
             envir = dag$tf_environment)

    },

    # which node to use af the *tf* target (overwritten by some distributions)
    get_tf_target_node = function () {
      self$target
    },

    tf_log_density = function (dag) {

      # fetch inputs
      tf_target_node <- self$get_tf_target_node()
      tf_target <- get(dag$tf_name(tf_target_node),
                       envir = dag$tf_environment)
      tf_parameters <- self$tf_fetch_parameters(dag)

      # calculate log density
      ld <- self$tf_log_density_function(tf_target, tf_parameters)

      # check for truncation
      if (!is.null(self$truncation))
        ld <- ld - self$tf_log_density_offset(tf_parameters)

      ld
    },

    tf_fetch_parameters = function (dag) {
      # fetch the tensors corresponding to this node's parameters from the
      # environment, and return them in a named list

      # find names
      tf_names <- lapply(self$parameters, dag$tf_name)

      # fetch tensors
      lapply(tf_names, get, envir = dag$tf_environment)

    },

    tf_log_density_offset = function (parameters) {

      # calculate the log-adjustment to the truncation term of the density
      # function i.e. the density of a distribution, truncated between a and b,
      # is the non truncated density, divided by the integral of the density
      # function between the truncation bounds. This can be calculated from the
      # distribution's CDF

      lower <- self$truncation[1]
      upper <- self$truncation[2]

      if (lower == -Inf) {

        # if only upper is constrained, just need the cdf at the upper
        offset <- self$tf_log_cdf_function(fl(upper), parameters)

      } else if (upper == Inf) {

        # if only lower is constrained, get the log of the integral above it
        offset <- tf$log(fl(1) - self$tf_cdf_function(fl(lower), parameters))

      } else {

        # if both are constrained, get the log of the integral between them
        offset <- tf$log(self$tf_cdf_function(fl(upper), parameters) -
                           self$tf_cdf_function(fl(lower), parameters))

      }

      offset

    },

    add_parameter = function (parameter, name) {

      parameter <- to_node(parameter)
      self$add_child(parameter)
      self$parameters[[name]] <- parameter

    },

    tf_log_density_function = function (x, parameters) {

      self$tf_distrib(parameters)$log_prob(x)

    },

    tf_cdf_function = function (x, parameters) {

      self$tf_distrib(parameters)$cdf(x)

    },

    tf_log_cdf_function = function (x, parameters) {

      self$tf_distrib(parameters)$log_cdf(x)

    }

  )
)

