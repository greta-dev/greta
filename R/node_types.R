deterministic_node <- R6Class(
  'deterministic_node',
  inherit = node,
  public = list(

    type = 'deterministic'

  )
)


# different types of node

data_node <- R6Class(
  'data_node',
  inherit = deterministic_node,
  public = list(

    type = 'data',

    initialize = function (data) {

      # coerce data from common formats to an array here
      data <- as.array(data)

      # coerce 1D arrays to column vectors
      if (length(dim(data)) == 1)
        dim(data) <- c(dim(data), 1)

      # update and store array and store dimension
      self$value(data)
      self$register()

    },

    tf = function (env) {
      assign(self$name,
             tf$constant(self$value(), dtype = tf$float32),
             envir = env)
    }
  )
)

# a node for applying operations to values
operation_node <- R6Class(
  'operation_node',
  inherit = deterministic_node,
  public = list(

    type = 'operation',
    .operation = NA,
    .operation_args = NA,
    arguments = list(),

    add_argument = function (argument) {

      # guess at a name, coerce to a node, and add as a child
      parameter <- to_node(argument)
      self$add_child(parameter)

    },

    initialize = function (operation,
                           ...,
                           dimfun = NULL,
                           operation_args = list(),
                           value = NULL) {

      # coerce all arguments to nodes, and remember the operation
      dots <- lapply(list(...), to_node)
      for(node in dots)
        self$add_argument(node)

      self$.operation <- operation
      self$.operation_args <- operation_args

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

      self$value(value)
      self$dim <- dim
      self$register()

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

    tf = function (env) {

      # switch out the op for non-sugared variety
      op <- self$switch_op(self$.operation)

      # get the function
      fun <- eval(parse(text = op))

      # fetch the tensors for the environment
      arg_names <- self$child_names(recursive = FALSE)
      args <- lapply(arg_names, function (x) get(x, envir = env))

      # fetch additional (non-tensor) arguments, if any
      if (length(self$.operation_args) > 0)
        args <- c(args, self$.operation_args)

      # apply function on tensors
      node <- do.call(fun, args)

      # assign it in the environment
      assign(self$name, node, envir = env)

    }
  )
)

# shorthand to speed up op definitions
op <- function (...) {
  ga(operation_node$new(...))
}

stochastic_node <- R6Class (
  'stochastic_node',
  inherit = node,
  public = list(

    type = 'stochastic',
    parameters = list(),
    tf_from_free = function (x, env) x,
    to_free = function (x) x,

    initialize = function (dim = NULL) {

      if (is.null(dim))
        dim <- c(1, 1)

      # coerce dim to integer
      dim <- as.integer(dim)

      # store array (updates dim)
      self$value(unknowns(dim = dim))
      self$register()

    },

    tf = function (env) {

      # define self as a tensor
      self$tf_define(env)

    },


    tf_define = function (env) {

      # if it's an observed stochastic, make it a constant and assign
      if (self$.fixed_value) {

        tf_obj <- tf$constant(self$value(),
                              shape = to_shape(self$dim),
                              dtype = tf$float32)

        assign(self$name,
               tf_obj,
               envir = env)

      } else {

        # otherwise, make a Variable tensor to hold the free state
        obj <- self$to_free(self$value())
        tf_obj <- tf$Variable(initial_value = obj, dtype = tf$float32)

        # assign this as the free state
        free_name <- sprintf('%s_free',
                             self$name)
        assign(free_name,
               tf_obj,
               envir = env)

        # map from the free to constrained state in a new tensor

        # fetch the free node
        tf_free <- get(free_name, envir = env)

        # appy transformation
        node <- self$tf_from_free(tf_free, env)

        # assign back to environment with base name (density will use this)
        assign(self$name,
               node,
               envir = env)

      }

    },

    # overwrite value with option to switch to free state
    value = function(new_value = NULL, free = FALSE, ...) {

      if (is.null(new_value)) {
        ans <- super$value(new_value, ...)
        if (free)
          ans <- self$to_free(ans)

        return (ans)

      } else {

        super$value(new_value, ...)

      }

    }

  )
)

variable_node <- R6Class (
  'variable_node',
  inherit = stochastic_node,
  public = list(

    type = 'variable',
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

    to_free = function (y) {

      upper <- self$upper
      lower <- self$lower

      if (is_scalar(upper))
        upper <- as.vector(upper)

      if (is_scalar(lower))
        lower <- as.vector(lower)

      if (self$constraint == 'none') {

        x <- y

      } else if (self$constraint == 'both') {

        x <- qlogis((y - lower) / (upper - lower))

      } else if (self$constraint == 'low') {

        baseline <- upper - y
        x <- log(exp(baseline) - 1)

      } else if (self$constraint == 'high') {

        baseline <- y - lower
        x <- log(exp(baseline) - 1)

      }

      x

    },

    tf_from_free = function (x, env) {

      if (self$constraint == 'none') {

        y <- x

      } else if (self$constraint == 'both') {

        upper <- self$upper
        lower <- self$lower
        y <- (1 / (1 + tf$exp(-1 * x))) * (upper - lower) + lower

      } else if (self$constraint == 'low') {

        upper <- self$upper
        baseline <- tf$log(1 + tf$exp(x))
        # have to coerce upper since it's being subtracted *from* and has type
        # 'float32_ref'
        y <- tf_as_float(upper) - baseline

      } else if (self$constraint == 'high') {

        lower <- self$lower
        baseline <- tf$log(1 + tf$exp(x))
        y <- baseline + lower

      }

      y

    }

  )
)

# helper function to create a variable node
# by default, make x (the node containing the value) a free parameter of the correct dimension
variable = function(...)
  variable_node$new(...)

distribution_node <- R6Class (
  'distribution_node',
  inherit = stochastic_node,
  public = list(
    type = 'distribution',
    distribution_name = 'no distribution',
    discrete = NA,
    x = NULL,
    truncation = NULL,

    initialize = function (name = 'no distribution', dim = NULL, discrete = FALSE) {

      super$initialize(dim)

      # for all distributions, set name, store dims and set whether discrete
      self$distribution_name <- name
      self$discrete <- discrete

      # initialize x (the target values of this distribution)
      self$add_x(self$create_x())

    },

    # create x, add as a child, and give it this distribution
    add_x = function (new_x) {

      # add as x and as a child
      self$x <- new_x
      self$add_child(new_x)

      # get its values
      self$value(new_x$value())

      # give self to x as its distribution
      self$x$set_distribution(self)

    },

    # replace the existing x with a new one, including updating the parameter and possibly fixing the value
    replace_x = function (new_x) {

      # remove x from children
      self$remove_child(self$x)

      # add the new one in
      self$add_x(new_x)

    },

    tf = function (env) {

      # define density as a tensor
      self$tf_define_density(env)

    },

    tf_define_density = function (env) {

      # define a tensor with this node's log density in env

      # run the TF version of the density function
      tf_obj <- self$tf_log_density(env)

      # assign the result back to env
      density_name <- sprintf('%s_density',
                              self$name)
      assign(density_name,
             tf_obj,
             envir = env)

    },

    tf_log_density = function (env) {

      # fetch inputs
      tf_x <- get(self$x$name, envir = env)
      tf_parameters <- self$tf_fetch_parameters(env)

      # calculate log density
      ld <- self$tf_log_density_function(tf_x, tf_parameters)

      # check for truncation
      if (!is.null(self$truncation))
        ld <- ld - self$tf_log_density_offset(tf_parameters)

      ld
    },

    tf_fetch_parameters = function (env) {
      # fetch the tensors corresponding to this node's parameters from the
      # environment, and return them in a named list

      # find names
      names <- lapply(self$parameters, member, 'name')

      # fetch tensors
      lapply(names, function (x) get(x, envir = env))

    },

    tf_log_density_offset = function (parameters) {

      # calculate the log-adjustment to the truncation term of the density
      # function i.e. the density of a distribution, truncated between a and b,
      # is the non truncated density, divided by the integral of the density
      # function between the truncation bounds. This can be calculated from the
      # distribution's CDF

      if (is.null(self$tf_cdf_function)) {

        stop('distribution cannot be truncated',
             call. = FALSE)

      }

      lower <- self$truncation[1]
      upper <- self$truncation[2]

      if (lower == -Inf && upper == Inf) {

        # if neither constrained, offset is 0
        offset <- 0

      } else if (lower == -Inf) {

        # if only upper is constrained, just need the cdf at the upper
        offset <- self$tf_log_cdf_function(upper, parameters)

      } else if (upper == Inf) {

        # if only lower is constrained, get the log of the integral above it
        offset <- tf$log(1 - self$tf_cdf_function(lower, parameters))

      } else {

        # if both are constrained, get the log of the integral between them
        offset <- tf$log(self$tf_cdf_function(upper, parameters) -
                           self$tf_cdf_function(lower, parameters))

      }

      offset

    },

    # default version of the log truncation function, should be overloaded with
    # a more efficient one
    tf_log_cdf_function = function (quantile, parameters) {
      tf$log(self$tf_cdf_function(quantile, parameters))
    },

    add_parameter = function (parameter, name) {

      # coerce to a node, add as a child and register as a parameter

      # just add as a scalar numeric (not a constant node) here.
      # ensure that the value can be fetched
      parameter <- to_node(parameter)
      self$add_child(parameter)
      self$parameters[[name]] <- parameter

    }

  ))

