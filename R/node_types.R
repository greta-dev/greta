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
      super$initialize(dim = dim(data), value = data)

    },

    tf = function (dag) {
      assign(dag$tf_name(self),
             tf$constant(self$value(), dtype = tf$float32),
             envir = dag$tf_environment)
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
      op <- self$switch_op(self$.operation)

      # get the function
      fun <- eval(parse(text = op))

      # fetch the tensors for the environment
      arg_tf_names <- lapply(self$children, dag$tf_name)
      args <- lapply(arg_tf_names, get, envir = dag$tf_environment)

      # fetch additional (non-tensor) arguments, if any
      if (length(self$.operation_args) > 0)
        args <- c(args, self$.operation_args)

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

    tf = function (dag) {

      # make a Variable tensor to hold the free state
      tf_obj <- tf$Variable(initial_value = self$value(),
                            dtype = tf$float32)

      # assign this as the free state
      tf_name <- dag$tf_name(self)

      free_name <- sprintf('%s_free', tf_name)
      assign(free_name,
             tf_obj,
             envir = dag$tf_environment)

      # map from the free to constrained state in a new tensor

      # fetch the free node
      tf_free <- get(free_name, envir = dag$tf_environment)

      # appy transformation
      node <- self$tf_from_free(tf_free, dag$tf_environment)

      # assign back to environment with base name (density will use this)
      assign(tf_name,
             node,
             envir = dag$tf_environment)

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
  inherit = node,
  public = list(
    type = 'distribution',
    distribution_name = 'no distribution',
    discrete = NA,
    target = NULL,
    truncation = NULL,
    parameters = list(),

    initialize = function (name = 'no distribution', dim = NULL, discrete = FALSE) {

      super$initialize(dim)

      # for all distributions, set name, store dims and set whether discrete
      self$distribution_name <- name
      self$discrete <- discrete

      # initialize the target values of this distribution
      self$add_target(self$create_target())

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

    tf_log_density = function (dag) {

      # fetch inputs

      tf_target <- get(dag$tf_name(self$target),
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

    add_parameter = function (parameter, name) {

      # coerce to a node, add as a child and register as a parameter

      # just add as a scalar numeric (not a constant node) here.
      # ensure that the value can be fetched
      parameter <- to_node(parameter)
      self$add_child(parameter)
      self$parameters[[name]] <- parameter

    }

  ))

