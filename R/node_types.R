# different types of node

deterministic_node <- R6Class (
  'deterministic_node',
  inherit = node,
  public = list(

    type = 'deterministic',

    # deterministic nodes add nothing to the density, only to the value
    log_density_function = function (val) 0

  )
)

# a node for constant values
constant_node <- R6Class(
  'constant_node',
  inherit = deterministic_node,
  public = list(

    type = 'constant',

    # is x is a numeric scalar, accept it
    initialize = function (x) {


      if (!(is.numeric(x) && is.vector(x) && length(x) == 1))
        stop ('object cannot be coerced to a node')

      self$value(x)
      self$register()

    },

    tf = function (env) {
      assign(self$name,
             tf$constant(self$value(), dtype = tf$float32),
             envir = env)
    }
  )
)


data_node <- R6Class(
  'data_node',
  inherit = constant_node,
  public = list(

    type = 'data',
    likelihood = NA,

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

    set_likelihood = function (distribution) {

      # check it
      if (!inherits(distribution, 'distribution'))
        stop ('invalid distribution')

      # register it
      self$add_child(distribution)

      # add it
      self$likelihood <- distribution

    },

    tf = function (env) {
      assign(self$name,
             tf$constant(self$value(), dtype = tf$float32),
             envir = env)
    }
  )
)

#' @name observed
#' @title define observed data
#' @description define an object in an R session as data in a greta model
#' @param data an object that can be coerced to an array
#' @export
observed <- function (data)
  data_node$new(data)


# a node for applying operations to values
operation_node <- R6Class(
  'operation_node',
  inherit = deterministic_node,
  public = list(

    type = 'operation',
    .operation = NA,
    arguments = list(),

    add_argument = function (argument) {

      # guess at a name, coerce to a node, and add as a child
      parameter <- self$as_node(argument)
      self$add_child(parameter)

    },

    initialize = function (operation, ..., dimfun = NULL) {

      # coerce all arguments to nodes, and remember the operation
      dots <- lapply(list(...), to_node)
      for(node in dots)
        self$add_argument(node)

      self$.operation <- operation

      # work out the dimensions of the new node, if NULL assume an elementwise
      # operation and get the largest number of each dimension, otherwise expect
      # a function to be passed which will calculate it from the provided list
      # of nodes arguments
      if (is.null(dimfun))
        dim <- do.call(pmax, lapply(dots, member, 'dim'))
      else
        dim <- dimfun(dots)

      # assign empty value of the right dimension
      self$value(array(NA, dim = dim))
      self$dim <- dim
      self$register()

    },

    switch_op = function (op) {
      # look up the operation in this table to see if there is a more stable
      # name
      op_list <- list("`*`" = 'tf$mul')

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

      # apply function on tensors
      node <- do.call(fun, args)

      # assign it in the environment
      assign(self$name, node, envir = env)

    }
  )
)

# wrapper to parse inputs before R6 mangles them, & shorthand to speed up the
# rest of the definitions
op <- operation_node$new


stochastic_node <- R6Class (
  inherit = node,
  'stochastic_node',
  public = list(

    type = 'stochastic',

    tf_define = function (env) {

      # if it's an observed stochastic, make it a constant and assign
      if (self$.fixed_value) {

        tf_obj <- tf$constant(self$value(), shape = to_shape(self$dim), dtype = tf$float32)

        assign(self$name,
               tf_obj,
               envir = env)

      } else {

        # otherwise, make a Variable tensor to hold the free state
        tf_obj <- tf$Variable(initial_value = self$value(), dtype = tf$float32)

        # assign this as the free state
        free_name <- sprintf('%s_free',
                             self$name)
        assign(free_name,
               tf_obj,
               envir = env)

        # map from the free to constrained state in a new tensor

        # fetch the free node
        free_node <- get(free_name, envir = env)

        # appy transformation
        node <- self$tf_from_free(free_node, env)

        # assign back to environment with base name (density will use this)
        assign(self$name,
               node,
               envir = env)

      }

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

    tf_fetch_parameters = function (env) {
      # fetch the tensors corresponding to this node's parameters from the
      # environment, and return them in a named list

      # find names
      names <- lapply(self$parameters, member, 'name')

      # fetch tensors
      lapply(names, function (x) get(x, envir = env))

    },

    # fetch tensor corresponding to this node from the environment
    tf_fetch_self = function (env)
      get(self$name, envir = env),

    tf = function (env) {

      # define self as a tensor
      self$tf_define(env)
      self$tf_define_density(env)

    },

    tf_log_density = function (env) {

      # fetch inputs
      value <- self$tf_fetch_self(env)
      parameters <- self$tf_fetch_parameters(env)

      # calculate log density
      self$tf_log_density_function(value, parameters)

    },

    tf_from_free = function (free_node, env) notimplemented()

  )
)

# define base distribution constructor classes
distribution <- R6Class (
  'distribution',
  inherit = stochastic_node,
  public = list(

    type = 'stochastic',
    discrete = NA,
    parameters = list(),
    distribution_name = 'no distribution',
    from_free = function (x) notimplemented(),
    to_free = function (x) notimplemented(),

    initialize = function (name = 'no distribution', dim = 1, discrete = FALSE) {

      # for all distributions, set name, store dims and set whether discrete
      self$distribution_name <- name
      self$discrete <- discrete

      # coerce dim to integer
      dim <- as.integer(dim)

      # store array (updates dim)
      self$value(array(0, dim = dim))
      self$register()

    },

    add_parameter = function (parameter, name) {

      # coerce to a node, add as a child and register as a parameter

      # just add as a scalar numeric (not a constant node) here.
      # ensure that the value can be fetched
      parameter <- self$as_node(parameter)
      self$add_child(parameter)
      self$parameters[[name]] <- parameter

    },

    print = function() {
      msg <- sprintf('%s distribution',
                     self$distribution_name)
      cat(msg)
    }
  )
)
