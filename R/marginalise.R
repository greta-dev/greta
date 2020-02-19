# marginalising random variables

#' @name marginalisation
#'
#' @title direct marginalisation of random variables
#' @description Inference on many statistical models requires marginalisation of
#'   (ie. integration of functions over) random variables. In addition to
#'   general purpose marginalisation methods like MCMC that act on an entire
#'   model, it is often useful to directly marginalise a single random variable
#'   within a larger model; e.g. for random variables where marginalisation by
#'   MCMC is inefficient, impossible, or when an approximate marginalisation is
#'   substantially more efficient. \code{marginalise()} performs direct
#'   marginalisation within a greta model, using one of the available
#'   marginalisation methods, detailed in \link{marginalisers}.
#'
#' @examples
#' \dontrun{
#' # marginalise a discrete random variable and carry out HMC on the
#' # continouous variables:
#' y <- rnorm(100)
#'
#' lambda <- lognormal(0, 1)
#' theta <- normal(0, 1)
#' sd <- lognormal(0, 1)
#'
#' # if discrete variables could be sampled by MCMC in greta,
#' # we would be able to simply do:
#' #   n <- poisson(lambda)
#' #   mu <- theta ^ n
#' #   distribution(y) <- normal(mu, sd)
#'
#' # the function to marginalise over - the variable to marginalise
#' # must be first, others must be passed to marginalise()
#' likelihood <- function (n, theta, sd) {
#'   mu <- theta ^ n
#'   distribution(y) <- normal(mu, sd)
#' }
#'
#' # integrate the function values w.r.t. the poisson distribution
#' marginalise(fun = likelihood,
#'             variable = poisson(lambda),
#'             method = discrete_marginalisation(values = 0:10),
#'             theta = theta,
#'             sd = sd)
#'
#' m <- model(lambda)
#' draws <- mcmc(m, hmc())
#' }
NULL

#' @rdname marginalisation
#' @export
#'
#' @param fun an R function to integrate with respect to the random variable.
#'   The first argument must be a greta array for the random variable, any
#'   subsequent arguments must passed in via the \dots argument.
#' @param variable a variable greta array with a distribution, representing the
#'   random variable to marginalise
#' @param method a \code{marginaliser} object giving the method for carrying out
#'   the marginalisation
#' @param \dots named greta arrays to be passed to \code{fun}
#'
#' @details The code in \code{fun} must define at least one distribution over
#'   data (ie. a model likelihood), and cannot create any new variables. Any
#'   variables must be created outide this function, and passed in via the \dots
#'   argument.
marginalise <- function(fun, variable, method, ...) {

  # get the distribution for the random variable
  distribution_node <- get_node(variable)$distribution

  # check the inputs
  if (!is.function(fun)) {
    stop("'fun' must be an R function")
  }

  # check the additional arguments to fun are given in dots
  dots <- list(...)
  dot_names <- names(dots)
  expected_names <- names(formals(fun)[-1])
  if (!all(expected_names %in% dot_names)) {
    stop("all arguments to 'fun' must be passed, named, to marginalise")
  }

  if (!inherits(distribution_node, "distribution_node")) {
    stop("'variable' must be a variable greta array with a distribution")
  }

  if (!inherits(method, "marginaliser")) {
    stop("'method' must be a valid marginalisation method. ",
         "See ?marginalise for options")
  }

  # check the distribution is compatible with the method
  method$distribution_check(distribution_node)

  # excise the variable from the distribution
  distribution_node$remove_target()

  # turn the greta function into a TF conditional density function; doing
  # something very similar to as_tf_function(), but giving a function that
  # returns a tensorflow scalar for the density (unadjusted since there will be
  # no new variables)
  conditional_joint_density <- as_conditional_density(fun, c(list(variable), dots))

  # get a list of greta arrays for the marginalisation parameters:
  parameters <- method$compute_parameters(
    conditional_density_fun = conditional_joint_density,
    distribution_node = distribution_node,
    dots = dots,
    marginaliser = method
  )

  # add on any pre-computed parameters
  parameters <- c(parameters, method$other_parameters)

  # create the distribution
  distribution <- distrib(
    distribution = "marginalisation",
    marginaliser = method,
    parameters = parameters,
    conditional_density_fun = conditional_joint_density,
    distribution_node = distribution_node,
    dots = dots
  )

  # return a list of the greta arrays computed during the marginalisation, and any other
  # things from the method
  output <- method$return_list(parameters)
  invisible(output)

}

marginalisation_distribution <- R6Class(
  "marginalisation_distribution",
  inherit = distribution_node,
  public = list(

    marginaliser = NULL,
    tf_marginalisation_density = NULL,
    conditional_density_fun = NULL,

    initialize = function(marginaliser,
                          parameters = parameters,
                          conditional_density_fun,
                          distribution_node,
                          dots) {

      # initialize class, and add methods
      super$initialize("marginalisation")
      self$marginaliser <- marginaliser
      self$conditional_density_fun <- conditional_density_fun

      # add the parameters
      parameter_names <- names(parameters)
      for (i in seq_along(parameters)) {
        self$add_parameter(parameters[[i]],
                           parameter_names[i])
      }

      # add the dots (extra inputs to conditional_density_fun) as parameters
      dot_nodes <- lapply(dots, get_node)
      for (i in seq_along(dot_nodes)) {
        self$add_parameter(dot_nodes[[i]],
                           paste("input", i))
      }

      # make the distribution the target
      self$remove_target()
      self$add_target(distribution_node)

    },

    tf_distrib = function(parameters, dag) {

      # unpack the parameters here
      are_dots <- grepl("^input ", names(parameters))
      dots <- parameters[are_dots]
      parameters <- parameters[!are_dots]

      # the marginal density implied by the function
      log_prob <- function(x) {

        marginal_density <- self$marginaliser$tf_marginalisation_density
        marginal_density(parameters = parameters,
                         tf_conditional_density_fun = self$conditional_density_fun,
                         dots = dots,
                         other_args = self$marginaliser$other_args)

      }


      list(log_prob = log_prob)

    }

  )
)

# turn the greta function into a TF conditional density function; doing
# something very similar to as_tf_function(), but giving a function that
# returns a tensorflow scalar for the density
as_conditional_density <- function(r_fun, args) {

  # run the operation on isolated greta arrays, so nothing gets attached to the
  # model. Real greta arrays in args
  ga_dummies <- lapply(args, dummy_greta_array)
  out <- do.call(r_fun, ga_dummies)

  # we don't want to rely on the function returning a greta array that depends
  # on everything internally. instead, we want to grab any densities defined in
  # the function.

  # this function will take in tensors corresponding to the things in args
  function(..., reduce = TRUE) {

    tensor_inputs <- list(...)

    # if any of these are shapeless, make them into greta scalars (3D)
    tensor_inputs <- lapply(tensor_inputs,
                            function(x) {
                              if (identical(dim(x), list()))
                                x <- tf$reshape(x, shape(1, 1, 1))
                              x
                            })

    # transfer batch dimensions if needed
    tensor_inputs <- match_batches(tensor_inputs)

    # create a sub-dag for everything connected to the inputs
    sub_dag <- dag_class$new(ga_dummies, tf_float = options()$greta_tf_float)

    # check there are distributions
    if (!any(sub_dag$node_types == "distribution")) {
      stop("'fun' must constain at least one distribution over data",
           call. = FALSE)
    }

    # check there are no variables
    if (any(sub_dag$node_types == "variable")) {
      stop("'fun' must not create any new variables, ",
           "variables can be passed in as arguments",
           call. = FALSE)
    }

    # use the default graph, so that it can be overwritten when this is called
    # (using on_graph())
    sub_dag$tf_graph <- tf$get_default_graph()
    sub_tfe <- sub_dag$tf_environment

    # pass on the batch size, used when defining data
    sub_tfe$batch_size <- get_batch_size()

    # set the input tensors as the values for the dummy greta arrays in the new
    # tf_environment
    node_dummies <- lapply(ga_dummies, get_node)
    tf_names <- lapply(node_dummies, sub_dag$tf_name)
    for (i in seq_along(tf_names))
      assign(tf_names[[i]], tensor_inputs[[i]], envir = sub_tfe)

    # have any new data defined as constants, to avoid placeholding issues in
    # the wider model
    greta_stash$data_as_constants <- TRUE
    on.exit(greta_stash$data_as_constants <- NULL)

    # define all nodes (only data, operation and distribution) in the
    # environment, and on the graph
    sub_dag$on_graph(lapply(sub_dag$node_list,
                            function(x) x$define_tf(sub_dag)))

    # define and return the joint density tensor (no variables, so no need for
    # log jacobian adjustment)
    sub_dag$define_joint_density(adjusted = FALSE)

    # whether to reduce_sum the densities, or sum them elementwise
    if (reduce) {
      result <- sub_tfe$joint_density
    } else {
      densities <- sub_tfe$component_densities
      if (length(densities) > 1) {
        names(densities) <- NULL
        result <- tf$add_n(densities)
      } else {
        result <- densities[[1]]
      }
    }

    result

  }

}
