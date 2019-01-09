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
  distr <- get_node(variable)$distribution

  # check the inputs
  if (!is.function(fun)) {
    stop ("'fun' must be an R function")
  }

  # check the additional arguments to fun are given in dots
  dots <- list(...)
  dot_names <- names(dots)
  expected_names <- names(formals(fun)[-1])
  if (!all(expected_names %in% dot_names)) {
    stop ("all arguments to 'fun' must be passed, named, to marginalise")
  }

  if (!inherits(distr, "distribution_node")) {
    stop ("'variable' must be a variable greta array with a distribution")
  }

  if (!inherits(method, "marginaliser")) {
    stop ("'method' must be a valid marginalisation method. ",
          "See ?marginalise for options")
  }

  # check the distribution is compatible with the method
  method$distribution_check(distr)

  # excise the variable from the distribution
  distr$remove_target()

  # turn the greta function into a TF conditional density function; doing
  # something very similar to as_tf_function(), but giving a function that
  # returns a tensorflow scalar for the density (unadjusted since there will be
  # no new variables)
  args <- c(list(variable), dots)
  conditional_joint_density <- as_conditional_density(fun, args)

  # get a tf function from the method which will turn that conditional density
  # function into a marginal density (conditional on the inputs to the
  # distribution) to be added to the overall model density

  # pass the conditional density function and the marginaliser to a
  # marginalisation distribution

  # create the distribution
  vble <- distrib(
    distribution = "marginalisation",
    marginaliser = method,
    conditional_density_fun = conditional_joint_density,
    target_distribution = distr,
    dots = dots
  )

  # return nothing (users can't have distribution nodes on their own)
  invisible(NULL)

}

marginalisation_distribution <- R6Class(
  "marginalisation_distribution",
  inherit = distribution_node,
  public = list(

    tf_marginaliser = NULL,
    conditional_density_fun = NULL,

    initialize = function(marginaliser,
                          conditional_density_fun,
                          target_distribution,
                          dots) {

      # initialize class, and add methods
      super$initialize("marginalisation")
      self$tf_marginaliser <- marginaliser$tf_marginaliser
      self$conditional_density_fun <- conditional_density_fun

      # add the dots (extra inputs to conditional_density_fun) as parameters
      dot_nodes <- lapply(dots, get_node)
      for (i in seq_along(dot_nodes)) {
        self$add_parameter(dot_nodes[[i]],
                           paste("input", i))
      }

      # make the distribution the target
      self$remove_target()
      self$add_target(target_distribution)

    },

    tf_distrib = function(parameters, dag) {

      # the marginal density implied by the function
      log_prob <- function(x) {

        # x will be the target; a tf function for the distribution being
        # marginalised
        self$tf_marginaliser(self$conditional_density_fun,
                             tf_distribution_log_pdf = x,
                             parameters)

      }

      list(log_prob = log_prob, cdf = NULL, log_cdf = NULL)

    },

    tf_cdf_function = NULL,
    tf_log_cdf_function = NULL

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
  function(...) {

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
      stop ("'fun' must constain at least one distribution over data",
            call. = FALSE)
    }

    # check there are no variables
    if (any(sub_dag$node_types == "variable")) {
      stop ("'fun' must not create any new variables, ",
            "variables can be passed in as arguments",
            call. = FALSE)
    }

    # use the default graph, so that it can be overwritten when this is called
    # (using on_graph())
    sub_dag$tf_graph <- tf$get_default_graph()
    sub_tfe <- sub_dag$tf_environment

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

    sub_tfe$joint_density

  }

}
