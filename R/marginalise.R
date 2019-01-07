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
#'   marginalisation within a greta model, using one of the available marginalisation methods.
#'
#' @return \code{discrete_marginalisation} - a \code{marginaliser} object that
#'   can be passed to \code{marginalise}.
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
#' fun <- function (n, theta, mu, sd) {
#'   mu <- theta ^ n
#'   distribution(y) <- normal(mu, sd)
#' }
#'
#' # integrate the function values w.r.t. the poisson distribution
#' marginalise(fun,
#'             poisson(lambda),
#'             method = discrete_marginalisation(values = 0:10),
#'             theta = theta,
#'             mu = mu,
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
#'   The first argument must be - a greta array giving the value of the random
#'   variable, any subsequent arguments must passed in via the \dots argument.
#' @param variable a variable greta array with a distribution, representing the
#'   random variable to marginalise
#' @param method a \code{marginaliser} object giving the method for carrying out
#'   the marginalisation
#' @param \dots named greta arrays to be passed to \code{fun}
marginalise <- function(fun, variable, method, ...) {

  # get the distribution for the random variable
  distrib <- get_node(variable)$distribution

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

  if (!inherits(distrib, "distribution_node")) {
    stop ("'variable' must be a variable greta array with a distribution")
  }

  if (!inherits(method, "marginaliser")) {
    stop ("'method' must be a valid marginalisation method. ",
          "See ?marginalise for options")
  }

  # check the distribution is compatible with the method
  method$distribution_check(distrib)

  # excise the variable from the distribution
  distrib$remove_target()

  stop ("not yet implemented")

  # turn the greta function into a TF conditional density function; doing
  # something very similar to as_tf_function(), but giving a function that
  # returns a tensorflow scalar for the density
  conditional_joint_density <- as_conditional_density(fun)

  # get a tf function from the method which will turn that conditional density
  # function into a marginal density (conditional on the inputs to the
  # distribution) to be added to the overall model density
  method$tf_marginaliser

  # pass the conditional density function and the marginaliser TF function to a
  # marginalisation distribution (needs an R6 class)

  # create the distribution
  vble <- distrib(
    "marginalisation",
    marginaliser = method,
    conditional_density_fun = conditional_joint_density,
    distribution = distrib,
    dots = dots
  )

  # excise the variable from the marginalisation distribution
  distrib <- get_node(variable)$distribution
  distrib$remove_target()

  # return nothing
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
                          distribution,
                          dots) {

      # initialize class, and add methods
      super$initialize("marginalisation")
      self$tf_marginaliser <- marginaliser$tf_marginaliser
      self$conditional_density_fun <- conditional_density_fun

      # add the dots (extra inputs to conditional_density_fun) as parameters
      dot_nodes <- lapply(dots, get_node)
      for (i in seq_len(dot_nodes)) {
        self$add_parameter(dot_nodes[[i]],
                           paste("input", i))
      }

      # add the distribution as a parameter
      self$add_parameter(distribution, "distribution")

    },

    tf_distrib = function(parameters, dag) {

      # do the marginalisation

      # but it has no target!!
      # should we handle distributions with no targets as a special case?
      # or should this not be a distribution after all?

      log_prob <- function(x) {

        self$tf_marginaliser(self$conditional_density_fun,
                             parameters$distribution,
                             parameters[names(parameters) != "distribution"])

      }

      list(log_prob = log_prob, cdf = NULL, log_cdf = NULL)

    },

    tf_cdf_function = NULL,
    tf_log_cdf_function = NULL

  )
)

#' @rdname marginalisation
#' @export
#'
#' @param values an R vector giving values at which to evaluate the function for a
#'   discrete marginalisation
#'
#' @details \code{discrete_marginalisation} can only be used with discrete
#'   probability distributions, e.g. those defined with \code{poisson()} and
#'   \code{binomial()}. For discrete distributions with finite support (such as
#'   \code{bernoulli()}) the marginalisation will be exact, so long as
#'   \code{values} includes all possible values of the variable. For discrete
#'   distributions with non-finite support (such as \code{poisson()}, which has
#'   no upper bound), the marginalisation can only ever be approximate. However
#'   if \code{values} cover a range of values with sufficiently high support in
#'   the distribution, that approximation error will be minimal.
discrete_marginalisation <- function(values) {

  if (!is.vector(values) | !is.numeric(values)) {
    msg <- "'values' must be an R numeric vector"

    if (inherits(values, "greta_array")) {
      msg <- paste0(msg, ", not a greta array")
    }

    stop (msg)
  }

  # define the marginalisation function
  tf_marginaliser <- function(conditional_density_fun,
                              distribution,
                              other_args) {
    # base this on mixture;
    # 1. get weights from the distribution (use its log_pdf tf method on the
    #   values)
    # 2. compute the conditional joint density for each value (passing in
    #   other_args)
    # 3. compute a weighted sum with tf_reduce_log_sum_exp()

    stop ("not yet implemented")
  }

  as_marginaliser(name = "discrete",
                  tf_marginaliser = tf_marginaliser,
                  parameters = list(values = values),
                  distribution_check = discrete_check)

}

# check that the distribution is discrete
discrete_check <- function(distrib) {
  if (!distrib$discrete) {
    stop ("this marginalisation method can only be used ",
          "with discrete distributions",
          call. = FALSE)
  }
}

# helper to contruct marginalisers
as_marginaliser <- function (name, method, parameters, distribution_check) {

  obj <- list(name = name,
              method = method,
              parameters = parameters,
              distribution_check = distribution_check)

  class_name <- paste0(name, "_marginaliser")
  class(obj) <- c(class_name, "marginaliser")
  obj

}

#' @noRd
#' @export
print.marginaliser <- function(x, ...) {
  msg <- paste(x$name, "marginaliser object")
  cat(msg)
}
