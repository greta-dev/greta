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
#' # the function to marginalise over
#' # (only the variable to marginalise is passed in)
#' fun <- function (n) {
#'   mu <- theta ^ n
#'   distribution(y) <- normal(mu, sd)
#' }
#'
#' # integrate the function values w.r.t. the poisson distribution
#' marginalise(fun,
#'             poisson(lambda),
#'             method = discrete_marginalisation(values = 0:10))
#'
#' m <- model(lambda)
#' draws <- mcmc(m, hmc())
#' }
NULL

#' @rdname marginalisation
#' @export
#'
#' @param fun an R function to integrate with respect to the random variable.
#'   Must take a single argument - a greta array giving the value of the random
#'   variable
#' @param variable a variable greta array with a distribution, representing the
#'   random variable to marginalise
#' @param method a \code{marginaliser} object giving the method for carrying out
#'   the marginalisation
marginalise <- function(fun, variable, method) {

  distrib <- get_node(variable)$distribution

  # check the inputs
  if (!is.function(fun)) {
    stop ("'fun' must be an R function")
  }

  if (!inherits(distrib, "distribution_node")) {
    stop ("'variable' must be a variable greta array with a distribution")
  }

  # handle case when the method is passed like: `laplace_approximation`, rather
  # than `laplace_approximation()`
  if (is.function(method)) {
    method <- method()
  }

  if (!inherits(method, "marginalisation_method")) {
    stop ("'method' must be a valid marginalisation method. ",
          "See ?marginalise for options")
  }

  # check the distribution is compatible with the method
  method$distribution_check(distrib)

  stop ("not yet implemented")

}

#' @rdname marginalisation
#' @export
#'
#' @param fun an R vector giving values at which to evaluate the function for a
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

  if (inherits(values, "greta_array")) {
    stop ("'values' must be an R numeric vector, not a greta array")
  }

  # define the marginalisation function
  marginalisation_function <- function() {
    stop ("not yet implemented")
  }

  as_marginaliser(name = "discrete",
                  method = marginalisation_function,
                  parameters = list(values = values),
                  distribution_check = discrete_check)

}

# check that the distribution is discrete
discrete_check <- function(node) {
  if (!distrib$discrete) {
    stop ("this marginalisation method can only be used with discrete distributions")
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
