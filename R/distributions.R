
flat_distribution <- R6Class (
  'flat_distribution',
  inherit = distribution,
  public = list(

    to_free = function (y) {
      upper <- self$parameters$upper$value()
      lower <- self$parameters$lower$value()
      qlogis((y - lower) / (upper - lower))
    },

    tf_from_free = function (x, env) {

      # cannot allow the upper and lower values to be nodes
      # otherwise it would severely screw with the gradients
      upper <- self$parameters$upper$value()
      lower <- self$parameters$lower$value()

      (1 / (1 + tf$exp(-1 * x))) * (upper - lower) + lower

    },

    initialize = function (lower = -1e6, upper = 1e6, dim = 1) {

      if (!(is.numeric(lower) & is.numeric(upper) &
            is.finite(lower) & is.finite(upper) &
            length(lower) == 1 & length(upper) == 1)) {
        stop ('lower and upper must be finite scalars')
      }

      super$initialize('flat', dim)

      self$add_parameter(lower, 'lower')
      self$add_parameter(upper, 'upper')
    },

    tf_log_density_function = function (value, parameters)
      tf$constant(0, dtype = tf$float32)

  )
)

free_distribution <- R6Class (
  'free_distribution',
  inherit = distribution,
  public = list(

    to_free = function (y) y,

    tf_from_free = function (x, env) x,

    initialize = function (dim = 1)
      super$initialize('free', dim),

    tf_log_density_function = function (value, parameters)
      tf$constant(0, dtype = tf$float32)

  )
)

normal_distribution <- R6Class (
  'normal_distribution',
  inherit = distribution,
  public = list(

    to_free = function (y) y,

    tf_from_free = function (x, env) x,

    initialize = function (mu, sigma, dim = 1) {
      # add the nodes as children and parameters
      super$initialize('normal', dim)
      self$add_parameter(mu, 'mu')
      self$add_parameter(sigma, 'sigma')
    },

    tf_log_density_function = function (x, parameters) {

      mu <- parameters$mu
      var <- tf$square(parameters$sigma)

      -0.5 * tf$log(2 * pi) - 0.5 * tf$log(var) - 0.5 * tf$square(tf$sub(mu, x)) / var

    }

  )
)

lognormal_distribution <- R6Class (
  'lognormal_distribution',
  inherit = distribution,
  public = list(

    to_free = log,
    tf_from_free = function (x, env) tf$exp(x),

    initialize = function (mu, sigma, dim = 1) {
      super$initialize('lognormal', dim)
      self$add_parameter(mu, 'mu')
      self$add_parameter(sigma, 'sigma')
    },

    tf_log_density_function = function (x, parameters) {

      mu <- parameters$mu
      var <- tf$square(parameters$sigma)
      lx <- tf$log(x)

      -0.5 * tf$log(2 * pi) - 0.5 * tf$log(var) - 0.5 * tf$square(tf$sub(mu, lx)) / var


    }
  )
)

# export constructors

#' @name greta-distributions
#' @title greta probability distributions
#' @description These probability distributions can be used to define random
#'   variables in a greta model. They return a 'node' object that can be
#'   combined with other nodes to construct a model.
#' @param mu,sigma probability distribution parameters
#' @param range a finite, length 2 numeric vector giving the range of values to
#'   which \code{flat} distributions are constrained. The first element must
#'   be lower than the second.
#' @param dim the dimensions of the variable, by default a scalar
#' @details Most of these distributions have non-uniform probability densities,
#'   however the distributions \code{flat} and \code{free} do not. These can
#'   therefore be used as parameters in likelihood (rather than Bayesian)
#'   inference. By default, all distributions are
#' @examples
#' # a fixed distribution, e.g. for a prior
#' mu = normal(0, 1)
#'
#' # an unconstrained, positive parameter sigma
#' log_sigma = free()
#' sigma = exp(log_sigma)
#'
#' # a hierarchical distribution
#' theta = normal(mu, lognormal(0, 1))
#'
#' # a vector of 3 variables drawn from the same hierarchical distribution
#' thetas = normal(mu, sigma, dim = 3)
#'
#' # a matrix of 12 variables drawn from the same hierarchical distribution
#' thetas = normal(mu, sigma, dim = c(3, 4))
#'
#' # a constrained parameter with no density (e.g. for a constrained likelihood model)
#' theta = flat(c(1, 5))

#' @rdname distributions
#' @export
normal <- function (mu, sigma, dim = 1)
  normal_distribution$new(mu = mu, sigma = sigma, dim = dim)

#' @rdname distributions
#' @export
lognormal <- function (mu, sigma, dim = 1)
  lognormal_distribution$new(mu = mu, sigma = sigma, dim = dim)

#' @rdname distributions
#' @export
flat <- function (range, dim = 1) {
  if (is_node(range))
    stop ('range must be fixed, and cannot be another node')
  if (!(is.vector(range) && length(range) == 2 &&
        is.numeric(range) && range[1] < range[2])) {
    stop ('range must be a length 2 numeric vector in ascending order')
  }
  flat_distribution$new(lower = range[1], upper = range[2], dim = dim)
}

#' @rdname distributions
#' @export
free <- function (dim = 1)
  free_distribution$new(dim = dim)
