
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

bernoulli_distribution <- R6Class (
  'bernoulli_distribution',
  inherit = distribution,
  public = list(

    to_free = function (y)
      stop ('cannot infer discrete random variables'),

    tf_from_free = function (x, env)
      stop ('cannot infer discrete random variables'),

    initialize = function (p, dim = 1) {
      # add the nodes as children and parameters
      super$initialize('bernoulli', dim, discrete = TRUE)
      self$add_parameter(p, 'p')
    },

    tf_log_density_function = function (x, parameters) {

      p <- parameters$p
      tf$log(tf$select(tf$equal(x, 1), p, 1 - p))

    }

  )
)

binomial_distribution <- R6Class (
  'binomial_distribution',
  inherit = distribution,
  public = list(

    to_free = function (y)
      stop ('cannot infer discrete random variables'),

    tf_from_free = function (x, env)
      stop ('cannot infer discrete random variables'),

    initialize = function (size, p, dim = 1) {
      # add the nodes as children and parameters
      super$initialize('binomial', dim, discrete = TRUE)
      self$add_parameter(size, 'size')
      self$add_parameter(p, 'p')

    },

    tf_log_density_function = function (x, parameters) {

      size <- parameters$size
      p <- parameters$p

      log_choose <- tf$lgamma(size + 1) - tf$lgamma(x + 1) -
        tf$lgamma(size - x + 1)
      log_choose + x * tf$log(p) + (size - x) * tf$log(1 - p)

    }

  )
)

poisson_distribution <- R6Class (
  'poisson_distribution',
  inherit = distribution,
  public = list(

    to_free = function (y)
      stop ('cannot infer discrete random variables'),

    tf_from_free = function (x, env)
      stop ('cannot infer discrete random variables'),

    initialize = function (lambda, dim = 1) {
      # add the nodes as children and parameters
      super$initialize('poisson', dim, discrete = TRUE)
      self$add_parameter(lambda, 'lambda')
    },

    tf_log_density_function = function (x, parameters) {

      lambda <- parameters$lambda
      x * tf$log(lambda) - lambda - tf$lgamma(x + 1)

    }

  )
)

negative_binomial_distribution <- R6Class (
  'negative_binomial_distribution',
  inherit = distribution,
  public = list(

    to_free = function (y)
      stop ('cannot infer discrete random variables'),

    tf_from_free = function (x, env)
      stop ('cannot infer discrete random variables'),

    initialize = function (p, r, dim = 1) {
      # add the nodes as children and parameters
      super$initialize('negative_binomial', dim, discrete = TRUE)
      self$add_parameter(p, 'p')
      self$add_parameter(r, 'r')
    },

    tf_log_density_function = function (x, parameters) {

      r <- parameters$r
      p <- parameters$p

      log_choose <- tf$lgamma(x + r) - tf$lgamma(x + 1) -
        tf$lgamma(r)
      log_choose + r * tf$log(p) + x * tf$log(1 - p)

    }

  )
)

gamma_distribution <- R6Class (
  'gamma_distribution',
  inherit = distribution,
  public = list(

    to_free = function (y) log(expm1(y)),
    tf_from_free = function (x, env) tf_log1pe(x),

    initialize = function (shape, scale, dim = 1) {
      # add the nodes as children and parameters
      super$initialize('gamma', dim)
      self$add_parameter(shape, 'shape')
      self$add_parameter(scale, 'scale')
    },

    tf_log_density_function = function (x, parameters) {

      shape <- parameters$shape
      scale <- parameters$scale

      -shape * tf$log(scale) - tf$lgamma(shape) +
        (shape - 1) * tf$log(x) - x / scale

    }

  )
)

exponential_distribution <- R6Class (
  'exponential_distribution',
  inherit = distribution,
  public = list(

    to_free = function (y) log(expm1(y)),
    tf_from_free = function (x, env) tf_log1pe(x),

    initialize = function (rate, dim = 1) {
      # add the nodes as children and parameters
      super$initialize('exponential', dim)
      self$add_parameter(rate, 'rate')
    },

    tf_log_density_function = function (x, parameters) {

      rate <- parameters$shape
      -x / rate - tf$log(rate)

    }

  )
)

student_distribution <- R6Class (
  'student_distribution',
  inherit = distribution,
  public = list(

    to_free = function (y) y,
    tf_from_free = function (x, env) x,

    initialize = function (mean, df, dim = 1) {
      # add the nodes as children and parameters
      super$initialize('student', dim)
      self$add_parameter(mean, 'mean')
      self$add_parameter(df, 'df')
    },

    tf_log_density_function = function (x, parameters) {

      mean <- parameters$mean
      df <- parameters$df

      const <- tf$lgamma((df + 1) * 0.5) - tf$lgamma(df * 0.5) -
        0.5 * (tf$log(df) + log(pi))
      const - 0.5 * (df + 1) * tf$log(1 + (1 / df) * (tf$square(x - mean)))

    }

  )
)

beta_distribution <- R6Class (
  'beta_distribution',
  inherit = distribution,
  public = list(

    to_free = function (y) qlogis(y),
    tf_from_free = function (x, env) tf_ilogit(x),

    initialize = function (shape1, shape2, dim = 1) {
      # add the nodes as children and parameters
      super$initialize('beta', dim)
      self$add_parameter(shape1, 'shape2')
      self$add_parameter(shape2, 'shape2')
    },

    tf_log_density_function = function (x, parameters) {

      shape1 <- parameters$shape1
      shape2 <- parameters$shape2

      (shape1 - 1) * tf$log(x) +
        (shape2 - 1) * tf$log(1 - x) +
        tf$lgamma(shape1 + shape2) -
        tf$lgamma(shape1) - tf$lgamma(shape2)

    }

  )
)



# export constructors

#' @name greta-distributions
#' @title greta probability distributions
#' @description These probability distributions can be used to define random
#'   variables in a greta model. They return a 'node' object that can be
#'   combined with other nodes to construct a model.
#'
#' @param mu unconstrained parameter
#' @param sigma positive parameter (\code{sigma > 0})
#' @param p probability parameter (\code{0 < p < 1})
#' @param size positive integer parameter (\code{size} in \code{{1, 2, 3, ...}})
#' @param r positive parameter (\code{r > 0})
#' @param lambda positive parameter (\code{lambda > 0})
#' @param shape positive parameter (\code{shape > 0})
#' @param scale positive parameter (\code{scale > 0})
#' @param rate positive parameter (\code{rate > 0})
#' @param mean unconstrained parameter
#' @param df positive parameter (\code{df > 0})
#' @param shape1 positive parameter (\code{shape1 > 0})
#' @param shape2 positive parameter (\code{shape2 > 0})
#'
#' @param range a finite, length 2 numeric vector giving the range of values to
#'   which \code{flat} distributions are constrained. The first element must
#'   be lower than the second.
#' @param dim the dimensions of the variable, by default a scalar
#'
#' @details Most of these distributions have non-uniform probability densities,
#'   however the distributions \code{flat} and \code{free} do not. These can
#'   therefore be used as parameters in likelihood (rather than Bayesian)
#'   inference.
#'
#'   The discrete probability distributions (\code{bernoulli}, \code{poisson})
#'   can be used as likelihoods, but not as unknown variables.
#'
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
NULL

#' @rdname greta-distributions
#' @export
free <- function (dim = 1)
  free_distribution$new(dim = dim)

#' @rdname greta-distributions
#' @export
normal <- function (mu, sigma, dim = 1)
  normal_distribution$new(mu = mu, sigma = sigma, dim = dim)

#' @rdname greta-distributions
#' @export
lognormal <- function (mu, sigma, dim = 1)
  lognormal_distribution$new(mu = mu, sigma = sigma, dim = dim)

#' @rdname greta-distributions
#' @export
bernoulli <- function (p, dim = 1)
  bernoulli_distribution$new(p, dim = dim)

#' @rdname greta-distributions
#' @export
binomial <- function (size, p, dim = 1)
  binomial_distribution$new(size, p, dim = dim)

#' @rdname greta-distributions
#' @export
negative_binomial <- function (p, r, dim = 1)
  negative_binomial_distribution$new(p, r, dim = dim)

#' @rdname greta-distributions
#' @export
poisson <- function (lambda, dim = 1)
  poisson_distribution$new(lambda, dim = dim)

#' @rdname greta-distributions
#' @export
gamma <- function (shape, scale, dim = 1)
  gamma_distribution$new(shape, scale, dim = dim)

#' @rdname greta-distributions
#' @export
exponential <- function (rate, dim = 1)
  exponential_distribution$new(rate, dim = dim)

#' @rdname greta-distributions
#' @export
student <- function (mean, df, dim = 1)
  student_distribution$new(mean, df, dim = dim)

#' @rdname greta-distributions
#' @export
beta <- function (shape1, shape2, dim = 1)
  beta_distribution$new(shape1, shape2, dim = dim)

#' @rdname greta-distributions
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

