
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

    initialize = function (mean, sd, dim = 1) {
      # add the nodes as children and parameters
      super$initialize('normal', dim)
      self$add_parameter(mean, 'mean')
      self$add_parameter(sd, 'sd')
    },

    tf_log_density_function = function (x, parameters) {

      mean <- parameters$mean
      var <- tf$square(parameters$sd)

      -0.5 * tf$log(2 * pi) - 0.5 * tf$log(var) - 0.5 * tf$square(tf$sub(mean, x)) / var

    }

  )
)

lognormal_distribution <- R6Class (
  'lognormal_distribution',
  inherit = distribution,
  public = list(

    to_free = log,
    tf_from_free = function (x, env) tf$exp(x),

    initialize = function (meanlog, sdlog, dim = 1) {
      super$initialize('lognormal', dim)
      self$add_parameter(meanlog, 'meanlog')
      self$add_parameter(sdlog, 'sdlog')
    },

    tf_log_density_function = function (x, parameters) {

      mean <- parameters$meanlog
      var <- tf$square(parameters$sdlog)
      lx <- tf$log(x)

      -0.5 * tf$log(2 * pi) - 0.5 * tf$log(var) - 0.5 * tf$square(tf$sub(mean, lx)) / var

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

    initialize = function (prob, dim = 1) {
      # add the nodes as children and parameters
      super$initialize('bernoulli', dim, discrete = TRUE)
      self$add_parameter(prob, 'prob')
    },

    tf_log_density_function = function (x, parameters) {

      prob <- parameters$prob
      tf$log(tf$select(tf$equal(x, 1), prob, 1 - prob))

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

    initialize = function (size, prob, dim = 1) {
      # add the nodes as children and parameters
      super$initialize('binomial', dim, discrete = TRUE)
      self$add_parameter(size, 'size')
      self$add_parameter(prob, 'prob')

    },

    tf_log_density_function = function (x, parameters) {

      size <- parameters$size
      prob <- parameters$prob

      log_choose <- tf$lgamma(size + 1) - tf$lgamma(x + 1) -
        tf$lgamma(size - x + 1)
      log_choose + x * tf$log(prob) + (size - x) * tf$log(1 - prob)

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

    initialize = function (size, prob, dim = 1) {
      # add the nodes as children and parameters
      super$initialize('negative_binomial', dim, discrete = TRUE)
      self$add_parameter(size, 'size')
      self$add_parameter(prob, 'prob')
    },

    tf_log_density_function = function (x, parameters) {

      size <- parameters$size
      prob <- parameters$prob

      log_choose <- tf$lgamma(x + size) - tf$lgamma(x + 1) -
        tf$lgamma(size)
      log_choose + size * tf$log(prob) + x * tf$log(1 - prob)

    }

  )
)

gamma_distribution <- R6Class (
  'gamma_distribution',
  inherit = distribution,
  public = list(

    to_free = function (y) log(expm1(y)),
    tf_from_free = function (x, env) tf_log1pe(x),

    initialize = function (shape, rate, dim = 1) {
      # add the nodes as children and parameters
      super$initialize('gamma', dim)
      self$add_parameter(shape, 'shape')
      self$add_parameter(rate, 'rate')
    },

    tf_log_density_function = function (x, parameters) {

      shape <- parameters$shape
      scale <- 1 /parameters$rate

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

    initialize = function (df, ncp, dim = 1) {
      # add the nodes as children and parameters
      super$initialize('student', dim)
      self$add_parameter(df, 'df')
      self$add_parameter(ncp, 'ncp')
    },

    tf_log_density_function = function (x, parameters) {

      df <- parameters$df
      ncp <- parameters$ncp

      const <- tf$lgamma((df + 1) * 0.5) - tf$lgamma(df * 0.5) -
        0.5 * (tf$log(df) + log(pi))
      const - 0.5 * (df + 1) * tf$log(1 + (1 / df) * (tf$square(x - ncp)))

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
#' @param mean,meanlog,ncp unconstrained parameters
#' @param sd,sdlog,size,lambda,shape,scale,rate,df,shape1,shape2 positive parameters
#' @param prob probability parameter (\code{0 < prob < 1})
#'
#' @param range a finite, length 2 numeric vector giving the range of values to
#'   which \code{flat} distributions are constrained. The first element must
#'   be lower than the second.
#'
#' @param dim the dimensions of the variable, by default a scalar
#'
#' @details Most of these distributions have non-uniform probability densities,
#'   however the distributions \code{flat} and \code{free} do not. These can
#'   therefore be used as parameters in likelihood (rather than Bayesian)
#'   inference.
#'
#'   The discrete probability distributions (\code{bernoulli}, \code{binomial},
#'   \code{negative_binomial}, \code{poisson}) can be used as likelihoods, but
#'   not as unknown variables.
#'
#'   Wherever possible, the parameterisation of these distributions matches the
#'   those in the \code{stats} package. E.g. for the parameterisation of
#'   \code{negative_binomial()}, see \code{\link{dnbinom}}.
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
normal <- function (mean, sd, dim = 1)
  normal_distribution$new(mean, sd, dim)

#' @rdname greta-distributions
#' @export
lognormal <- function (meanlog, sdlog, dim = 1)
  lognormal_distribution$new(meanlog, sdlog, dim)

#' @rdname greta-distributions
#' @export
bernoulli <- function (prob, dim = 1)
  bernoulli_distribution$new(prob, dim)

#' @rdname greta-distributions
#' @export
binomial <- function (size, prob, dim = 1)
  binomial_distribution$new(size, prob, dim)

#' @rdname greta-distributions
#' @export
negative_binomial <- function (size, prob, dim = 1)
  negative_binomial_distribution$new(size, prob, dim)

#' @rdname greta-distributions
#' @export
poisson <- function (lambda, dim = 1)
  poisson_distribution$new(lambda, dim)

#' @rdname greta-distributions
#' @export
gamma <- function (shape, rate, dim = 1)
  gamma_distribution$new(shape, rate, dim)

#' @rdname greta-distributions
#' @export
exponential <- function (rate, dim = 1)
  exponential_distribution$new(rate, dim)

#' @rdname greta-distributions
#' @export
student <- function (df, ncp, dim = 1)
  student_distribution$new(df, ncp, dim)

#' @rdname greta-distributions
#' @export
beta <- function (shape1, shape2, dim = 1)
  beta_distribution$new(shape1, shape2, dim)

#' @rdname greta-distributions
#' @export
free <- function (dim = 1)
  free_distribution$new(dim)

#' @rdname greta-distributions
#' @export
flat <- function (range, dim) {
  if (is_node(range))
    stop ('range must be fixed, and cannot be another node')
  if (!(is.vector(range) && length(range) == 2 &&
        is.numeric(range) && range[1] < range[2])) {
    stop ('range must be a length 2 numeric vector in ascending order')
  }
  flat_distribution$new(lower = range[1], upper = range[2], dim = dim)
}

