free_distribution <- R6Class (
  'free_distribution',
  inherit = distribution,
  public = list(

    constraint = NULL,

    initialize = function (lower = -Inf, upper = Inf, dim = 1) {

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

      # must be length one, and can't be greta arrays
      if (length(lower) != 1 | length(upper) != 1 |
          !is.numeric(lower) | !is.numeric(upper)) {

        bad_limits <- TRUE

      }

      if (bad_limits) {

        stop ('lower and upper must either be -Inf (lower only), ',
              'Inf (upper only) or finite scalars')

      }

      if (lower >= upper) {

        stop ('upper bound must be greater than lower bound')

      }

      # add parameters
      super$initialize('free', dim)
      self$add_parameter(lower, 'lower')
      self$add_parameter(upper, 'upper')

    },


    to_free = function (y) {

      if (self$constraint == 'none') {

        x <- y

      } else if (self$constraint == 'both') {

        upper <- self$parameters$upper$value()
        lower <- self$parameters$lower$value()
        x <- qlogis((y - lower) / (upper - lower))

      } else if (self$constraint == 'low') {

        upper <- self$parameters$upper$value()
        baseline <- upper - y
        x <- log(exp(baseline) - 1)

      } else if (self$constraint == 'high') {

        lower <- self$parameters$lower$value()
        baseline <- y - lower
        x <- log(exp(baseline) - 1)

      }

      x

    },

    tf_from_free = function (x, env) {

      if (self$constraint == 'none') {

        y <- x

      } else if (self$constraint == 'both') {

        upper <- self$parameters$upper$value()
        lower <- self$parameters$lower$value()
        y <- (1 / (1 + tf$exp(-1 * x))) * (upper - lower) + lower

      } else if (self$constraint == 'low') {

        upper <- self$parameters$upper$value()
        baseline <- tf$log(1 + tf$exp(x))
        y <- upper - baseline

      } else if (self$constraint == 'high') {

        lower <- self$parameters$lower$value()
        baseline <- tf$log(1 + tf$exp(x))
        y <- baseline + lower

      }

      y

    },

    tf_log_density_function = function (value, parameters)
      tf$constant(0, dtype = tf$float32)

  )
)

uniform_distribution <- R6Class (
  'uniform_distribution',
  inherit = distribution,
  public = list(

    log_density = NULL,

    to_free = function (y) {
      max <- self$parameters$max$value()
      min <- self$parameters$min$value()
      qlogis((y - min) / (max - min))
    },

    tf_from_free = function (x, env) {

      max <- self$parameters$max$value()
      min <- self$parameters$min$value()
      (1 / (1 + tf$exp(-1 * x))) * (max - min) + min

    },

    initialize = function (min, max, dim) {

      # check and assign limits
      bad_limits <- FALSE

      if (!is.finite(min) | !is.finite(max))
        bad_limits <- TRUE

      # must be length one, and can't be greta arrays
      if (length(min) != 1 | length(max) != 1 |
          !is.numeric(max) | !is.numeric(max)) {

        bad_limits <- TRUE

      }

      if (bad_limits) {

        stop ('min and max must finite scalars')

      }

      if (min >= max) {

        stop ('max must be greater than min')

      }

      # add the nodes as children and parameters
      super$initialize('uniform', dim)
      self$add_parameter(min, 'min')
      self$add_parameter(max, 'max')

      # the density is fixed, so calculate it now
      self$log_density <- tf$constant(-log(max - min))

    },

    # weird hack to make TF see a gradient here
    tf_log_density_function = function (x, parameters)
      self$log_density + tf$reduce_sum(x * 0)

  )
)


normal_distribution <- R6Class (
  'normal_distribution',
  inherit = distribution,
  public = list(

    to_free = function (y) y,

    tf_from_free = function (x, env) x,

    initialize = function (mean, sd, dim) {
      # add the nodes as children and parameters
      dim <- check_dims(mean, sd, target_dim = dim)
      super$initialize('normal', dim)
      self$add_parameter(mean, 'mean')
      self$add_parameter(sd, 'sd')
    },

    tf_log_density_function = function (x, parameters) {

      mean <- parameters$mean
      var <- tf$square(parameters$sd)

      -0.5 * tf$log(2 * pi) - 0.5 * tf$log(var) - 0.5 * tf$square(tf$subtract(mean, x)) / var

    }

  )
)

lognormal_distribution <- R6Class (
  'lognormal_distribution',
  inherit = distribution,
  public = list(

    to_free = log,
    tf_from_free = function (x, env) tf$exp(x),

    initialize = function (meanlog, sdlog, dim) {
      dim <- check_dims(meanlog, sdlog, target_dim = dim)
      super$initialize('lognormal', dim)
      self$add_parameter(meanlog, 'meanlog')
      self$add_parameter(sdlog, 'sdlog')
    },

    tf_log_density_function = function (x, parameters) {

      mean <- parameters$meanlog
      sd <- parameters$sdlog
      var <- tf$square(sd)
      lx <- tf$log(x)

      -1 * (lx + tf$log(sd) + 0.9189385) +
        -0.5 * tf$square(tf$subtract(lx, mean)) / var

    }
  )
)

bernoulli_distribution <- R6Class (
  'bernoulli_distribution',
  inherit = distribution,
  public = list(

    initialize = function (prob, dim) {
      # add the nodes as children and parameters
      dim <- check_dims(prob, target_dim = dim)
      super$initialize('bernoulli', dim, discrete = TRUE)
      self$add_parameter(prob, 'prob')
    },

    tf_log_density_function = function (x, parameters) {

      prob <- parameters$prob

      # optionally reshape prob
      prob_shape <- prob$get_shape()$as_list()
      x_shape <- x$get_shape()$as_list()

      if (identical(prob_shape, c(1L, 1L)) & !identical(x_shape, c(1L, 1L)))
        probs <- tf$tile(prob, x_shape)

      tf$log(tf$where(tf$equal(x, 1), probs, 1 - probs))

    }

  )
)

binomial_distribution <- R6Class (
  'binomial_distribution',
  inherit = distribution,
  public = list(

    initialize = function (size, prob, dim) {
      # add the nodes as children and parameters
      dim <- check_dims(size, prob, target_dim = dim)
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

    initialize = function (lambda, dim) {
      # add the nodes as children and parameters
      dim <- check_dims(lambda, target_dim = dim)
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

    initialize = function (size, prob, dim) {
      # add the nodes as children and parameters
      dim <- check_dims(size, prob, target_dim = dim)
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

    initialize = function (shape, rate, dim) {
      # add the nodes as children and parameters
      dim <- check_dims(shape, rate, target_dim = dim)
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

    initialize = function (rate, dim) {
      # add the nodes as children and parameters
      dim <- check_dims(rate, target_dim = dim)
      super$initialize('exponential', dim)
      self$add_parameter(rate, 'rate')
    },

    tf_log_density_function = function (x, parameters) {

      rate <- parameters$rate
      tf$log(rate) - rate * x

    }

  )
)

student_distribution <- R6Class (
  'student_distribution',
  inherit = distribution,
  public = list(

    to_free = function (y) y,
    tf_from_free = function (x, env) x,

    initialize = function (df, location, scale, dim) {
      # add the nodes as children and parameters
      dim <- check_dims(df, location, scale, target_dim = dim)
      super$initialize('student', dim)
      self$add_parameter(df, 'df')
      self$add_parameter(location, 'location')
      self$add_parameter(scale, 'scale')
    },

    tf_log_density_function = function (x, parameters) {

      df <- parameters$df
      location <- parameters$location
      scale <- parameters$scale

      x_ <- (x - location) / scale

      const <- tf$lgamma((df + 1) * 0.5) -
        tf$lgamma(df * 0.5) -
        0.5 * (tf$log(tf$square(scale)) + tf$log(df) + log(pi))

      const - 0.5 * (df + 1) * tf$log(1 + (1 / df) * (tf$square(x_)))

    }

  )
)

beta_distribution <- R6Class (
  'beta_distribution',
  inherit = distribution,
  public = list(

    to_free = function (y) qlogis(y),
    tf_from_free = function (x, env) tf_ilogit(x),

    initialize = function (shape1, shape2, dim) {
      # add the nodes as children and parameters
      dim <- check_dims(shape1, shape2, target_dim = dim)
      super$initialize('beta', dim)
      self$add_parameter(shape1, 'shape1')
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

# need to add checking of mean and Sigma dimensions
multivariate_normal_distribution <- R6Class (
  'multivariate_normal_distribution',
  inherit = distribution,
  public = list(

    to_free = function (y) y,
    tf_from_free = function (x, env) x,

    initialize = function (mean, Sigma, dim) {

      # coerce to greta arrays
      mean <- ga(mean)
      Sigma <- ga(Sigma)

      # check dimensions of mean
      if (ncol(mean) != 1 |
          length(dim(mean)) != 2) {

        stop ('mean must be a 2D greta array with one column, but has dimensions ',
              paste(dim(Sigma), collapse = ' x '))

      }

      # check dimensions of Sigma
      if (nrow(Sigma) != ncol(Sigma) |
          length(dim(Sigma)) != 2) {

        stop ('Sigma must be a square 2D greta array, but has dimensions ',
              paste(dim(Sigma), collapse = ' x '))

      }

      # compare possible dimensions
      dim_mean <- nrow(mean)
      dim_Sigma <- nrow(Sigma)

      if (dim_mean != dim_Sigma) {
        stop ('mean and Sigma have different dimensions, ',
              dim_mean, ' vs ', dim_Sigma)
      }

      if (dim_mean == 1)
        stop ('the multivariate normal distribution is for vectors, but the parameters were scalar')

      # check dim is a positive scalar integer
      dim <- as.integer(dim)
      if (length(dim) > 1 | dim <= 0 | !is.finite(dim))
        stop ('dim must be a scalar positive integer, but was: ', dput(dim))

      # coerce the parameter arguments to nodes and add as children and
      # parameters
      super$initialize('multivariate_normal', c(dim, dim_mean))
      self$add_parameter(mean, 'mean')
      self$add_parameter(Sigma, 'Sigma')

    },

    tf_log_density_function = function (x, parameters) {

      mean <- parameters$mean
      Sigma <- parameters$Sigma

      # number of observations & dimension of distribution
      nobs <- x$get_shape()$as_list()[1]
      dim <- x$get_shape()$as_list()[2]

      # Cholesky decomposition of Sigma
      L <- tf$cholesky(Sigma)

      # whiten (decorrelate) the errors
      mean_col <- tf$tile(mean, c(1L, nobs))
      diff_col <- tf$transpose(x) - mean_col
      alpha <- tf$matrix_triangular_solve(L, diff_col, lower = TRUE)

      # calculate density, per-row in x:
      -0.5 * dim * log(2 * pi) -
        tf$reduce_sum(tf$log(tf$diag_part(L))) -
        0.5 * tf$reduce_sum(tf$square(alpha), axis = 0L)
    }

  )
)

# need to add checking of mean and Sigma dimensions
wishart_distribution <- R6Class (
  'wishart_distribution',
  inherit = distribution,
  public = list(

    # grab it in lower-triangular form, so it's upper when putting it back in python-style
    to_free = function (y) {
      L <- t(chol(y))
      vals <- L[lower.tri(L, diag = TRUE)]
      matrix(vals)
    },

    tf_from_free = function (x, env) {
      dims <- self$parameters$Sigma$dim
      L_dummy <- greta:::dummy(dims)
      indices <- sort(L_dummy[upper.tri(L_dummy, diag = TRUE)])
      values <- tf$zeros(shape(prod(dims), 1), dtype = tf$float32)
      values <- greta:::recombine(values, indices, x)
      L <- tf$reshape(values, shape(dims[1], dims[2]))
      tf$matmul(tf$transpose(L), L)
    },

    initialize = function (df, Sigma) {
      # add the nodes as children and parameters

      df <- ga(df)
      Sigma <- ga(Sigma)

      # check dimensions of Sigma
      if (nrow(Sigma) != ncol(Sigma) |
          length(dim(Sigma)) != 2) {

        stop ('Sigma must be a square 2D greta array, but has dimensions ',
              paste(dim(Sigma), collapse = ' x '))

      }

      dim <- nrow(Sigma)

      super$initialize('wishart', dim(Sigma))
      self$add_parameter(df, 'df')
      self$add_parameter(Sigma, 'Sigma')

      # make the initial value PD
      self$value(unknowns(dims = c(dim, dim), data = diag(dim)))

    },

    tf_log_density_function = function (x, parameters) {

      df <- parameters$df
      Sigma <- parameters$Sigma

      dist <- tf$contrib$distributions$WishartFull(df = df, scale = Sigma)
      tf$reshape(dist$log_prob(x), shape(1, 1))

    }

  )
)


# export constructors

#' @name greta-distributions
#' @title greta probability distributions
#' @description These functions can be used to define random variables in a
#'   greta model. They return a greta array object that can be combined with
#'   other greta arrays to construct a model. All of these functions construct
#'   random variables with prior distributions, except for \code{free()}, which
#'   creates 'free' parameters, so can be used for frequentist analyses.
#'
#' @param lower,upper scalar values giving optional limits to free
#'   parameters. These must be specified as numerics, they cannot be greta
#'   arrays (though see details for a workaround). They can be set to \code{-Inf} (\code{lower}) or \code{Inf}
#'   (\code{upper}), though \code{lower} must always be less than \code{upper}.
#'
#' @param min,max scalar values giving optional limits to \code{uniform}
#'   variables. Like \code{lower} and \code{upper}, these must be specified as
#'   numerics, they cannot be greta arrays (though see details for a
#'   workaround). Unlike \code{lower} and \code{upper}, they must be finite.
#'   \code{min} must always be less than \code{max}.
#'
#' @param mean,meanlog,location unconstrained parameters
#' @param sd,sdlog,size,lambda,shape,rate,df,scale,shape1,shape2 positive
#'   parameters
#' @param prob probability parameter (\code{0 < prob < 1})
#' @param Sigma positive definite variance-covariance matrix parameter
#'
#' @param dim the dimensions of the variable, either a scalar or a vector of
#'   positive integers. See details.
#'
#' @details The discrete probability distributions (\code{bernoulli}, \code{binomial},
#'   \code{negative_binomial}, \code{poisson}) can be used as likelihoods, but
#'   not as unknown variables.
#'
#'   For \code{free()}, \code{dim} gives the dimension of the greta array to
#'   create as a free parameter. All elements of that array will have the same
#'   constraints (\code{lower} and \code{upper}). For univariate distributions
#'   \code{dim} also gives the dimensions of the greta array to create. Each
#'   element of the greta array will be (independently) distributed according to
#'   the distribution. \code{dim} can also be left at its default of
#'   \code{NULL}, in which case the dimension will be detected from the
#'   dimensions of the parameters (provided they are compatible with one
#'   another).
#'
#'   For \code{multivariate_normal()}, \code{dim} must be a scalar giving the
#'   number of rows in the resulting greta array, each row being (independently)
#'   distributed according to the multivariate normal distribution. The number
#'   of columns will always be the dimension of the distribution, determined
#'   from the parameters specified. \code{wishart()} always returns a single
#'   square, 2D greta array, with dimension determined from the parameter
#'   \code{Sigma}.
#'
#'   The parameters of both \code{free} and \code{uniform} must be fixed, not
#'   greta variables. This ensures these values can always be transformed to a
#'   continuous scale to run the samplers efficiently. However, a hierarchical
#'   \code{uniform} or \code{free} parameter can always be created by defining a
#'   \code{free} or \code{uniform} variable constrained between 0 and 1, and then
#'   transforming it to the required scale. I.e. \code{min + u * (max - min)},
#'   where u is e.g. \code{uniform(0, 1)}. See below for an example.
#'
#'   Wherever possible, the parameterisation of these distributions matches the
#'   those in the \code{stats} package. E.g. for the parameterisation of
#'   \code{negative_binomial()}, see \code{\link{dnbinom}}. \code{student()} is an
#'   exception, since the \href{https://en.wikipedia.org/wiki/Student\%27s_t-distribution#In_terms_of_scaling_parameter_.CF.83.2C_or_.CF.832}{location-scale representation} we use is more useful,
#'   and widely used, for statistical modelling than the noncentral version
#'   implemented in \code{stats}
#'
#' @examples
#' # an unconstrained and prior-free parameter (e.g. for a frequentist model)
#' alpha = free()
#'
#' # positive prior-free parameter (could also do: sigma = exp(free()) )
#' sigma = free(lower = 0)
#'
#' # a prior-free parameter constrained to be less that -1
#' neg_alpha = free(upper = -1)
#'
#' # a prior-free parameter constrained to be between 0 and 1
#' psi = free(lower = 0, upper = 1)
#'
#' # a uniform parameter constrained to be between 0 and 1
#' phi = uniform(min = 0, max = 1)
#'
#' # create a hierarchical uniform, constrained between alpha and alpha + sigma,
#' eta = alpha + uniform(0, 1) * sigma
#'
#' # an unconstrained parameter with standard normal prior
#' mu = normal(0, 1)
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
#' # a multivariate normal variable, with correlation between two elements
#' Sig <- diag(4)
#' Sig[3, 4] <- Sig[4, 3] <- 0.6
#' theta = multivariate_normal(rep(mu, 4), Sig)
#'
#' # 10 independent replicates of that
#' theta = multivariate_normal(rep(mu, 4), Sig, dim = 10)
#'
#' # a Wishart variable with the same covariance parameter
#' theta = wishart(df = 5, Sigma = Sig)
NULL

#' @rdname greta-distributions
#' @export
free <- function (lower = -Inf, upper = Inf, dim = 1) {

  if (is.greta_array(lower) | is.greta_array(upper))
    stop ('lower and upper must be fixed, they cannot be another greta array')

  ga(free_distribution$new(lower, upper, dim))

}

#' @rdname greta-distributions
#' @export
uniform <- function (min, max, dim = NULL) {

  if (is.greta_array(min) | is.greta_array(max))
    stop ('min and max must be fixed, they cannot be another greta array')

  ga(uniform_distribution$new(min, max, dim))

}

#' @rdname greta-distributions
#' @export
normal <- function (mean, sd, dim = NULL)
  ga(normal_distribution$new(mean, sd, dim))

#' @rdname greta-distributions
#' @export
lognormal <- function (meanlog, sdlog, dim = NULL)
  ga(lognormal_distribution$new(meanlog, sdlog, dim))

#' @rdname greta-distributions
#' @export
bernoulli <- function (prob, dim = NULL)
  ga(bernoulli_distribution$new(prob, dim))

#' @rdname greta-distributions
#' @export
binomial <- function (size, prob, dim = NULL)
  ga(binomial_distribution$new(size, prob, dim))

#' @rdname greta-distributions
#' @export
negative_binomial <- function (size, prob, dim = NULL)
  ga(negative_binomial_distribution$new(size, prob, dim))

#' @rdname greta-distributions
#' @export
poisson <- function (lambda, dim = NULL)
  ga(poisson_distribution$new(lambda, dim))

#' @rdname greta-distributions
#' @export
gamma <- function (shape, rate, dim = NULL)
  ga(gamma_distribution$new(shape, rate, dim))

#' @rdname greta-distributions
#' @export
exponential <- function (rate, dim = NULL)
  ga(exponential_distribution$new(rate, dim))

#' @rdname greta-distributions
#' @export
student <- function (df, location, scale, dim = NULL)
  ga(student_distribution$new(df, location, scale, dim))

#' @rdname greta-distributions
#' @export
beta <- function (shape1, shape2, dim = NULL)
  ga(beta_distribution$new(shape1, shape2, dim))

#' @rdname greta-distributions
#' @export
multivariate_normal <- function (mean, Sigma, dim = 1)
  ga(multivariate_normal_distribution$new(mean, Sigma, dim))

#' @rdname greta-distributions
#' @export
wishart <- function (df, Sigma)
  ga(wishart_distribution$new(df, Sigma))
