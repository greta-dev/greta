uniform_distribution <- R6Class (
  'uniform_distribution',
  inherit = distribution_node,
  public = list(

    min = NA,
    max = NA,
    log_density = NULL,

    initialize = function (min, max, dim) {

      good_types <- is.numeric(min) && length(min) == 1 &
        is.numeric(max) && length(max) == 1

      if (!good_types) {

        stop ('min and max must be numeric vectors of length 1',
              call. = FALSE)

      }

      if (!is.finite(min) | !is.finite(max)) {

        stop ('min and max must finite scalars',
              call. = FALSE)

      }

      if (min >= max) {

        stop ('max must be greater than min',
              call. = FALSE)

      }

      # store min and max as numeric scalars (needed in create_target, done in
      # initialisation)
      self$min <- min
      self$max <- max

      # initialize the rest
      super$initialize('uniform', dim)

      # add them as children and greta arrays
      self$add_parameter(min, 'min')
      self$add_parameter(max, 'max')

      # the density is fixed, so calculate it now
      self$log_density <- -log(max - min)

    },

    # default value
    create_target = function() {
      vble(lower = self$min,
               upper = self$max,
               dim = self$dim)
    },

    tf_distrib = function (parameters) {
      tf$contrib$distributions$Uniform(low = parameters$min,
                                       high = parameters$max)
    },

    # weird hack to make TF see a gradient here
    tf_log_density_function = function (x, parameters) {
      fl(self$log_density) + x * fl(0)
    }

  )
)


normal_distribution <- R6Class (
  'normal_distribution',
  inherit = distribution_node,
  public = list(

    initialize = function (mean, sd, dim) {
      # add the nodes as children and parameters
      dim <- check_dims(mean, sd, target_dim = dim)
      super$initialize('normal', dim)
      self$add_parameter(mean, 'mean')
      self$add_parameter(sd, 'sd')
    },

    # default value
    create_target = function() {
      vble(dim = self$dim)
    },

    tf_distrib = function (parameters) {
      tf$contrib$distributions$Normal(loc = parameters$mean,
                                      scale = parameters$sd)
    }

  )
)

lognormal_distribution <- R6Class (
  'lognormal_distribution',
  inherit = distribution_node,
  public = list(

    initialize = function (meanlog, sdlog, dim) {
      dim <- check_dims(meanlog, sdlog, target_dim = dim)
      super$initialize('lognormal', dim)
      self$add_parameter(meanlog, 'meanlog')
      self$add_parameter(sdlog, 'sdlog')
    },

    # default value
    create_target = function() {
      vble(lower = 0, dim = self$dim)
    },

    tf_distrib = function (parameters) {

      mean <- parameters$meanlog
      sd <- parameters$sdlog
      var <- tf$square(sd)

      log_prob = function (x) {
        lx <- tf$log(x)
        fl(-1) * (lx + tf$log(sd) + fl(0.9189385)) +
          fl(-0.5) * tf$square(tf$subtract(lx, mean)) / var
      }

      cdf = function (x) {
        lx <- tf$log(x)
        fl(0.5) + fl(0.5) * tf$erf((lx - mean) / (fl(sqrt(2)) * sd))
      }

      log_cdf = function (x) {
        log(cdf(x))
      }

      list(log_prob = log_prob, cdf = cdf, log_cdf = log_cdf)

    }

  )
)

bernoulli_distribution <- R6Class (
  'bernoulli_distribution',
  inherit = distribution_node,
  public = list(

    initialize = function (prob, dim) {
      # add the nodes as children and parameters
      dim <- check_dims(prob, target_dim = dim)
      super$initialize('bernoulli', dim, discrete = TRUE)
      self$add_parameter(prob, 'prob')
    },

    # default value (should get overwritten anyway!)
    create_target = function() {
      vble(dim = self$dim)
    },

    tf_distrib = function (parameters) {
      tf$contrib$distributions$Bernoulli(probs = parameters$prob)
    },

    # no CDF for discrete distributions
    tf_cdf_function = NULL,
    tf_log_cdf_function = NULL


  )
)

binomial_distribution <- R6Class (
  'binomial_distribution',
  inherit = distribution_node,
  public = list(

    initialize = function (size, prob, dim) {
      # add the nodes as children and parameters
      dim <- check_dims(size, prob, target_dim = dim)
      super$initialize('binomial', dim, discrete = TRUE)
      self$add_parameter(size, 'size')
      self$add_parameter(prob, 'prob')

    },

    # default value (should get overwritten anyway!)
    create_target = function() {
      vble(dim = self$dim)
    },

    tf_distrib = function (parameters) {
      tf$contrib$distributions$Binomial(total_count = parameters$size,
                                        probs = parameters$prob)
    },

    # no CDF for discrete distributions
    tf_cdf_function = NULL,
    tf_log_cdf_function = NULL

  )
)

poisson_distribution <- R6Class (
  'poisson_distribution',
  inherit = distribution_node,
  public = list(

    initialize = function (lambda, dim) {
      # add the nodes as children and parameters
      dim <- check_dims(lambda, target_dim = dim)
      super$initialize('poisson', dim, discrete = TRUE)
      self$add_parameter(lambda, 'lambda')
    },

    # default value (should get overwritten anyway!)
    create_target = function() {
      vble(dim = self$dim)
    },

    tf_distrib = function (parameters) {
      tf$contrib$distributions$Poisson(rate = parameters$lambda)
    },

    # no CDF for discrete distributions
    tf_cdf_function = NULL,
    tf_log_cdf_function = NULL

  )
)

negative_binomial_distribution <- R6Class (
  'negative_binomial_distribution',
  inherit = distribution_node,
  public = list(

    initialize = function (size, prob, dim) {
      # add the nodes as children and parameters
      dim <- check_dims(size, prob, target_dim = dim)
      super$initialize('negative_binomial', dim, discrete = TRUE)
      self$add_parameter(size, 'size')
      self$add_parameter(prob, 'prob')
    },

    # default value (should get overwritten anyway!)
    create_target = function() {
      vble(dim = self$dim)
    },

    tf_distrib = function (parameters) {
      tf$contrib$distributions$NegativeBinomial(total_count = parameters$size,
                                                probs = fl(1) - parameters$prob)
    },

    # no CDF for discrete distributions
    tf_cdf_function = NULL,
    tf_log_cdf_function = NULL

  )
)

gamma_distribution <- R6Class (
  'gamma_distribution',
  inherit = distribution_node,
  public = list(

    initialize = function (shape, rate, dim) {
      # add the nodes as children and parameters
      dim <- check_dims(shape, rate, target_dim = dim)
      super$initialize('gamma', dim)
      self$add_parameter(shape, 'shape')
      self$add_parameter(rate, 'rate')
    },

    # default value
    create_target = function() {
      vble(lower = 0, dim = self$dim)
    },


    tf_distrib = function (parameters) {
      tf$contrib$distributions$Gamma(concentration = parameters$shape,
                                     rate = parameters$rate)
    }

  )
)

inverse_gamma_distribution <- R6Class (
  'inverse_gamma_distribution',
  inherit = distribution_node,
  public = list(

    initialize = function (alpha, beta, dim) {
      # add the nodes as children and parameters
      dim <- check_dims(alpha, beta, target_dim = dim)
      super$initialize('inverse_gamma', dim)
      self$add_parameter(alpha, 'alpha')
      self$add_parameter(beta, 'beta')
    },

    # default value
    create_target = function() {
      vble(lower = 0, dim = self$dim)
    },

    tf_distrib = function (parameters) {
      tf$contrib$distributions$InverseGamma(concentration = parameters$alpha,
                                            rate = parameters$beta)
    }

  )
)


weibull_distribution <- R6Class (
  'weibull_distribution',
  inherit = distribution_node,
  public = list(

    initialize = function (shape, scale, dim) {
      # add the nodes as children and parameters
      dim <- check_dims(shape, scale, target_dim = dim)
      super$initialize('weibull', dim)
      self$add_parameter(shape, 'shape')
      self$add_parameter(scale, 'scale')
    },

    # default value
    create_target = function() {
      vble(lower = 0, dim = self$dim)
    },

    tf_distrib = function (parameters) {

      a <- parameters$shape
      b <- parameters$scale

      log_prob = function (x) {
        log(a) - log(b) + (a - fl(1)) * (log(x) - log(b)) - (x / b) ^ a
      }

      cdf = function (x) {
        fl(1) - exp(-(x / b) ^ a)
      }

      log_cdf = function (x) {
        log(cdf(x))
      }

      list(log_prob = log_prob, cdf = cdf, log_cdf = log_cdf)

    }

  )
)

exponential_distribution <- R6Class (
  'exponential_distribution',
  inherit = distribution_node,
  public = list(

    initialize = function (rate, dim) {
      # add the nodes as children and parameters
      dim <- check_dims(rate, target_dim = dim)
      super$initialize('exponential', dim)
      self$add_parameter(rate, 'rate')
    },

    # default value
    create_target = function() {
      vble(lower = 0, dim = self$dim)
    },

    tf_distrib = function (parameters) {
      tf$contrib$distributions$Exponential(rate = parameters$rate)
    }

  )
)

pareto_distribution <- R6Class (
  'pareto_distribution',
  inherit = distribution_node,
  public = list(

    initialize = function (a, b, dim) {
      # add the nodes as children and parameters
      dim <- check_dims(a, b, target_dim = dim)
      super$initialize('pareto', dim)
      self$add_parameter(a, 'a')
      self$add_parameter(b, 'b')
    },

    # default value
    create_target = function() {
      vble(lower = 0, dim = self$dim)
    },

    tf_distrib = function (parameters) {

      a <- parameters$a
      b <- parameters$b

      log_prob <- function (x)
        log(a) + a * log(b) - (a + 1) * log(x)

      cdf <- function (x)
        1 - (b / x) ^ a

      log_cdf <- function (x)
        log(cdf(x))

      list(log_prob = log_prob, cdf = cdf, log_cdf = log_cdf)

    }

  )
)

student_distribution <- R6Class (
  'student_distribution',
  inherit = distribution_node,
  public = list(

    initialize = function (df, mu, sigma, dim) {
      # add the nodes as children and parameters
      dim <- check_dims(df, mu, sigma, target_dim = dim)
      super$initialize('student', dim)
      self$add_parameter(df, 'df')
      self$add_parameter(mu, 'mu')
      self$add_parameter(sigma, 'sigma')
    },

    # default value
    create_target = function() {
      vble(dim = self$dim)
    },

    tf_distrib = function (parameters) {
      tf$contrib$distributions$StudentT(df = parameters$df,
                                        loc = parameters$mu,
                                        scale = parameters$sigma)
    }

  )
)

laplace_distribution <- R6Class (
  'laplace_distribution',
  inherit = distribution_node,
  public = list(

    initialize = function (mu, sigma, dim) {
      # add the nodes as children and parameters
      dim <- check_dims(mu, sigma, target_dim = dim)
      super$initialize('laplace', dim)
      self$add_parameter(mu, 'mu')
      self$add_parameter(sigma, 'sigma')
    },

    # default value
    create_target = function() {
      vble(dim = self$dim)
    },

    tf_distrib = function (parameters) {
      tf$contrib$distributions$Laplace(loc = parameters$mu,
                                       scale = parameters$sigma)
    }

  )
)

beta_distribution <- R6Class (
  'beta_distribution',
  inherit = distribution_node,
  public = list(

    initialize = function (shape1, shape2, dim) {
      # add the nodes as children and parameters
      dim <- check_dims(shape1, shape2, target_dim = dim)
      super$initialize('beta', dim)
      self$add_parameter(shape1, 'shape1')
      self$add_parameter(shape2, 'shape2')
    },

    # default value
    create_target = function() {
      vble(lower = 0, upper = 1, dim = self$dim)
    },

    tf_distrib = function (parameters) {
      tf$contrib$distributions$Beta(concentration1 = parameters$shape1,
                                    concentration0 = parameters$shape2)
    }

  )
)

cauchy_distribution <- R6Class (
  'cauchy_distribution',
  inherit = distribution_node,
  public = list(

    initialize = function (location, scale, dim) {
      # add the nodes as children and parameters
      dim <- check_dims(location, scale, target_dim = dim)
      super$initialize('cauchy', dim)
      self$add_parameter(location, 'location')
      self$add_parameter(scale, 'scale')
    },

    # default value
    create_target = function() {
      vble(dim = self$dim)
    },

    tf_distrib = function (parameters) {

      loc <- parameters$location
      s <- parameters$scale

      log_prob = function (x)
        -tf$log(fl(pi) * s * (fl(1) + tf$square((x - loc) / s)))

      cdf = function (x)
        fl(1 / pi)  * tf$atan((x - loc) / s) + fl(0.5)

      log_cdf = function (x)
        tf$log(cdf(x))

      list(log_prob = log_prob, cdf = cdf, log_cdf = log_cdf)

    }

  )
)

chi_squared_distribution <- R6Class (
  'chi_squared_distribution',
  inherit = distribution_node,
  public = list(

    initialize = function (df, dim) {
      # add the nodes as children and parameters
      dim <- check_dims(df, target_dim = dim)
      super$initialize('chi_squared', dim)
      self$add_parameter(df, 'df')
    },

    # default value
    create_target = function() {
      vble(lower = 0, dim = self$dim)
    },

    tf_distrib = function (parameters) {
      tf$contrib$distributions$Chi2(df = parameters$df)
    }

  )
)


logistic_distribution <- R6Class (
  'logistic_distribution',
  inherit = distribution_node,
  public = list(

    initialize = function (location, scale, dim) {
      # add the nodes as children and parameters
      dim <- check_dims(location, scale, target_dim = dim)
      super$initialize('logistic', dim)
      self$add_parameter(location, 'location')
      self$add_parameter(scale, 'scale')
    },

    # default value
    create_target = function() {
      vble(dim = self$dim)
    },

    tf_distrib = function (parameters) {
      tf$contrib$distributions$Logistic(loc = parameters$location,
                                        scale = parameters$scale)
    },

    # log_cdf in tf$cotrib$distributions has the wrong sign :/
    tf_log_cdf_function = function (x, parameters) {
      tf$log(self$tf_cdf_function(x, parameters))
    }

  )
)

f_distribution <- R6Class (
  'f_distribution',
  inherit = distribution_node,
  public = list(

    initialize = function (df1, df2, dim) {
      # add the nodes as children and parameters
      dim <- check_dims(df1, df2, target_dim = dim)
      super$initialize('d', dim)
      self$add_parameter(df1, 'df1')
      self$add_parameter(df2, 'df2')
    },

    # default value
    create_target = function() {
      vble(lower = 0, dim = self$dim)
    },

    tf_distrib = function (parameters) {

      df1 <- parameters$df1
      df2 <- parameters$df2

      tf_lbeta <- function(a, b)
        tf$lgamma(a) + tf$lgamma(b) - tf$lgamma(a + b)

      log_prob = function (x) {
        df1_x <- df1 * x
        la <- df1 * log(df1_x) + df2 * log(df2)
        lb <- (df1 + df2) * log(df1_x + df2)
        lnumerator <- fl(0.5) * (la - lb)
        lnumerator - log(x) - tf_lbeta(df1 / fl(2), df2 / fl(2))
      }

      cdf = function (x) {
        df1_x <- df1 * x
        ratio <- df1_x / (df1_x + df2)
        tf$betainc(df1 / fl(2), df2 / fl(2), ratio)
      }

      log_cdf = function(x)
        log(cdf(x))

      list(log_prob = log_prob, cdf = cdf, log_cdf = log_cdf)

    }

  )
)

# need to add checking of mean and Sigma dimensions
multinomial_distribution <- R6Class (
  'multinomial_distribution',
  inherit = distribution_node,
  public = list(

    initialize = function (size, prob, dim) {

      # coerce to greta arrays
      size <- as.greta_array(size)
      prob <- as.greta_array(prob)

      # check dimensions of prob
      if (length(size) != 1) {

        stop ('size must be a scalar, but has dimensions ',
              paste(dim(size), collapse = ' x '),
              call. = FALSE)

      }

      # check dimensions of prob
      if (ncol(prob) != 1 |
          length(dim(prob)) != 2) {

        stop ('prob must be a 2D greta array with one column, but has dimensions ',
              paste(dim(prob), collapse = ' x '),
              call. = FALSE)

      }

      if (length(prob) == 1) {

        stop ('the multinomial distribution is for vectors, ',
              'but the parameters were scalar',
              call. = FALSE)

      }

      # check dim is a positive scalar integer
      dim_old <- dim
      dim <- as.integer(dim)
      if (length(dim) > 1 || dim <= 0 || !is.finite(dim)) {

        stop ('dim must be a scalar positive integer, but was: ',
              capture.output(dput(dim_old)),
              call. = FALSE)

      }

      # coerce the parameter arguments to nodes and add as children and
      # parameters
      super$initialize('multinomial', c(dim, length(prob)))
      self$add_parameter(size, 'size')
      self$add_parameter(prob, 'prob')

    },

    # default value
    create_target = function() {
      vble(dim = self$dim)
    },

    tf_distrib = function (parameters) {
      # transpose and scale probs to get absolute density correct
      probs <- tf$transpose(parameters$prob)
      probs <- probs / tf$reduce_sum(probs)
      tf$contrib$distributions$Multinomial(total_count = parameters$size,
                                           probs = probs)
    },

    # no CDF for multivariate distributions
    tf_cdf_function = NULL,
    tf_log_cdf_function = NULL

  )
)

# need to add checking of mean and Sigma dimensions
categorical_distribution <- R6Class (
  'categorical_distribution',
  inherit = distribution_node,
  public = list(

    initialize = function (prob, dim) {

      # coerce to greta arrays
      prob <- as.greta_array(prob)

      # check dimensions of prob
      if (ncol(prob) != 1 |
          length(dim(prob)) != 2) {

        stop ('prob must be a 2D greta array with one column, but has dimensions ',
              paste(dim(prob), collapse = ' x '),
              call. = FALSE)

      }

      if (length(prob) == 1) {

        stop ('the categorical distribution is for vectors, ',
              'but the parameters were scalar',
              call. = FALSE)

      }

      # check dim is a positive scalar integer
      dim_old <- dim
      dim <- as.integer(dim)
      if (length(dim) > 1 || dim <= 0 || !is.finite(dim)) {

        stop ('dim must be a scalar positive integer, but was: ',
              capture.output(dput(dim_old)),
              call. = FALSE)

      }

      # coerce the parameter arguments to nodes and add as children and
      # parameters
      super$initialize('categorical', c(dim, length(prob)))
      self$add_parameter(prob, 'prob')

    },

    # default value
    create_target = function() {
      vble(dim = self$dim)
    },

    tf_distrib = function (parameters) {
      # transpose and scale probs to get absolute density correct
      probs <- tf$transpose(parameters$prob)
      probs <- probs / tf$reduce_sum(probs)
      tf$contrib$distributions$Multinomial(total_count = fl(1),
                                           probs = probs)
    },

    # no CDF for multivariate distributions
    tf_cdf_function = NULL,
    tf_log_cdf_function = NULL

  )
)

# need to add checking of mean and Sigma dimensions
multivariate_normal_distribution <- R6Class (
  'multivariate_normal_distribution',
  inherit = distribution_node,
  public = list(

    initialize = function (mean, Sigma, dim) {

      # coerce to greta arrays
      mean <- as.greta_array(mean)
      Sigma <- as.greta_array(Sigma)

      # check dimensions of mean
      if (ncol(mean) != 1 |
          length(dim(mean)) != 2) {

        stop ('mean must be a 2D greta array with one column, but has dimensions ',
              paste(dim(mean), collapse = ' x '),
              call. = FALSE)

      }

      # check dimensions of Sigma
      if (nrow(Sigma) != ncol(Sigma) |
          length(dim(Sigma)) != 2) {

        stop ('Sigma must be a square 2D greta array, but has dimensions ',
              paste(dim(Sigma), collapse = ' x '),
              call. = FALSE)

      }

      # compare possible dimensions
      dim_mean <- nrow(mean)
      dim_Sigma <- nrow(Sigma)

      if (dim_mean != dim_Sigma) {

        stop ('mean and Sigma have different dimensions, ',
              dim_mean, ' vs ', dim_Sigma,
              call. = FALSE)

      }

      if (dim_mean == 1) {

        stop ('the multivariate normal distribution is for vectors, ',
              'but the parameters were scalar',
              call. = FALSE)

      }

      # check dim is a positive scalar integer
      dim_old <- dim
      dim <- as.integer(dim)
      if (length(dim) > 1 || dim <= 0 || !is.finite(dim)) {

        stop ('dim must be a scalar positive integer, but was: ',
              capture.output(dput(dim_old)),
              call. = FALSE)

      }

      # coerce the parameter arguments to nodes and add as children and
      # parameters
      super$initialize('multivariate_normal', c(dim, dim_mean))
      self$add_parameter(mean, 'mean')
      self$add_parameter(Sigma, 'Sigma')

    },

    # default value
    create_target = function() {
      vble(dim = self$dim)
    },

    tf_distrib = function (parameters) {
      L <- tf$cholesky(parameters$Sigma)
      mu = tf$transpose(parameters$mean)
      tf$contrib$distributions$MultivariateNormalTriL(loc = mu,
                                                      scale_tril = L)
    },

    # no CDF for multivariate distributions
    tf_cdf_function = NULL,
    tf_log_cdf_function = NULL

  )
)

# need to add checking of mean and Sigma dimensions
wishart_distribution <- R6Class (
  'wishart_distribution',
  inherit = distribution_node,
  public = list(

    initialize = function (df, Sigma) {
      # add the nodes as children and parameters

      df <- as.greta_array(df)
      Sigma <- as.greta_array(Sigma)

      # check dimensions of Sigma
      if (nrow(Sigma) != ncol(Sigma) |
          length(dim(Sigma)) != 2) {

        stop ('Sigma must be a square 2D greta array, but has dimensions ',
              paste(dim(Sigma), collapse = ' x '),
              call. = FALSE)

      }

      dim <- nrow(Sigma)

      super$initialize('wishart', dim(Sigma))
      self$add_parameter(df, 'df')
      self$add_parameter(Sigma, 'Sigma')

      # make the initial value PD
      self$value(unknowns(dims = c(dim, dim), data = diag(dim)))

    },

    # default value
    create_target = function() {

      # handle reshaping via a greta array
      free_greta_array <- vble(dim = prod(self$dim))
      matrix_greta_array <- flat_to_symmetric(free_greta_array, self$dim)
      matrix_greta_array$node

    },

    tf_distrib = function (parameters) {
      tf$contrib$distributions$WishartFull(df = parameters$df,
                                           scale = parameters$Sigma)
    },

    tf_log_density_function = function (x, parameters) {
      lp <- self$tf_distrib(parameters)$log_prob(x)
      tf$reshape(lp, shape(1, 1))
    },

    # no CDF for multivariate distributions
    tf_cdf_function = NULL,
    tf_log_cdf_function = NULL

  )
)


# shorthand for distribution parameter constructors
distrib <- function (distribution, ...) {

  # get and initialize the distribution, with a default value node
  constructor <- get(paste0(distribution, '_distribution'))
  distrib <- constructor$new(...)

  # return the value node as a greta array
  value <- distrib$target
  as.greta_array(value)

}


# export constructors

#' @name greta-distributions
#' @title greta probability distributions
#' @description These functions can be used to define random variables in a
#'   greta model. They return a variable greta array that follows the specified
#'   distribution. This variable greta array can be used to represent a
#'   parameter with  prior distribution, or used with \code{\link{distribution}}
#'   to define a distribution over an existing greta array.
#'
#' @param min,max scalar values giving optional limits to \code{uniform}
#'   variables. Like \code{lower} and \code{upper}, these must be specified as
#'   numerics, they cannot be greta arrays (though see details for a
#'   workaround). Unlike \code{lower} and \code{upper}, they must be finite.
#'   \code{min} must always be less than \code{max}.
#'
#' @param mean,meanlog,location unconstrained parameters
#' @param sd,sdlog,size,lambda,shape,rate,df,scale,shape1,shape2,df1,df2,a,b
#'   positive parameters
#' @param prob probability parameter (\code{0 < prob < 1}), must be a vector for
#'   \code{multinomial} and \code{categorical}
#' @param Sigma positive definite variance-covariance matrix parameter
#'
#' @param dim the dimensions of the greta array to be returned, either a scalar
#'   or a vector of positive integers. See details.
#'
#' @details The discrete probability distributions (\code{bernoulli},
#'   \code{binomial}, \code{negative_binomial}, \code{poisson}) can be used when
#'   they have fixed values (e.g. defined as a likelihood using
#'   \code{\link{distribution}}, but not as unknown variables.
#'
#'   For univariate distributions \code{dim} gives the dimensions of the greta
#'   array to create. Each element of the greta array will be (independently)
#'   distributed according to the distribution. \code{dim} can also be left at
#'   its default of \code{NULL}, in which case the dimension will be detected
#'   from the dimensions of the parameters (provided they are compatible with
#'   one another).
#'
#'   For \code{multivariate_normal()}, \code{multinomial()}, and
#'   \code{categorical()} \code{dim} must be a scalar giving the number of rows
#'   in the resulting greta array, each row being (independently) distributed
#'   according to the multivariate normal distribution. The number of columns
#'   will always be the dimension of the distribution, determined from the
#'   parameters specified. \code{wishart()} always returns a single square, 2D
#'   greta array, with dimension determined from the parameter \code{Sigma}.
#'
#'   \code{multinomial()} does not check that observed values sum to
#'   \code{size}, and \code{categorical()} does not check that only one of the
#'   observed entries is 1. It's the user's responsibility to check their data
#'   matches the distribution!
#'
#'   The parameters of \code{uniform} must be fixed, not greta arrays. This
#'   ensures these values can always be transformed to a continuous scale to run
#'   the samplers efficiently. However, a hierarchical \code{uniform} parameter
#'   can always be created by defining a \code{uniform} variable constrained
#'   between 0 and 1, and then transforming it to the required scale. See
#'   below for an example.
#'
#'   Wherever possible, the parameterisation and argument names of greta
#'   distributions matches commonly used R functions for distributions, such as
#'   those in the \code{stats} or \code{extraDistr} packages. The following
#'   table states the distribution function to which greta's implementation
#'   corresponds:
#'
#'   \tabular{ll}{ greta \tab reference\cr \code{uniform} \tab
#'   \code{\link[stats:dunif]{stats::dunif}}\cr \code{normal} \tab
#'   \code{\link[stats:dnorm]{stats::dnorm}}\cr \code{lognormal} \tab
#'   \code{\link[stats:dlnorm]{stats::dlnorm}}\cr \code{bernoulli} \tab
#'   \code{\link[extraDistr:dbern]{extraDistr::dbern}}\cr \code{binomial} \tab
#'   \code{\link[stats:dbinom]{stats::dbinom}}\cr \code{negative_binomial} \tab
#'   \code{\link[stats:dnbinom]{stats::dnbinom}}\cr \code{poisson} \tab
#'   \code{\link[stats:dpois]{stats::dpois}}\cr \code{gamma} \tab
#'   \code{\link[stats:dgamma]{stats::dgamma}}\cr \code{inverse_gamma} \tab
#'   \code{\link[extraDistr:dinvgamma]{extraDistr::dinvgamma}}\cr \code{weibull}
#'   \tab \code{\link[stats:dweibull]{stats::dweibull}}\cr \code{exponential}
#'   \tab \code{\link[stats:dexp]{stats::dexp}}\cr \code{pareto} \tab
#'   \code{\link[extraDistr:dpareto]{extraDistr::dpareto}}\cr \code{student}
#'   \tab \code{\link[extraDistr:dnst]{extraDistr::dnst}}\cr \code{laplace} \tab
#'   \code{\link[extraDistr:dlaplace]{extraDistr::dlaplace}}\cr \code{beta} \tab
#'   \code{\link[stats:dbeta]{stats::dbeta}}\cr \code{cauchy} \tab
#'   \code{\link[stats:dcauchy]{stats::dcauchy}}\cr \code{chi_squared} \tab
#'   \code{\link[stats:dchisq]{stats::dchisq}}\cr \code{logistic} \tab
#'   \code{\link[stats:dlogis]{stats::dlogis}}\cr \code{f} \tab
#'   \code{\link[stats:df]{stats::df}}\cr \code{multivariate_normal} \tab
#'   \code{\link[mvtnorm:dmvnorm]{mvtnorm::dmvnorm}}\cr \code{multinomial} \tab
#'   \code{\link[stats:dmultinom]{stats::dmultinom}}\cr \code{categorical} \tab
#'   {\code{\link[stats:dmultinom]{stats::dmultinom}} (size = 1)}\cr
#'   \code{wishart} \tab \code{\link[MCMCpack:dwish]{MCMCpack::dwish}}\cr }
#'
#' @examples
#' # a uniform parameter constrained to be between 0 and 1
#' phi = uniform(min = 0, max = 1)
#'
#' # a length-three variable, with each element following a standard normal
#' # distribution
#' alpha = normal(0, 1, dim = 3)
#'
#' # a length-three variable of lognormals
#' sigma = lognormal(0, 3, dim = 3)
#'
#' # a hierarchical uniform, constrained between alpha and alpha + sigma,
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
uniform <- function (min, max, dim = NULL) {

  if (is.greta_array(min) | is.greta_array(max))
    stop ('min and max must be fixed, they cannot be another greta array')

  distrib('uniform', min, max, dim)

}

#' @rdname greta-distributions
#' @export
normal <- function (mean, sd, dim = NULL)
  distrib('normal', mean, sd, dim)

#' @rdname greta-distributions
#' @export
lognormal <- function (meanlog, sdlog, dim = NULL)
  distrib('lognormal', meanlog, sdlog, dim)

#' @rdname greta-distributions
#' @export
bernoulli <- function (prob, dim = NULL)
  distrib('bernoulli', prob, dim)

#' @rdname greta-distributions
#' @export
binomial <- function (size, prob, dim = NULL)
  distrib('binomial', size, prob, dim)

#' @rdname greta-distributions
#' @export
negative_binomial <- function (size, prob, dim = NULL)
  distrib('negative_binomial', size, prob, dim)

#' @rdname greta-distributions
#' @export
poisson <- function (lambda, dim = NULL)
  distrib('poisson', lambda, dim)

#' @rdname greta-distributions
#' @export
gamma <- function (shape, rate, dim = NULL)
  distrib('gamma', shape, rate, dim)

#' @rdname greta-distributions
#' @export
inverse_gamma <- function (alpha, beta, dim = NULL)
  distrib('inverse_gamma', alpha, beta, dim)

#' @rdname greta-distributions
#' @export
weibull <- function (shape, scale, dim = NULL)
  distrib('weibull', shape, scale, dim)

#' @rdname greta-distributions
#' @export
exponential <- function (rate, dim = NULL)
  distrib('exponential', rate, dim)

#' @rdname greta-distributions
#' @export
pareto <- function (a, b, dim = NULL)
  distrib('pareto', a, b, dim)

#' @rdname greta-distributions
#' @export
student <- function (df, mu, sigma, dim = NULL)
  distrib('student', df, mu, sigma, dim)

#' @rdname greta-distributions
#' @export
laplace <- function (mu, sigma, dim = NULL)
  distrib('laplace', mu, sigma, dim)

#' @rdname greta-distributions
#' @export
beta <- function (shape1, shape2, dim = NULL)
  distrib('beta', shape1, shape2, dim)

#' @rdname greta-distributions
#' @export
cauchy <- function (location, scale, dim = NULL)
  distrib('cauchy', location, scale, dim)

#' @rdname greta-distributions
#' @export
chi_squared <- function (df, dim = NULL)
  distrib('chi_squared', df, dim)

#' @rdname greta-distributions
#' @export
logistic <- function (location, scale, dim = NULL)
  distrib('logistic', location, scale, dim)

#' @rdname greta-distributions
#' @export
f <- function (df1, df2, dim = NULL)
  distrib('f', df1, df2, dim)

#' @rdname greta-distributions
#' @export
multivariate_normal <- function (mean, Sigma, dim = 1)
  distrib('multivariate_normal', mean, Sigma, dim)

#' @rdname greta-distributions
#' @export
wishart <- function (df, Sigma)
  distrib('wishart', df, Sigma)

#' @rdname greta-distributions
#' @export
multinomial <- function (size, prob, dim = 1)
  distrib('multinomial', size, prob, dim)

#' @rdname greta-distributions
#' @export
categorical <- function (prob, dim = 1)
  distrib('categorical', prob, dim)
