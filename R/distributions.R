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
      variable(lower = self$min,
               upper = self$max,
               dim = self$dim)
    },

    tf_distrib = function (parameters) {
      tf$contrib$distributions$Uniform(low = parameters$min,
                                       high = parameters$max)
    },

    # weird hack to make TF see a gradient here
    tf_log_density_function = function (x, parameters) {
      self$log_density + x * 0
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
      variable(dim = self$dim)
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
      variable(lower = 0, dim = self$dim)
    },

    tf_distrib = function (parameters) {

      mean <- parameters$meanlog
      sd <- parameters$sdlog
      var <- tf$square(sd)

      log_prob = function (x) {
        lx <- tf$log(x)
        -1 * (lx + tf$log(sd) + 0.9189385) +
          -0.5 * tf$square(tf$subtract(lx, mean)) / var
      }

      cdf = function (x) {
        lx <- tf$log(x)
        0.5 + 0.5 * tf$erf((lx - mean) / (sqrt(2) * sd))
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
      variable(dim = self$dim)
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
      variable(dim = self$dim)
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
      variable(dim = self$dim)
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
      variable(dim = self$dim)
    },

    tf_distrib = function (parameters) {
      tf$contrib$distributions$NegativeBinomial(total_count = parameters$size,
                                                probs = 1 - parameters$prob)
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
      variable(lower = 0, dim = self$dim)
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

    initialize = function (shape, scale, dim) {
      # add the nodes as children and parameters
      dim <- check_dims(shape, scale, target_dim = dim)
      super$initialize('inverse_gamma', dim)
      self$add_parameter(shape, 'shape')
      self$add_parameter(scale, 'scale')
    },

    # default value
    create_target = function() {
      variable(lower = 0, dim = self$dim)
    },

    tf_distrib = function (parameters) {
      tf$contrib$distributions$InverseGamma(concentration = parameters$shape,
                                            rate = parameters$scale)
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
      variable(lower = 0, dim = self$dim)
    },

    tf_distrib = function (parameters) {

      a <- parameters$shape
      b <- parameters$scale

      log_prob = function (x) {
        log(a) - log(b) + (a - 1) * (log(x) - log(b)) - (x / b) ^ a
      }

      cdf = function (x) {
        1 - exp(-(x / b) ^ a)
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
      variable(lower = 0, dim = self$dim)
    },

    tf_distrib = function (parameters) {
      tf$contrib$distributions$Exponential(rate = parameters$rate)
    }

  )
)

student_distribution <- R6Class (
  'student_distribution',
  inherit = distribution_node,
  public = list(

    initialize = function (df, location, scale, dim) {
      # add the nodes as children and parameters
      dim <- check_dims(df, location, scale, target_dim = dim)
      super$initialize('student', dim)
      self$add_parameter(df, 'df')
      self$add_parameter(location, 'location')
      self$add_parameter(scale, 'scale')
    },

    # default value
    create_target = function() {
      variable(dim = self$dim)
    },

    tf_distrib = function (parameters) {
      tf$contrib$distributions$StudentT(df = parameters$df,
                                        loc = parameters$location,
                                        scale = parameters$scale)
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
      variable(lower = 0, upper = 1, dim = self$dim)
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
      variable(dim = self$dim)
    },

    tf_distrib = function (parameters) {

      loc <- parameters$location
      s <- parameters$scale

      log_prob = function (x)
        -tf$log(pi * s * (1 + ((x - loc) / s) ^ 2))

      cdf = function (x)
        (1 / pi)  * tf$atan((x - loc) / s) + 0.5

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
      variable(lower = 0, dim = self$dim)
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
      variable(dim = self$dim)
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
      variable(dim = self$dim)
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
      free_greta_array <- free(dim = prod(self$dim))
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
#' @details The discrete probability distributions (\code{bernoulli},
#'   \code{binomial}, \code{negative_binomial}, \code{poisson}) can be used when
#'   they have fixed values (e.g. defined as a likelihood using
#'   \code{\link{distribution}}, but not as unknown variables.
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
#'   Wherever possible, the parameterisation and argument names of greta
#'   distributions matches commonly used R functions for distributions, such as
#'   those in the \code{stats} package.\code{student()} is an exception, since
#'   the
#'   \href{https://en.wikipedia.org/wiki/Student\%27s_t-distribution#In_terms_of_scaling_parameter_.CF.83.2C_or_.CF.832}{location-scale
#'   representation} we use is more useful, and widely used, for statistical
#'   modelling than the noncentral version implemented in \code{stats}. The
#'   following table states the distribution function to which greta's
#'   implementation corresponds:
#'
#'   \tabular{ll}{
#'   greta \tab reference\cr
#'   \code{uniform} \tab \code{\link[stats:dunif]{stats::dunif}}\cr
#'   \code{normal} \tab \code{\link[stats:dnorm]{stats::dnorm}}\cr
#'   \code{lognormal} \tab \code{\link[stats:dlnorm]{stats::dlnorm}}\cr
#'   \code{bernoulli} \tab {\code{\link[stats:dbinom]{stats::dbinom}} (n = 1)}\cr
#'   \code{binomial} \tab \code{\link[stats:dbinom]{stats::dbinom}}\cr
#'   \code{negative_binomial} \tab \code{\link[stats:dnbinom]{stats::dnbinom}}\cr
#'   \code{poisson} \tab \code{\link[stats:dpois]{stats::dpois}}\cr
#'   \code{gamma} \tab \code{\link[stats:dgamma]{stats::dgamma}}\cr
#'   \code{inverse_gamma} \tab \code{\link[MCMCpack:dinvgamma]{MCMCpack::dinvgamma}}\cr
#'   \code{weibull} \tab \code{\link[stats:dweibull]{stats::dweibull}}\cr
#'   \code{exponential} \tab \code{\link[stats:dexp]{stats::dexp}}\cr
#'   \code{student} \tab \href{https://en.wikipedia.org/wiki/Student\%27s_t-distribution#In_terms_of_scaling_parameter_.CF.83.2C_or_.CF.832}{wikipedia}\cr
#'   \code{beta} \tab \code{\link[stats:dbeta]{stats::dbeta}}\cr
#'   \code{cauchy} \tab \code{\link[stats:dcauchy]{stats::dcauchy}}\cr
#'   \code{chi_squared} \tab \code{\link[stats:dchisq]{stats::dchisq}}\cr
#'   \code{logistic} \tab \code{\link[stats:dlogis]{stats::dlogis}}\cr
#'   \code{multivariate_normal} \tab \code{\link[mvtnorm:dmvnorm]{mvtnorm::dmvnorm}}\cr
#'   \code{wishart} \tab \code{\link[MCMCpack:dwish]{MCMCpack::dwish}}\cr
#'   }
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

  node <- variable_node$new(lower, upper, dim)
  as.greta_array(node)

}

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
inverse_gamma <- function (shape, scale, dim = NULL)
  distrib('inverse_gamma', shape, scale, dim)

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
student <- function (df, location, scale, dim = NULL)
  distrib('student', df, location, scale, dim)

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
multivariate_normal <- function (mean, Sigma, dim = 1)
  distrib('multivariate_normal', mean, Sigma, dim)

#' @rdname greta-distributions
#' @export
wishart <- function (df, Sigma)
  distrib('wishart', df, Sigma)
