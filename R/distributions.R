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
      self$log_density <- tf$constant(-log(max - min))

    },

    # default value
    create_target = function() {
      variable(lower = self$min,
               upper = self$max,
               dim = self$dim)
    },

    # weird hack to make TF see a gradient here
    tf_log_density_function = function (x, parameters) {
      self$log_density + tf$reduce_sum(x * 0)
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

    tf_log_density_function = function (x, parameters) {

      norm <- tf$contrib$distributions$Normal(loc = parameters$mean,
                                              scale = parameters$sd)
      norm$log_prob(x)

    },

    tf_cdf_function = function (x, parameters) {

      norm <- tf$contrib$distributions$Normal(loc = parameters$mean,
                                              scale = parameters$sd)
      norm$cdf(x)

    },

    tf_log_cdf_function = function (x, parameters) {

      norm <- tf$contrib$distributions$Normal(loc = parameters$mean,
                                              scale = parameters$sd)
      norm$log_cdf(x)

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

    tf_log_density_function = function (x, parameters) {

      prob <- parameters$prob

      # optionally reshape prob
      prob_shape <- prob$get_shape()$as_list()
      x_shape <- x$get_shape()$as_list()

      if (identical(prob_shape, c(1L, 1L)) & !identical(x_shape, c(1L, 1L)))
        prob <- tf$tile(prob, x_shape)

      tf$log(tf$where(tf$equal(x, 1), prob, 1 - prob))

    }

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

    tf_log_density_function = function (x, parameters) {

      lambda <- parameters$lambda
      x * tf$log(lambda) - lambda - tf$lgamma(x + 1)

    }

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

    tf_log_density_function = function (x, parameters) {

      rate <- parameters$rate
      tf$log(rate) - rate * x

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

    tf_log_density_function = function (x, parameters) {

      df <- parameters$df
      Sigma <- parameters$Sigma

      dist <- tf$contrib$distributions$WishartFull(df = df, scale = Sigma)
      tf$reshape(dist$log_prob(x), shape(1, 1))

    }

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
multivariate_normal <- function (mean, Sigma, dim = 1)
  distrib('multivariate_normal', mean, Sigma, dim)

#' @rdname greta-distributions
#' @export
wishart <- function (df, Sigma)
  distrib('wishart', df, Sigma)
