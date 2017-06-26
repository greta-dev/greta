uniform_distribution <- R6Class (
  'uniform_distribution',
  inherit = distribution_node,
  public = list(

    min = NA,
    max = NA,
    log_density = NULL,

    initialize = function (min, max, dim) {

      if (is.greta_array(min) | is.greta_array(max))
        stop ('min and max must be fixed, they cannot be another greta array')

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

      self$bounds <- c(min, max)

      # initialize the rest
      super$initialize('uniform', dim)

      # add them as children and greta arrays
      self$add_parameter(min, 'min')
      self$add_parameter(max, 'max')

      # the density is fixed, so calculate it now
      self$log_density <- -log(max - min)

    },

    # default value (ignore any truncation arguments)
    create_target = function (...) {
      vble(truncation = c(self$min, self$max),
           dim = self$dim)
    },

    tf_distrib = function (parameters) {

      tf_ld <- fl(self$log_density)

      # weird hack to make TF see a gradient here
      log_prob = function (x)
        tf_ld + x * fl(0)

      list(log_prob = log_prob, cdf = NULL, log_cdf = NULL)

    }

  )
)

normal_distribution <- R6Class (
  'normal_distribution',
  inherit = distribution_node,
  public = list(

    initialize = function (mean, sd, dim, truncation) {
      # add the nodes as children and parameters
      dim <- check_dims(mean, sd, target_dim = dim)
      super$initialize('normal', dim, truncation)
      self$add_parameter(mean, 'mean')
      self$add_parameter(sd, 'sd')
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

    initialize = function (meanlog, sdlog, dim, truncation) {
      dim <- check_dims(meanlog, sdlog, target_dim = dim)
      check_positive(truncation)
      self$bounds <- c(0, Inf)
      super$initialize('lognormal', dim, truncation)
      self$add_parameter(meanlog, 'meanlog')
      self$add_parameter(sdlog, 'sdlog')
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

    tf_distrib = function (parameters) {
      tf$contrib$distributions$Binomial(total_count = parameters$size,
                                        probs = parameters$prob)
    },

    # no CDF for discrete distributions
    tf_cdf_function = NULL,
    tf_log_cdf_function = NULL

  )
)

beta_binomial_distribution <- R6Class (
  'beta_binomial_distribution',
  inherit = distribution_node,
  public = list(

    initialize = function (size, alpha, beta, dim) {
      # add the nodes as children and parameters
      dim <- check_dims(size, alpha, beta, target_dim = dim)
      super$initialize('beta_binomial', dim, discrete = TRUE)
      self$add_parameter(size, 'size')
      self$add_parameter(alpha, 'alpha')
      self$add_parameter(beta, 'beta')

    },

    tf_distrib = function (parameters) {

      size <- parameters$size
      alpha <- parameters$alpha
      beta <- parameters$beta

      log_prob <- function(x) {
        tf_lchoose(size, x) +
          tf_lbeta(x + alpha, size - x + beta) -
          tf_lbeta(alpha, beta)
      }

      list(log_prob = log_prob, cdf = NULL, log_cdf = NULL)

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

    tf_distrib = function (parameters) {
      tf$contrib$distributions$NegativeBinomial(total_count = parameters$size,
                                                probs = fl(1) - parameters$prob)
    },

    # no CDF for discrete distributions
    tf_cdf_function = NULL,
    tf_log_cdf_function = NULL

  )
)

hypergeometric_distribution <- R6Class (
  'hypergeometric_distribution',
  inherit = distribution_node,
  public = list(

    initialize = function (m, n, k, dim) {
      # add the nodes as children and parameters
      dim <- check_dims(m, n, k, target_dim = dim)
      super$initialize('hypergeometric', dim, discrete = TRUE)
      self$add_parameter(m, 'm')
      self$add_parameter(n, 'n')
      self$add_parameter(k, 'k')
    },

    tf_distrib = function (parameters) {

      m <- parameters$m
      n <- parameters$n
      k <- parameters$k

      log_prob <- function (x)
        tf_lchoose(m, x) + tf_lchoose(n, k - x) - tf_lchoose(m + n, k)

      list(log_prob = log_prob, cdf = NULL, log_cdf = NULL)

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

    initialize = function (shape, rate, dim, truncation) {
      # add the nodes as children and parameters
      dim <- check_dims(shape, rate, target_dim = dim)
      check_positive(truncation)
      self$bounds <- c(0, Inf)
      super$initialize('gamma', dim, truncation)
      self$add_parameter(shape, 'shape')
      self$add_parameter(rate, 'rate')
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

    initialize = function (alpha, beta, dim, truncation) {
      # add the nodes as children and parameters
      dim <- check_dims(alpha, beta, target_dim = dim)
      check_positive(truncation)
      self$bounds <- c(0, Inf)
      super$initialize('inverse_gamma', dim, truncation)
      self$add_parameter(alpha, 'alpha')
      self$add_parameter(beta, 'beta')
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

    initialize = function (shape, scale, dim, truncation) {
      # add the nodes as children and parameters
      dim <- check_dims(shape, scale, target_dim = dim)
      check_positive(truncation)
      self$bounds <- c(0, Inf)
      super$initialize('weibull', dim, truncation)
      self$add_parameter(shape, 'shape')
      self$add_parameter(scale, 'scale')
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

    initialize = function (rate, dim, truncation) {
      # add the nodes as children and parameters
      dim <- check_dims(rate, target_dim = dim)
      check_positive(truncation)
      self$bounds <- c(0, Inf)
      super$initialize('exponential', dim, truncation)
      self$add_parameter(rate, 'rate')
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

    initialize = function (a, b, dim, truncation) {
      # add the nodes as children and parameters
      dim <- check_dims(a, b, target_dim = dim)
      check_positive(truncation)
      self$bounds <- c(0, Inf)
      super$initialize('pareto', dim, truncation)
      self$add_parameter(a, 'a')
      self$add_parameter(b, 'b')
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

    initialize = function (df, mu, sigma, dim, truncation) {
      # add the nodes as children and parameters
      dim <- check_dims(df, mu, sigma, target_dim = dim)
      super$initialize('student', dim, truncation)
      self$add_parameter(df, 'df')
      self$add_parameter(mu, 'mu')
      self$add_parameter(sigma, 'sigma')
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

    initialize = function (mu, sigma, dim, truncation) {
      # add the nodes as children and parameters
      dim <- check_dims(mu, sigma, target_dim = dim)
      super$initialize('laplace', dim, truncation)
      self$add_parameter(mu, 'mu')
      self$add_parameter(sigma, 'sigma')
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

    initialize = function (shape1, shape2, dim, truncation) {
      # add the nodes as children and parameters
      dim <- check_dims(shape1, shape2, target_dim = dim)
      check_unit(truncation)
      self$bounds <- c(0, 1)
      super$initialize('beta', dim, truncation)
      self$add_parameter(shape1, 'shape1')
      self$add_parameter(shape2, 'shape2')
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

    initialize = function (location, scale, dim, truncation) {
      # add the nodes as children and parameters
      dim <- check_dims(location, scale, target_dim = dim)
      super$initialize('cauchy', dim, truncation)
      self$add_parameter(location, 'location')
      self$add_parameter(scale, 'scale')
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

    initialize = function (df, dim, truncation) {
      # add the nodes as children and parameters
      dim <- check_dims(df, target_dim = dim)
      check_positive(truncation)
      self$bounds <- c(0, Inf)
      super$initialize('chi_squared', dim, truncation)
      self$add_parameter(df, 'df')
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

    initialize = function (location, scale, dim, truncation) {
      # add the nodes as children and parameters
      dim <- check_dims(location, scale, target_dim = dim)
      super$initialize('logistic', dim, truncation)
      self$add_parameter(location, 'location')
      self$add_parameter(scale, 'scale')
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

    initialize = function (df1, df2, dim, truncation) {
      # add the nodes as children and parameters
      dim <- check_dims(df1, df2, target_dim = dim)
      check_positive(truncation)
      self$bounds <- c(0, Inf)
      super$initialize('d', dim, truncation)
      self$add_parameter(df1, 'df1')
      self$add_parameter(df2, 'df2')
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

dirichlet_distribution <- R6Class (
  'dirichlet_distribution',
  inherit = distribution_node,
  public = list(

    initialize = function (alpha, dim) {

      # coerce to greta arrays
      alpha <- as.greta_array(alpha)

      # check dimensions of alpha
      if (ncol(alpha) != 1 |
          length(dim(alpha)) != 2) {

        stop ('alpha must be a 2D greta array with one column, but has dimensions ',
              paste(dim(alpha), collapse = ' x '),
              call. = FALSE)

      }

      if (length(alpha) == 1) {

        stop ('the dirichlet distribution is for vectors, ',
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
      self$bounds <- c(0, Inf)
      super$initialize('dirichlet', c(dim, length(alpha)), truncation = c(0, Inf))
      self$add_parameter(alpha, 'alpha')

    },

    tf_distrib = function (parameters) {
      # transpose and scale probs to get absolute density correct
      alpha <- tf$transpose(parameters$alpha)
      tf$contrib$distributions$Dirichlet(concentration = alpha)
    },

    # no CDF for multivariate distributions
    tf_cdf_function = NULL,
    tf_log_cdf_function = NULL

  )
)


dirichlet_multinomial_distribution <- R6Class (
  'dirichlet_multinomial_distribution',
  inherit = distribution_node,
  public = list(

    initialize = function (size, alpha, dim) {

      # coerce to greta arrays
      size <- as.greta_array(size)
      alpha <- as.greta_array(alpha)

      # check dimensions
      if (length(size) != 1) {

        stop ('size must be a scalar, but has dimensions ',
              paste(dim(size), collapse = ' x '),
              call. = FALSE)

      }

      if (ncol(alpha) != 1 |
          length(dim(alpha)) != 2) {

        stop ('alpha must be a 2D greta array with one column, but has dimensions ',
              paste(dim(alpha), collapse = ' x '),
              call. = FALSE)

      }

      if (length(alpha) == 1) {

        stop ('the dirichlet distribution is for vectors, ',
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
      super$initialize('dirichlet_multinomial', dim = c(dim, length(alpha)), discrete = TRUE)
      self$add_parameter(size, 'size')
      self$add_parameter(alpha, 'alpha')

    },

    tf_distrib = function (parameters) {
      # transpose and scale probs to get absolute density correct
      alpha <- tf$transpose(parameters$alpha)
      tf$contrib$distributions$DirichletMultinomial(total_count = parameters$size,
                                                    concentration = alpha)
    },

    # no CDF for multivariate distributions
    tf_cdf_function = NULL,
    tf_log_cdf_function = NULL

  )
)

multinomial_distribution <- R6Class (
  'multinomial_distribution',
  inherit = distribution_node,
  public = list(

    initialize = function (size, prob, dim) {

      # coerce to greta arrays
      size <- as.greta_array(size)
      prob <- as.greta_array(prob)

      # check dimensions
      if (length(size) != 1) {

        stop ('size must be a scalar, but has dimensions ',
              paste(dim(size), collapse = ' x '),
              call. = FALSE)

      }

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
      super$initialize('multinomial', dim = c(dim, length(prob)), discrete = TRUE)
      self$add_parameter(size, 'size')
      self$add_parameter(prob, 'prob')

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
      super$initialize('categorical', dim = c(dim, length(prob)), discrete = TRUE)
      self$add_parameter(prob, 'prob')

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

    # fetch the tensors for the parameters, using the cholesky factor of Sigma
    # if available
    tf_fetch_parameters = function (dag) {

      # find names
      tf_names <- lapply(self$parameters, dag$tf_name)

      # replace Sigma's tensor with the cholesky factor, if available
      cf <- self$parameters$Sigma$representations$cholesky_factor
      if (!is.null(cf))
        tf_names$Sigma <- dag$tf_name(cf)

      # fetch tensors
      lapply(tf_names, get, envir = dag$tf_environment)

    },

    tf_distrib = function (parameters) {

      # check if Sigma (the node version) has a cholesky factor to use
      cf <- self$parameters$Sigma$representations$cholesky_factor

      # how to make sure that's the value being used as the parameter?

      # need to check the parameters definition bit

      if (is.null(cf))
        L <- tf$cholesky(parameters$Sigma)
      else
        L <- tf$transpose(parameters$Sigma)

      mu = tf$transpose(parameters$mean)
      tf$contrib$distributions$MultivariateNormalTriL(loc = mu,
                                                      scale_tril = L)
    },

    # no CDF for multivariate distributions
    tf_cdf_function = NULL,
    tf_log_cdf_function = NULL

  )
)

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

    # default value, cholesky factor (ignores truncation)
    create_target = function (truncation) {

      # handle reshaping via a greta array
      k <- self$dim[1]
      free_greta_array <- vble(truncation = c(-Inf, Inf),
                               dim = k + k * (k - 1) / 2)
      free_greta_array$constraint = "covariance_matrix"

      # first create a greta array for the cholesky
      chol_greta_array <- flat_to_chol(free_greta_array, self$dim)

      # create symmetric matrix to return as target node
      matrix_greta_array <- chol_to_symmetric(chol_greta_array)
      target_node <- matrix_greta_array$node

      # assign the cholesky factor as a representation of it
      target_node$representations$cholesky_factor <- chol_greta_array$node

      # return the symmetric node
      target_node

    },

    tf_distrib = function (parameters) {

      # if there is a cholesky factor for Sigma,use that
      is_cholesky <- !is.null(self$parameters$Sigma$representations$cholesky_factor)

      if (is_cholesky) {

        tf$contrib$distributions$WishartCholesky(df = parameters$df,
                                                 scale = parameters$Sigma)

      } else {

        tf$contrib$distributions$WishartFull(df = parameters$df,
                                             scale = parameters$Sigma)

      }

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

lkj_correlation_distribution <- R6Class (
  'lkj_correlation_distribution',
  inherit = distribution_node,
  public = list(

    initialize = function (eta, dim = 2) {

      # check dim is a scalar integer greater than 1
      dim_old <- dim
      dim <- as.integer(dim)
      if (length(dim) > 1 || dim <= 1 || !is.finite(dim)) {

        stop ('dim must be a scalar integer greater than one, but was: ',
              capture.output(dput(dim_old)),
              call. = FALSE)

      }

      if (!is.greta_array(eta)) {

        if (!is.numeric(eta) || !length(eta) == 1 || eta <= 0) {
          stop ("eta must be a positive scalar value, or a scalar greta array",
                call. = FALSE)
        }

      }

      # add the nodes as children and parameters
      eta <- as.greta_array(eta)

      if (!is_scalar(eta)) {

        stop ("eta must be a scalar, but had dimensions: ",
              capture.output(dput(dim(eta))),
              call. = FALSE)

      }

      super$initialize('lkj_correlation', c(dim, dim))
      self$add_parameter(eta, 'eta')

      # make the initial value PD
      self$value(unknowns(dims = c(dim, dim), data = diag(dim)))

    },

    # default (cholesky factor, ignores truncation)
    create_target = function (truncation) {

      # handle reshaping via a greta array
      k <- self$dim[1]
      free_greta_array <- vble(truncation = c(-Inf, Inf),
                               dim = k * (k - 1) / 2)
      free_greta_array$constraint = "correlation_matrix"

      # first create a greta array for the cholesky
      chol_greta_array <- flat_to_chol(free_greta_array, self$dim, correl = TRUE)

      # create symmetric matrix to return as target node
      matrix_greta_array <- chol_to_symmetric(chol_greta_array)
      target_node <- matrix_greta_array$node

      # assign the cholesky factor as a representation of it
      target_node$representations$cholesky_factor <- chol_greta_array$node

      # return the symmetric node
      target_node

    },

    # if the target has a cholesky factor, use that
    get_tf_target_node = function () {

      tf_target_node <- self$target$representations$cholesky_factor

      if (is.null(tf_target_node))
        tf_target_node <- self$target

      tf_target_node

    },

    tf_distrib = function (parameters) {

      eta <- parameters$eta

      # if the cholesky factor exists, we'll be using that
      is_cholesky <- !is.null(self$target$representations$cholesky_factor)

      log_prob = function (x) {

        if (!is_cholesky)
          x <- tf$cholesky(x)

        diags <- tf$diag_part(x)
        det <- tf$square(tf$reduce_prod(diags))
        prob <- det ^ (eta - fl(1))
        tf$log(prob)

      }

      list(log_prob = log_prob, cdf = NULL, log_cdf = NULL)

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

  # return the user-facing representation of the node as a greta array
  value <- distrib$user_node
  as.greta_array(value)

}

# export constructors

#' @name distributions
#' @title probability distributions
#' @description These functions can be used to define random variables in a
#'   greta model. They return a variable greta array that follows the specified
#'   distribution. This variable greta array can be used to represent a
#'   parameter with  prior distribution, or used with \code{\link{distribution}}
#'   to define a distribution over a data greta array.
#'
#' @param truncation a length-two vector giving values between which to truncate
#'   the distribution, similarly to the \code{lower} and \code{upper} arguments
#'   to \code{\link{variable}}
#'
#' @param min,max scalar values giving optional limits to \code{uniform}
#'   variables. Like \code{lower} and \code{upper}, these must be specified as
#'   numerics, they cannot be greta arrays (though see details for a
#'   workaround). Unlike \code{lower} and \code{upper}, they must be finite.
#'   \code{min} must always be less than \code{max}.
#'
#' @param mean,meanlog,location,mu unconstrained parameters
#'
#' @param sd,sdlog,sigma,lambda,shape,rate,df,scale,shape1,shape2,alpha,beta,df1,df2,a,b,eta
#'   positive parameters, \code{alpha} must be a vector for \code{dirichlet} and \code{dirichlet_multinomial}.
#'
#' @param size,m,n,k positive integer parameter
#'
#' @param prob probability parameter (\code{0 < prob < 1}), must be a vector for
#'   \code{multinomial} and \code{categorical}
#'
#' @param Sigma positive definite variance-covariance matrix parameter
#'
#' @param dim the dimensions of the greta array to be returned, either a scalar
#'   or a vector of positive integers. See details.
#'
#' @details The discrete probability distributions (\code{bernoulli},
#'   \code{binomial}, \code{negative_binomial}, \code{poisson},
#'   \code{multinomial}, \code{categorical}, \code{dirichlet_multinomial}) can
#'   be used when they have fixed values (e.g. defined as a likelihood using
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
#'   \tabular{ll}{ greta \tab reference\cr
#'    \code{uniform} \tab \link[stats:dunif]{stats::dunif}\cr
#'    \code{normal} \tab \link[stats:dnorm]{stats::dnorm}\cr
#'    \code{lognormal} \tab \link[stats:dlnorm]{stats::dlnorm}\cr
#'    \code{bernoulli} \tab \link[extraDistr:dbern]{extraDistr::dbern}\cr
#'    \code{binomial} \tab \link[stats:dbinom]{stats::dbinom}\cr
#'    \code{beta_binomial} \tab \link[extraDistr:dbbinom]{extraDistr::dbbinom}\cr
#'    \code{negative_binomial} \tab \link[stats:dnbinom]{stats::dnbinom}\cr
#'    \code{hypergeometric} \tab \link[stats:dhyper]{stats::dhyper}\cr
#'    \code{poisson} \tab \link[stats:dpois]{stats::dpois}\cr
#'    \code{gamma} \tab \link[stats:dgamma]{stats::dgamma}\cr
#'    \code{inverse_gamma} \tab \link[extraDistr:dinvgamma]{extraDistr::dinvgamma}\cr
#'    \code{weibull} \tab \link[stats:dweibull]{stats::dweibull}\cr
#'    \code{exponential} \tab \link[stats:dexp]{stats::dexp}\cr
#'    \code{pareto} \tab \link[extraDistr:dpareto]{extraDistr::dpareto}\cr
#'    \code{student} \tab \link[extraDistr:dnst]{extraDistr::dnst}\cr
#'    \code{laplace} \tab \link[extraDistr:dlaplace]{extraDistr::dlaplace}\cr
#'    \code{beta} \tab \link[stats:dbeta]{stats::dbeta}\cr
#'    \code{cauchy} \tab \link[stats:dcauchy]{stats::dcauchy}\cr
#'    \code{chi_squared} \tab \link[stats:dchisq]{stats::dchisq}\cr
#'    \code{logistic} \tab \link[stats:dlogis]{stats::dlogis}\cr
#'    \code{f} \tab \link[stats:df]{stats::df}\cr
#'    \code{multivariate_normal} \tab \link[mvtnorm:dmvnorm]{mvtnorm::dmvnorm}\cr
#'    \code{multinomial} \tab \link[stats:dmultinom]{stats::dmultinom}\cr
#'    \code{categorical} \tab {\link[stats:dmultinom]{stats::dmultinom} (size = 1)}\cr
#'    \code{dirichlet} \tab \link[extraDistr:ddirichlet]{extraDistr::ddirichlet}\cr
#'    \code{dirichlet_multinomial} \tab \link[extraDistr:ddirmnom]{extraDistr::ddirmnom}\cr
#'    \code{wishart} \tab \link[stats:rWishart]{stats::rWishart}\cr
#'    \code{lkj_correlation} \tab \href{https://rdrr.io/github/rmcelreath/rethinking/man/dlkjcorr.html}{rethinking::dlkjcorr}\cr }
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
#' eta = alpha + uniform(0, 1, dim = 3) * sigma
#'
#' # a hierarchical distribution
#' mu = normal(0, 1)
#' sigma = lognormal(0, 1)
#' theta = normal(mu, sigma)
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

#' @rdname distributions
#' @export
uniform <- function (min, max, dim = NULL)
  distrib('uniform', min, max, dim)

#' @rdname distributions
#' @export
normal <- function (mean, sd, dim = NULL, truncation = c(-Inf, Inf))
  distrib('normal', mean, sd, dim, truncation)

#' @rdname distributions
#' @export
lognormal <- function (meanlog, sdlog, dim = NULL, truncation = c(0, Inf))
  distrib('lognormal', meanlog, sdlog, dim, truncation)

#' @rdname distributions
#' @export
bernoulli <- function (prob, dim = NULL)
  distrib('bernoulli', prob, dim)

#' @rdname distributions
#' @export
binomial <- function (size, prob, dim = NULL)
  distrib('binomial', size, prob, dim)

#' @rdname distributions
#' @export
beta_binomial <- function (size, alpha, beta, dim = NULL)
  distrib('beta_binomial', size, alpha, beta, dim)

#' @rdname distributions
#' @export
negative_binomial <- function (size, prob, dim = NULL)
  distrib('negative_binomial', size, prob, dim)

#' @rdname distributions
#' @export
hypergeometric <- function (m, n, k, dim = NULL)
  distrib('hypergeometric', m, n, k, dim)

#' @rdname distributions
#' @export
poisson <- function (lambda, dim = NULL)
  distrib('poisson', lambda, dim)

#' @rdname distributions
#' @export
gamma <- function (shape, rate, dim = NULL, truncation = c(0, Inf))
  distrib('gamma', shape, rate, dim, truncation)

#' @rdname distributions
#' @export
inverse_gamma <- function (alpha, beta, dim = NULL, truncation = c(0, Inf))
  distrib('inverse_gamma', alpha, beta, dim, truncation)

#' @rdname distributions
#' @export
weibull <- function (shape, scale, dim = NULL, truncation = c(0, Inf))
  distrib('weibull', shape, scale, dim, truncation)

#' @rdname distributions
#' @export
exponential <- function (rate, dim = NULL, truncation = c(0, Inf))
  distrib('exponential', rate, dim, truncation)

#' @rdname distributions
#' @export
pareto <- function (a, b, dim = NULL, truncation = c(0, Inf))
  distrib('pareto', a, b, dim, truncation)

#' @rdname distributions
#' @export
student <- function (df, mu, sigma, dim = NULL, truncation = c(-Inf, Inf))
  distrib('student', df, mu, sigma, dim, truncation)

#' @rdname distributions
#' @export
laplace <- function (mu, sigma, dim = NULL, truncation = c(-Inf, Inf))
  distrib('laplace', mu, sigma, dim, truncation)

#' @rdname distributions
#' @export
beta <- function (shape1, shape2, dim = NULL, truncation = c(0, 1))
  distrib('beta', shape1, shape2, dim, truncation)

#' @rdname distributions
#' @export
cauchy <- function (location, scale, dim = NULL, truncation = c(-Inf, Inf))
  distrib('cauchy', location, scale, dim, truncation)

#' @rdname distributions
#' @export
chi_squared <- function (df, dim = NULL, truncation = c(0, Inf))
  distrib('chi_squared', df, dim, truncation)

#' @rdname distributions
#' @export
logistic <- function (location, scale, dim = NULL, truncation = c(-Inf, Inf))
  distrib('logistic', location, scale, dim, truncation)

#' @rdname distributions
#' @export
f <- function (df1, df2, dim = NULL, truncation = c(0, Inf))
  distrib('f', df1, df2, dim, truncation)

#' @rdname distributions
#' @export
multivariate_normal <- function (mean, Sigma, dim = 1)
  distrib('multivariate_normal', mean, Sigma, dim)

#' @rdname distributions
#' @export
wishart <- function (df, Sigma)
  distrib('wishart', df, Sigma)

#' @rdname distributions
#' @export
lkj_correlation <- function (eta, dim = 2)
  distrib('lkj_correlation', eta, dim)

#' @rdname distributions
#' @export
multinomial <- function (size, prob, dim = 1)
  distrib('multinomial', size, prob, dim)

#' @rdname distributions
#' @export
categorical <- function (prob, dim = 1)
  distrib('categorical', prob, dim)

#' @rdname distributions
#' @export
dirichlet <- function (alpha, dim = 1)
  distrib('dirichlet', alpha, dim)

#' @rdname distributions
#' @export
dirichlet_multinomial <- function (size, alpha, dim = 1)
  distrib('dirichlet_multinomial', size, alpha, dim)
