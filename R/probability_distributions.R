uniform_distribution <- R6Class(
  "uniform_distribution",
  inherit = distribution_node,
  public = list(
    min = NA,
    max = NA,
    initialize = function(min, max, dim) {
      check_param_greta_array(min)
      check_param_greta_array(max)
      check_numeric_length_1(min)
      check_numeric_length_1(max)
      check_finite(min)
      check_finite(max)
      check_x_gte_y(min, max)

      # store min and max as numeric scalars (needed in create_target, done in
      # initialisation)
      self$min <- min
      self$max <- max
      self$bounds <- c(min, max)

      # initialize the rest
      super$initialize("uniform", dim)

      # add them as parents and greta arrays
      min <- as.greta_array(min)
      max <- as.greta_array(max)
      self$add_parameter(min, "min")
      self$add_parameter(max, "max")
    },

    # default value (ignore any truncation arguments)
    create_target = function(...) {
      vble(
        truncation = c(self$min, self$max),
        dim = self$dim
      )
    },
    tf_distrib = function(parameters, dag) {
      tfp$distributions$Uniform(
        low = parameters$min,
        high = parameters$max
      )
    }
  )
)

normal_distribution <- R6Class(
  "normal_distribution",
  inherit = distribution_node,
  public = list(
    initialize = function(mean, sd, dim, truncation) {
      mean <- as.greta_array(mean)
      sd <- as.greta_array(sd)

      # add the nodes as parents and parameters
      dim <- check_dims(mean, sd, target_dim = dim)
      super$initialize("normal", dim, truncation)
      self$add_parameter(mean, "mean")
      self$add_parameter(sd, "sd")
    },
    # TODO
    # why is "dag" an argument here?
    tf_distrib = function(parameters, dag) {
      tfp$distributions$Normal(
        loc = parameters$mean,
        scale = parameters$sd
      )
    }
  )
)

lognormal_distribution <- R6Class(
  "lognormal_distribution",
  inherit = distribution_node,
  public = list(
    initialize = function(meanlog, sdlog, dim, truncation) {
      meanlog <- as.greta_array(meanlog)
      sdlog <- as.greta_array(sdlog)

      dim <- check_dims(meanlog, sdlog, target_dim = dim)
      check_positive(truncation)
      self$bounds <- c(0, Inf)
      super$initialize("lognormal", dim, truncation)
      self$add_parameter(meanlog, "meanlog")
      self$add_parameter(sdlog, "sdlog")
    },

    # nolint start
    tf_distrib = function(parameters, dag) {
      tfp$distributions$LogNormal(
        loc = parameters$meanlog,
        scale = parameters$sdlog
      )
    }
    # nolint end
  )
)

bernoulli_distribution <- R6Class(
  "bernoulli_distribution",
  inherit = distribution_node,
  public = list(
    prob_is_logit = FALSE,
    prob_is_probit = FALSE,
    initialize = function(prob, dim) {
      prob <- as.greta_array(prob)

      # add the nodes as parents and parameters
      dim <- check_dims(prob, target_dim = dim)
      super$initialize("bernoulli", dim, discrete = TRUE)

      if (has_representation(prob, "logit")) {
        prob <- representation(prob, "logit")
        self$prob_is_logit <- TRUE
      } else if (has_representation(prob, "probit")) {
        prob <- representation(prob, "probit")
        self$prob_is_probit <- TRUE
      }

      self$add_parameter(prob, "prob")
    },
    tf_distrib = function(parameters, dag) {
      if (self$prob_is_logit) {
        tfp$distributions$Bernoulli(logits = parameters$prob)
      } else if (self$prob_is_probit) {

        # in the probit case, get the log probability of success and compute the
        # log prob directly
        probit <- parameters$prob
        d <- tfp$distributions$Normal(fl(0), fl(1))
        lprob <- d$log_cdf(probit)
        lprobnot <- d$log_cdf(-probit)

        log_prob <- function(x) {
          x * lprob + (fl(1) - x) * lprobnot
        }

        list(log_prob = log_prob)
      } else {
        tfp$distributions$Bernoulli(probs = parameters$prob)
      }
    }
  )
)

binomial_distribution <- R6Class(
  "binomial_distribution",
  inherit = distribution_node,
  public = list(
    prob_is_logit = FALSE,
    prob_is_probit = FALSE,
    initialize = function(size, prob, dim) {
      size <- as.greta_array(size)
      prob <- as.greta_array(prob)

      # add the nodes as parents and parameters
      dim <- check_dims(size, prob, target_dim = dim)
      super$initialize("binomial", dim, discrete = TRUE)

      if (has_representation(prob, "logit")) {
        prob <- representation(prob, "logit")
        self$prob_is_logit <- TRUE
      } else if (has_representation(prob, "probit")) {
        prob <- representation(prob, "probit")
        self$prob_is_probit <- TRUE
      }

      self$add_parameter(prob, "prob")
      self$add_parameter(size, "size")
    },
    tf_distrib = function(parameters, dag) {
      if (self$prob_is_logit) {
        tfp$distributions$Binomial(
          total_count = parameters$size,
          logits = parameters$prob
        )
      } else if (self$prob_is_probit) {

        # in the probit case, get the log probability of success and compute the
        # log prob directly
        size <- parameters$size
        probit <- parameters$prob
        d <- tfp$distributions$Normal(fl(0), fl(1))
        lprob <- d$log_cdf(probit)
        lprobnot <- d$log_cdf(-probit)

        log_prob <- function(x) {
          log_choose <- tf$math$lgamma(size + fl(1)) -
            tf$math$lgamma(x + fl(1)) -
            tf$math$lgamma(size - x + fl(1))
          log_choose + x * lprob + (size - x) * lprobnot
        }

        list(log_prob = log_prob)
      } else {
        tfp$distributions$Binomial(
          total_count = parameters$size,
          probs = parameters$prob
        )
      }
    }
  )
)

beta_binomial_distribution <- R6Class(
  "beta_binomial_distribution",
  inherit = distribution_node,
  public = list(
    initialize = function(size, alpha, beta, dim) {
      size <- as.greta_array(size)
      alpha <- as.greta_array(alpha)
      beta <- as.greta_array(beta)

      # add the nodes as parents and parameters
      dim <- check_dims(size, alpha, beta, target_dim = dim)
      super$initialize("beta_binomial", dim, discrete = TRUE)
      self$add_parameter(size, "size")
      self$add_parameter(alpha, "alpha")
      self$add_parameter(beta, "beta")
    },
    tf_distrib = function(parameters, dag) {
      size <- parameters$size
      alpha <- parameters$alpha
      beta <- parameters$beta

      log_prob <- function(x) {
        tf_lchoose(size, x) +
          tf_lbeta(x + alpha, size - x + beta) -
          tf_lbeta(alpha, beta)
      }

      # generate a beta, then a binomial
      sample <- function(seed) {
        beta <- tfp$distributions$Beta(
          concentration1 = alpha,
          concentration0 = beta
        )
        probs <- beta$sample(seed = seed)
        binomial <- tfp$distributions$Binomial(
          total_count = size,
          probs = probs
        )
        binomial$sample(seed = seed)
      }

      list(log_prob = log_prob, sample = sample)
    }
  )
)

poisson_distribution <- R6Class(
  "poisson_distribution",
  inherit = distribution_node,
  public = list(
    lambda_is_log = FALSE,
    initialize = function(lambda, dim) {
      lambda <- as.greta_array(lambda)

      # add the nodes as parents and parameters
      dim <- check_dims(lambda, target_dim = dim)
      super$initialize("poisson", dim, discrete = TRUE)

      if (has_representation(lambda, "log")) {
        lambda <- representation(lambda, "log")
        self$lambda_is_log <- TRUE
      }
      self$add_parameter(lambda, "lambda")
    },
    tf_distrib = function(parameters, dag) {
      if (self$lambda_is_log) {
        log_lambda <- parameters$lambda
      } else {
        log_lambda <- tf$math$log(parameters$lambda)
      }

      tfp$distributions$Poisson(log_rate = log_lambda)
    }
  )
)

negative_binomial_distribution <- R6Class(
  "negative_binomial_distribution",
  inherit = distribution_node,
  public = list(
    initialize = function(size, prob, dim) {
      size <- as.greta_array(size)
      prob <- as.greta_array(prob)

      # add the nodes as parents and parameters
      dim <- check_dims(size, prob, target_dim = dim)
      super$initialize("negative_binomial", dim, discrete = TRUE)
      self$add_parameter(size, "size")
      self$add_parameter(prob, "prob")
    },

    # nolint start
    tf_distrib = function(parameters, dag) {
      tfp$distributions$NegativeBinomial(
        total_count = parameters$size,
        probs = fl(1) - parameters$prob
      )
    }
    # nolint end
  )
)

hypergeometric_distribution <- R6Class(
  "hypergeometric_distribution",
  inherit = distribution_node,
  public = list(
    initialize = function(m, n, k, dim) {
      m <- as.greta_array(m)
      n <- as.greta_array(n)
      k <- as.greta_array(k)

      # add the nodes as parents and parameters
      dim <- check_dims(m, n, k, target_dim = dim)
      super$initialize("hypergeometric", dim, discrete = TRUE)
      self$add_parameter(m, "m")
      self$add_parameter(n, "n")
      self$add_parameter(k, "k")
    },
    tf_distrib = function(parameters, dag) {
      m <- parameters$m
      n <- parameters$n
      k <- parameters$k

      log_prob <- function(x) {
        tf_lchoose(m, x) +
          tf_lchoose(n, k - x) -
          tf_lchoose(m + n, k)
      }

      list(log_prob = log_prob)
    }
  )
)

gamma_distribution <- R6Class(
  "gamma_distribution",
  inherit = distribution_node,
  public = list(
    initialize = function(shape, rate, dim, truncation) {
      shape <- as.greta_array(shape)
      rate <- as.greta_array(rate)

      # add the nodes as parents and parameters
      dim <- check_dims(shape, rate, target_dim = dim)
      check_positive(truncation)
      self$bounds <- c(0, Inf)
      super$initialize("gamma", dim, truncation)
      self$add_parameter(shape, "shape")
      self$add_parameter(rate, "rate")
    },
    tf_distrib = function(parameters, dag) {
      tfp$distributions$Gamma(
        concentration = parameters$shape,
        rate = parameters$rate
      )
    }
  )
)

inverse_gamma_distribution <- R6Class(
  "inverse_gamma_distribution",
  inherit = distribution_node,
  public = list(
    initialize = function(alpha, beta, dim, truncation) {
      alpha <- as.greta_array(alpha)
      beta <- as.greta_array(beta)

      # add the nodes as parents and parameters
      dim <- check_dims(alpha, beta, target_dim = dim)
      check_positive(truncation)
      self$bounds <- c(0, Inf)
      super$initialize("inverse_gamma", dim, truncation)
      self$add_parameter(alpha, "alpha")
      self$add_parameter(beta, "beta")
    },

    # nolint start
    tf_distrib = function(parameters, dag) {
      tfp$distributions$InverseGamma(
        concentration = parameters$alpha,
        scale = parameters$beta
      )
    }
    # nolint end
  )
)

weibull_distribution <- R6Class(
  "weibull_distribution",
  inherit = distribution_node,
  public = list(
    initialize = function(shape, scale, dim, truncation) {
      shape <- as.greta_array(shape)
      scale <- as.greta_array(scale)

      # add the nodes as parents and parameters
      dim <- check_dims(shape, scale, target_dim = dim)
      check_positive(truncation)
      self$bounds <- c(0, Inf)
      super$initialize("weibull", dim, truncation)
      self$add_parameter(shape, "shape")
      self$add_parameter(scale, "scale")
    },
    tf_distrib = function(parameters, dag) {
      a <- parameters$shape
      b <- parameters$scale

      # use the TFP Weibull CDF bijector
      bijector <- tfp$bijectors$WeibullCDF(scale = b, concentration = a)

      log_prob <- function(x) {
        log(a) - log(b) + (a - fl(1)) * (log(x) - log(b)) - (x / b)^a
      }

      cdf <- function(x) {
        bijector$forward(x)
      }

      log_cdf <- function(x) {
        log(cdf(x))
      }

      quantile <- function(x) {
        bijector$inverse(x)
      }

      sample <- function(seed) {

        # sample by pushing standard uniforms through the inverse cdf
        u <- tf_randu(self$dim, dag)
        quantile(u)
      }

      list(
        log_prob = log_prob,
        cdf = cdf,
        log_cdf = log_cdf,
        quantile = quantile,
        sample = sample
      )
    }
  )
)

exponential_distribution <- R6Class(
  "exponential_distribution",
  inherit = distribution_node,
  public = list(
    initialize = function(rate, dim, truncation) {
      rate <- as.greta_array(rate)

      # add the nodes as parents and parameters
      dim <- check_dims(rate, target_dim = dim)
      check_positive(truncation)
      self$bounds <- c(0, Inf)
      super$initialize("exponential", dim, truncation)
      self$add_parameter(rate, "rate")
    },
    tf_distrib = function(parameters, dag) {
      tfp$distributions$Exponential(rate = parameters$rate)
    }
  )
)

pareto_distribution <- R6Class(
  "pareto_distribution",
  inherit = distribution_node,
  public = list(
    initialize = function(a, b, dim, truncation) {
      a <- as.greta_array(a)
      b <- as.greta_array(b)

      # add the nodes as parents and parameters
      dim <- check_dims(a, b, target_dim = dim)
      check_positive(truncation)
      self$bounds <- c(0, Inf)
      super$initialize("pareto", dim, truncation)
      self$add_parameter(a, "a")
      self$add_parameter(b, "b")
    },
    tf_distrib = function(parameters, dag) {

      # a is shape, b is scale
      tfp$distributions$Pareto(
        concentration = parameters$a,
        scale = parameters$b
      )
    }
  )
)

student_distribution <- R6Class(
  "student_distribution",
  inherit = distribution_node,
  public = list(
    initialize = function(df, mu, sigma, dim, truncation) {
      df <- as.greta_array(df)
      mu <- as.greta_array(mu)
      sigma <- as.greta_array(sigma)

      # add the nodes as parents and parameters
      dim <- check_dims(df, mu, sigma, target_dim = dim)
      super$initialize("student", dim, truncation)
      self$add_parameter(df, "df")
      self$add_parameter(mu, "mu")
      self$add_parameter(sigma, "sigma")
    },

    # nolint start
    tf_distrib = function(parameters, dag) {
      tfp$distributions$StudentT(
        df = parameters$df,
        loc = parameters$mu,
        scale = parameters$sigma
      )
    }
    # nolint end
  )
)

laplace_distribution <- R6Class(
  "laplace_distribution",
  inherit = distribution_node,
  public = list(
    initialize = function(mu, sigma, dim, truncation) {
      mu <- as.greta_array(mu)
      sigma <- as.greta_array(sigma)

      # add the nodes as parents and parameters
      dim <- check_dims(mu, sigma, target_dim = dim)
      super$initialize("laplace", dim, truncation)
      self$add_parameter(mu, "mu")
      self$add_parameter(sigma, "sigma")
    },
    tf_distrib = function(parameters, dag) {
      tfp$distributions$Laplace(
        loc = parameters$mu,
        scale = parameters$sigma
      )
    }
  )
)

beta_distribution <- R6Class(
  "beta_distribution",
  inherit = distribution_node,
  public = list(
    initialize = function(shape1, shape2, dim, truncation) {
      shape1 <- as.greta_array(shape1)
      shape2 <- as.greta_array(shape2)

      # add the nodes as parents and parameters
      dim <- check_dims(shape1, shape2, target_dim = dim)
      check_unit(truncation)
      self$bounds <- c(0, 1)
      super$initialize("beta", dim, truncation)
      self$add_parameter(shape1, "shape1")
      self$add_parameter(shape2, "shape2")
    },
    tf_distrib = function(parameters, dag) {
      tfp$distributions$Beta(
        concentration1 = parameters$shape1,
        concentration0 = parameters$shape2
      )
    }
  )
)

cauchy_distribution <- R6Class(
  "cauchy_distribution",
  inherit = distribution_node,
  public = list(
    initialize = function(location, scale, dim, truncation) {
      location <- as.greta_array(location)
      scale <- as.greta_array(scale)

      # add the nodes as parents and parameters
      dim <- check_dims(location, scale, target_dim = dim)
      super$initialize("cauchy", dim, truncation)
      self$add_parameter(location, "location")
      self$add_parameter(scale, "scale")
    },
    tf_distrib = function(parameters, dag) {
      tfp$distributions$Cauchy(
        loc = parameters$location,
        scale = parameters$scale
      )
    }
  )
)

chi_squared_distribution <- R6Class(
  "chi_squared_distribution",
  inherit = distribution_node,
  public = list(
    initialize = function(df, dim, truncation) {
      df <- as.greta_array(df)

      # add the nodes as parents and parameters
      dim <- check_dims(df, target_dim = dim)
      check_positive(truncation)
      self$bounds <- c(0, Inf)
      super$initialize("chi_squared", dim, truncation)
      self$add_parameter(df, "df")
    },
    tf_distrib = function(parameters, dag) {
      tfp$distributions$Chi2(df = parameters$df)
    }
  )
)

logistic_distribution <- R6Class(
  "logistic_distribution",
  inherit = distribution_node,
  public = list(
    initialize = function(location, scale, dim, truncation) {
      location <- as.greta_array(location)
      scale <- as.greta_array(scale)

      # add the nodes as parents and parameters
      dim <- check_dims(location, scale, target_dim = dim)
      super$initialize("logistic", dim, truncation)
      self$add_parameter(location, "location")
      self$add_parameter(scale, "scale")
    },
    tf_distrib = function(parameters, dag) {
      tfp$distributions$Logistic(
        loc = parameters$location,
        scale = parameters$scale
      )
    }
  )
)

f_distribution <- R6Class(
  "f_distribution",
  inherit = distribution_node,
  public = list(
    initialize = function(df1, df2, dim, truncation) {
      df1 <- as.greta_array(df1)
      df2 <- as.greta_array(df2)

      # add the nodes as parents and parameters
      dim <- check_dims(df1, df2, target_dim = dim)
      check_positive(truncation)
      self$bounds <- c(0, Inf)
      super$initialize("f", dim, truncation)
      self$add_parameter(df1, "df1")
      self$add_parameter(df2, "df2")
    },
    tf_distrib = function(parameters, dag) {
      df1 <- parameters$df1
      df2 <- parameters$df2

      tf_lbeta <- function(a, b) {
        tf$math$lgamma(a) + tf$math$lgamma(b) - tf$math$lgamma(a + b)
      }

      log_prob <- function(x) {
        df1_x <- df1 * x
        la <- df1 * log(df1_x) + df2 * log(df2)
        lb <- (df1 + df2) * log(df1_x + df2)
        lnumerator <- fl(0.5) * (la - lb)
        lnumerator - log(x) - tf_lbeta(df1 / fl(2), df2 / fl(2))
      }

      cdf <- function(x) {
        df1_x <- df1 * x
        ratio <- df1_x / (df1_x + df2)
        tf$math$betainc(df1 / fl(2), df2 / fl(2), ratio)
      }

      log_cdf <- function(x) {
        log(cdf(x))
      }

      sample <- function(seed) {

        # sample as the ratio of two scaled chi squared distributions
        d1 <- tfp$distributions$Chi2(df = df1)
        d2 <- tfp$distributions$Chi2(df = df2)

        u1 <- d1$sample(seed = seed)
        u2 <- d2$sample(seed = seed)

        (u1 / df1) / (u2 / df2)
      }

      list(
        log_prob = log_prob,
        cdf = cdf,
        log_cdf = log_cdf,
        sample = sample
      )
    }
  )
)

dirichlet_distribution <- R6Class(
  "dirichlet_distribution",
  inherit = distribution_node,
  public = list(
    initialize = function(alpha, n_realisations, dimension) {
      # coerce to greta arrays
      alpha <- as.greta_array(alpha)

      dim <- check_multivariate_dims(
        vectors = list(alpha),
        n_realisations = n_realisations,
        dimension = dimension
      )

      # coerce the parameter arguments to nodes and add as parents and
      # parameters
      self$bounds <- c(0, Inf)
      super$initialize("dirichlet", dim,
        truncation = c(0, Inf),
        multivariate = TRUE
      )
      self$add_parameter(alpha, "alpha")
    },
    create_target = function(truncation) {
      simplex_greta_array <- simplex_variable(self$dim)

      # return the node for the simplex
      target_node <- get_node(simplex_greta_array)
      target_node
    },
    tf_distrib = function(parameters, dag) {
      alpha <- parameters$alpha
      tfp$distributions$Dirichlet(concentration = alpha)
    }
  )
)

dirichlet_multinomial_distribution <- R6Class(
  "dirichlet_multinomial_distribution",
  inherit = distribution_node,
  public = list(
    initialize = function(size, alpha, n_realisations, dimension) {

      # coerce to greta arrays
      size <- as.greta_array(size)
      alpha <- as.greta_array(alpha)

      dim <- check_multivariate_dims(
        scalars = list(size),
        vectors = list(alpha),
        n_realisations = n_realisations,
        dimension = dimension
      )


      # need to handle size as a vector!

      # coerce the parameter arguments to nodes and add as parents and
      # parameters
      super$initialize("dirichlet_multinomial",
        dim = dim,
        discrete = TRUE,
        multivariate = TRUE
      )
      self$add_parameter(size, "size", shape_matches_output = FALSE)
      self$add_parameter(alpha, "alpha")
    },

    # nolint start
    tf_distrib = function(parameters, dag) {
      parameters$size <- tf_flatten(parameters$size)
      distrib <- tfp$distributions$DirichletMultinomial
      distrib(
        total_count = parameters$size,
        concentration = parameters$alpha
      )
    }
    # nolint end
  )
)

multinomial_distribution <- R6Class(
  "multinomial_distribution",
  inherit = distribution_node,
  public = list(
    initialize = function(size, prob, n_realisations, dimension) {

      # coerce to greta arrays
      size <- as.greta_array(size)
      prob <- as.greta_array(prob)

      dim <- check_multivariate_dims(
        scalars = list(size),
        vectors = list(prob),
        n_realisations = n_realisations,
        dimension = dimension
      )

      # need to make sure size is a column vector!

      # coerce the parameter arguments to nodes and add as parents and
      # parameters
      super$initialize("multinomial",
        dim = dim,
        discrete = TRUE,
        multivariate = TRUE
      )
      self$add_parameter(size, "size", shape_matches_output = FALSE)
      self$add_parameter(prob, "prob")
    },
    tf_distrib = function(parameters, dag) {
      parameters$size <- tf_flatten(parameters$size)
      # scale probs to get absolute density correct
      # parameters$prob <- parameters$prob / tf_sum(parameters$prob)
      parameters$prob <- parameters$prob / tf_rowsums(parameters$prob,
                                                      dims = 1L)

      tfp$distributions$Multinomial(
        total_count = parameters$size,
        probs = parameters$prob
      )
    }
  )
)

categorical_distribution <- R6Class(
  "categorical_distribution",
  inherit = distribution_node,
  public = list(
    initialize = function(prob, n_realisations, dimension) {

      # coerce to greta arrays
      prob <- as.greta_array(prob)

      dim <- check_multivariate_dims(
        vectors = list(prob),
        n_realisations = n_realisations,
        dimension = dimension
      )

      # coerce the parameter arguments to nodes and add as parents and
      # parameters
      super$initialize("categorical",
        dim = dim,
        discrete = TRUE,
        multivariate = TRUE
      )
      self$add_parameter(prob, "prob")
    },
    tf_distrib = function(parameters, dag) {
      # scale probs to get absolute density correct
      probs <- parameters$prob
      # probs <- probs / tf_sum(probs)
      probs <- probs / tf_rowsums(probs, dims = 1L)
      tfp$distributions$Multinomial(
        total_count = fl(1),
        probs = probs
      )
    }
  )
)

multivariate_normal_distribution <- R6Class(
  "multivariate_normal_distribution",
  inherit = distribution_node,
  public = list(
    sigma_is_cholesky = FALSE,
    # nolint start
    initialize = function(mean, Sigma, n_realisations, dimension) {
      # nolint end
      # coerce to greta arrays
      mean <- as.greta_array(mean)
      sigma <- as.greta_array(Sigma)

      # check dim is a positive scalar integer
      dim <- check_multivariate_dims(
        vectors = list(mean),
        squares = list(sigma),
        n_realisations = n_realisations,
        dimension = dimension
      )

      check_sigma_square_2d_greta_array(sigma)
      check_mean_sigma_have_same_dimensions(mean, sigma)

      # coerce parameter arguments to nodes and add as parents and parameters
      super$initialize("multivariate_normal", dim, multivariate = TRUE)

      if (has_representation(sigma, "cholesky")) {
        sigma <- representation(sigma, "cholesky")
        self$sigma_is_cholesky <- TRUE
      }
      self$add_parameter(mean, "mean")
      self$add_parameter(sigma, "sigma")
    },
    tf_distrib = function(parameters, dag) {

      # if Sigma is a cholesky factor transpose it to tensorflow expoectation,
      # otherwise decompose it

      if (self$sigma_is_cholesky) {
        l <- tf_transpose(parameters$sigma)
      } else {
        l <- tf$linalg$cholesky(parameters$sigma)
      }

      # add an extra dimension for the observation batch size (otherwise tfp
      # will try to use the n_chains batch dimension)
      l <- tf$expand_dims(l, 1L)

      mu <- parameters$mean
      # nolint start
      tfp$distributions$MultivariateNormalTriL(
        loc = mu,
        scale_tril = l
      )
      # nolint end
    }
  )
)

wishart_distribution <- R6Class(
  "wishart_distribution",
  inherit = distribution_node,
  public = list(

    # TF1/2 - consider setting this as NULL for debugging purposes
    # set when defining the distribution
    sigma_is_cholesky = FALSE,

    # TF1/2 - consider setting this as NULL for debugging purposes
    # set when defining the graph
    target_is_cholesky = FALSE,
    initialize = function(df, Sigma) { # nolint
      # add the nodes as parents and parameters
      df <- as.greta_array(df)
      sigma <- as.greta_array(Sigma)

      # check dimensions of Sigma
      check_sigma_square_2d_greta_array(sigma)

      dim <- nrow(sigma)

      # initialize with a cholesky factor
      super$initialize("wishart", dim(sigma), multivariate = TRUE)
      # set parameters
      if (has_representation(sigma, "cholesky")) {
        sigma <- representation(sigma, "cholesky")
        self$sigma_is_cholesky <- TRUE
      }
      self$add_parameter(df, "df", shape_matches_output = FALSE)
      self$add_parameter(sigma, "sigma")

      # make the initial value PD (no idea whether this does anything)
      self$value(unknowns(dims = c(dim, dim), data = diag(dim)))
    },

    # create a variable, and transform to a symmetric matrix (with cholesky
    # factor representation)
    create_target = function(truncation) {
      # create cholesky factor variable greta array
      chol_greta_array <- cholesky_variable(self$dim[1])

      # reshape to a symmetric matrix (retaining cholesky representation)
      matrix_greta_array <- chol2symm(chol_greta_array)

      # return the node for the symmetric matrix
      target_node <- get_node(matrix_greta_array)

      target_node
    },

    # get a cholesky factor for the target if possible
    get_tf_target_node = function() {
      target <- self$target
      if (has_representation(target, "cholesky")) {
        chol <- representation(target, "cholesky")
        target <- get_node(chol)
        self$target_is_cholesky <- TRUE
      }
      target
    },

    # if the target is changed, make sure target_is_cholesky is reset to FALSE
    # (can be resent on graph definition)
    reset_target_flags = function() {
      self$target_is_cholesky <- FALSE
    },
    tf_distrib = function(parameters, dag) {
      # this is messy, we want to use the tfp wishart, but can't define the
      # density without expanding the dimension of x

      log_prob <- function(x) {
        # reshape the dimensions
        df <- tf_flatten(parameters$df)
        sigma <- tf$expand_dims(parameters$sigma, 1L)
        x <- tf$expand_dims(x, 1L)

        # get the cholesky factor of Sigma in tf orientation
        if (self$sigma_is_cholesky) {
          sigma_chol <- tf$linalg$matrix_transpose(sigma)
        } else {
          sigma_chol <- tf$linalg$cholesky(sigma)
        }

        # get the cholesky factor of the target in tf_orientation
        if (self$target_is_cholesky) {
          x_chol <- tf$linalg$matrix_transpose(x)
        } else {
          x_chol <- tf$linalg$cholesky(x)
        }

        # use the density for choleskied x, with choleskied Sigma
        distrib <- tfp$distributions$WishartTriL(
          df = df,
          scale_tril = sigma_chol,
          input_output_cholesky = TRUE
        )

        log_prob_raw <- distrib$log_prob(x_chol)

        # add an adjustment for the implicit chol2symm bijection in using the
        # choleskied distribution, rather than the symmetric matrix version
        chol2symm_bijector <- tfp$bijectors$CholeskyOuterProduct()
        adjustment <- chol2symm_bijector$forward_log_det_jacobian(x_chol)
        log_prob <- log_prob_raw + adjustment

        log_prob

      }

      sample <- function(seed) {
        df <- tf$squeeze(parameters$df, 1:2)
        sigma <- parameters$sigma

        # get the cholesky factor of Sigma in tf orientation
        if (self$sigma_is_cholesky) {
          sigma_chol <- tf$linalg$matrix_transpose(sigma)
        } else {
          sigma_chol <- tf$linalg$cholesky(sigma)
        }

        # use the density for choleskied x, with choleskied Sigma
        chol_distrib <- tfp$distributions$WishartTriL(
          df = df,
          scale_tril = sigma_chol,
          input_output_cholesky = TRUE
        )

        chol_draws <- chol_distrib$sample(seed = seed)

        # equivalent to (but faster than) tf_chol2symm(tf_transpose(chol_draws))
        # the transpose is needed because TF uses lower triangular
        # (non-zeros are in bottom left)
        # and R uses upper triangular (non zeroes are in top right)
        draws <- tf$matmul(chol_draws, chol_draws, adjoint_b = TRUE)
        draws
      }

      list(log_prob = log_prob, sample = sample)
    }
  )
)

lkj_correlation_distribution <- R6Class(
  "lkj_correlation_distribution",
  inherit = distribution_node,
  public = list(

    # set when defining the graph
    target_is_cholesky = FALSE,
    eta_is_cholesky = FALSE,
    initialize = function(eta, dimension = 2) {
      dimension <- check_dimension(target = dimension)

      if (!is.greta_array(eta)) {
        check_positive_scalar(eta)
      }

      # add the nodes as parents and parameters
      eta <- as.greta_array(eta)

      check_scalar(eta)

      dim <- c(dimension, dimension)
      super$initialize("lkj_correlation", dim, multivariate = TRUE)

      # don't try to expand scalar eta out to match the target size
      self$add_parameter(eta, "eta", shape_matches_output = FALSE)

      # make the initial value PD
      self$value(unknowns(dims = dim, data = diag(dimension)))
    },

    # default (cholesky factor, ignores truncation)
    create_target = function(truncation) {

      # create (correlation matrix) cholesky factor variable greta array
      chol_greta_array <- cholesky_variable(self$dim[1], correlation = TRUE)

      # reshape to a symmetric matrix (retaining cholesky representation)
      matrix_greta_array <- chol2symm(chol_greta_array)

      # return the node for the symmetric matrix
      target_node <- get_node(matrix_greta_array)
      target_node
    },

    # NOTE: this code is repeated above on line 1032, is that intended?
    # get a cholesky factor for the target if possible
    get_tf_target_node = function() {
      target <- self$target
      if (has_representation(target, "cholesky")) {
        chol <- representation(target, "cholesky")
        target <- get_node(chol)
        self$target_is_cholesky <- TRUE
      }
      target
    },

    # if the target is changed, make sure target_is_cholesky is reset to FALSE
    # (can be resent on graph definition)
    reset_target_flags = function() {
      self$target_is_cholesky <- FALSE
    },
    tf_distrib = function(parameters, dag) {
      eta <- tf$squeeze(parameters$eta, 1:2)
      dim <- self$dim[1]

      log_prob <- function(x){
        if (self$target_is_cholesky) {
          x_chol <- tf$linalg$matrix_transpose(x)
        } else {
          x_chol <- tf$linalg$cholesky(x)
        }

        chol_distrib <- tfp$distributions$CholeskyLKJ(
          dimension = dim,
          concentration = eta
        )

        # NOTE there seems to be a difference with our implementation of
        # normalising constant of the log prob. So we need to find a different
        # reference implementation of the normalising constant. This does not
        # impact MCMC or sampling, but may affect future uses of this.
        # e.g., the integration and marginalisation interface
        # chol_distrib$log_prob(x_chol)

        log_prob_raw <- chol_distrib$log_prob(x_chol)

        # add an adjustment for the implicit chol2symm bijection in using the
        # choleskied distribution, rather than the symmetric matrix version
        chol2symm_bijector <- tfp$bijectors$CholeskyOuterProduct()
        adjustment <- chol2symm_bijector$forward_log_det_jacobian(x_chol)
        log_prob <- log_prob_raw + adjustment

        log_prob

      }

      # tfp's lkj sampling can't detect the size of the output from eta, for
      # some reason. But we can use map_fn to apply their simulation to each
      # element of eta.
      sample <- function(seed) {
        sample_once <- function(eta) {
          d <- tfp$distributions$CholeskyLKJ(
            dimension = dim,
            concentration = eta
          )

          chol_draws <- d$sample(seed = seed)

          # equivalent to (but faster than) tf_chol2symm(tf_transpose(chol_draws))
          # the transpose is needed because TF uses lower triangular
          # (non-zeros are in bottom left)
          # and R uses upper triangular (non zeroes are in top right)
          draws <- tf$matmul(chol_draws, chol_draws, adjoint_b = TRUE)
          draws

        }

        tf$map_fn(sample_once, eta)
      }

      list(
        log_prob = log_prob,
        sample = sample
      )
    }
  )
)

# module for export via .internals
distribution_classes_module <- module(
  uniform_distribution,
  normal_distribution,
  lognormal_distribution,
  bernoulli_distribution,
  binomial_distribution,
  beta_binomial_distribution,
  negative_binomial_distribution,
  hypergeometric_distribution,
  poisson_distribution,
  gamma_distribution,
  inverse_gamma_distribution,
  weibull_distribution,
  exponential_distribution,
  pareto_distribution,
  student_distribution,
  laplace_distribution,
  beta_distribution,
  cauchy_distribution,
  chi_squared_distribution,
  logistic_distribution,
  f_distribution,
  multivariate_normal_distribution,
  wishart_distribution,
  lkj_correlation_distribution,
  multinomial_distribution,
  categorical_distribution,
  dirichlet_distribution,
  dirichlet_multinomial_distribution
)

# export constructors

# nolint start
#' @name distributions
#' @title probability distributions
#' @description These functions can be used to define random variables in a
#'   greta model. They return a variable greta array that follows the specified
#'   distribution. This variable greta array can be used to represent a
#'   parameter with prior distribution, combined into a mixture distribution
#'   using [mixture()], or used with [distribution()] to
#'   define a distribution over a data greta array.
#'
#' @param truncation a length-two vector giving values between which to truncate
#'   the distribution, similarly to the `lower` and `upper` arguments
#'   to [variable()]
#'
#' @param min,max scalar values giving optional limits to `uniform`
#'   variables. Like `lower` and `upper`, these must be specified as
#'   numerics, they cannot be greta arrays (though see details for a
#'   workaround). Unlike `lower` and `upper`, they must be finite.
#'   `min` must always be less than `max`.
#'
#' @param mean,meanlog,location,mu unconstrained parameters
#'
#' @param
#'   sd,sdlog,sigma,lambda,shape,rate,df,scale,shape1,shape2,alpha,beta,df1,df2,a,b,eta
#'    positive parameters, `alpha` must be a vector for `dirichlet`
#'   and `dirichlet_multinomial`.
#'
#' @param size,m,n,k positive integer parameter
#'
#' @param prob probability parameter (`0 < prob < 1`), must be a vector for
#'   `multinomial` and `categorical`
#'
#' @param Sigma positive definite variance-covariance matrix parameter
#'
#' @param dim the dimensions of the greta array to be returned, either a scalar
#'   or a vector of positive integers. See details.
#'
#' @param dimension the dimension of a multivariate distribution
#'
#' @param n_realisations the number of independent realisation of a multivariate
#'   distribution
#'
#' @details The discrete probability distributions (`bernoulli`,
#'   `binomial`, `negative_binomial`, `poisson`,
#'   `multinomial`, `categorical`, `dirichlet_multinomial`) can
#'   be used when they have fixed values (e.g. defined as a likelihood using
#'   [distribution()], but not as unknown variables.
#'
#'   For univariate distributions `dim` gives the dimensions of the greta
#'   array to create. Each element of the greta array will be (independently)
#'   distributed according to the distribution. `dim` can also be left at
#'   its default of `NULL`, in which case the dimension will be detected
#'   from the dimensions of the parameters (provided they are compatible with
#'   one another).
#'
#'   For multivariate distributions (`multivariate_normal()`,
#'   `multinomial()`, `categorical()`, `dirichlet()`, and
#'   `dirichlet_multinomial()`) each row of the output and parameters
#'   corresponds to an independent realisation. If a single realisation or
#'   parameter value is specified, it must therefore be a row vector (see
#'   example). `n_realisations` gives the number of rows/realisations, and
#'   `dimension` gives the dimension of the distribution. I.e. a bivariate
#'   normal distribution would be produced with `multivariate_normal(...,
#'   dimension = 2)`. The dimension can usually be detected from the parameters.
#'
#'   `multinomial()` does not check that observed values sum to
#'   `size`, and `categorical()` does not check that only one of the
#'   observed entries is 1. It's the user's responsibility to check their data
#'   matches the distribution!
#'
#'   The parameters of `uniform` must be fixed, not greta arrays. This
#'   ensures these values can always be transformed to a continuous scale to run
#'   the samplers efficiently. However, a hierarchical `uniform` parameter
#'   can always be created by defining a `uniform` variable constrained
#'   between 0 and 1, and then transforming it to the required scale. See below
#'   for an example.
#'
#'   Wherever possible, the parameterisations and argument names of greta
#'   distributions match commonly used R functions for distributions, such as
#'   those in the `stats` or `extraDistr` packages. The following
#'   table states the distribution function to which greta's implementation
#'   corresponds:
#'
#'   \tabular{ll}{ greta \tab reference\cr `uniform` \tab
#'   [stats::dunif]\cr `normal` \tab
#'   [stats::dnorm]\cr `lognormal` \tab
#'   [stats::dlnorm]\cr `bernoulli` \tab
#'   [extraDistr::dbern]\cr `binomial` \tab
#'   [stats::dbinom]\cr `beta_binomial` \tab
#'   [extraDistr::dbbinom]\cr `negative_binomial`
#'   \tab [stats::dnbinom]\cr `hypergeometric` \tab
#'   [stats::dhyper]\cr `poisson` \tab
#'   [stats::dpois]\cr `gamma` \tab
#'   [stats::dgamma]\cr `inverse_gamma` \tab
#'   [extraDistr::dinvgamma]\cr `weibull` \tab
#'   [stats::dweibull]\cr `exponential` \tab
#'   [stats::dexp]\cr `pareto` \tab
#'   [extraDistr::dpareto]\cr `student` \tab
#'   [extraDistr::dlst]\cr `laplace` \tab
#'   [extraDistr::dlaplace]\cr `beta` \tab
#'   [stats::dbeta]\cr `cauchy` \tab
#'   [stats::dcauchy]\cr `chi_squared` \tab
#'   [stats::dchisq]\cr `logistic` \tab
#'   [stats::dlogis]\cr `f` \tab
#'   [stats::df]\cr `multivariate_normal` \tab
#'   [mvtnorm::dmvnorm]\cr `multinomial` \tab
#'   [stats::dmultinom]\cr `categorical` \tab
#'   {[stats::dmultinom] (size = 1)}\cr `dirichlet`
#'   \tab [extraDistr::ddirichlet]\cr
#'   `dirichlet_multinomial` \tab
#'   [extraDistr::ddirmnom]\cr `wishart` \tab
#'   [stats::rWishart]\cr `lkj_correlation` \tab
#'   [rethinking::dlkjcorr](https://rdrr.io/github/rmcelreath/rethinking/man/dlkjcorr.html)
#'   }
#'
#' @examples
#' \dontrun{
#'
#' # a uniform parameter constrained to be between 0 and 1
#' phi <- uniform(min = 0, max = 1)
#'
#' # a length-three variable, with each element following a standard normal
#' # distribution
#' alpha <- normal(0, 1, dim = 3)
#'
#' # a length-three variable of lognormals
#' sigma <- lognormal(0, 3, dim = 3)
#'
#' # a hierarchical uniform, constrained between alpha and alpha + sigma,
#' eta <- alpha + uniform(0, 1, dim = 3) * sigma
#'
#' # a hierarchical distribution
#' mu <- normal(0, 1)
#' sigma <- lognormal(0, 1)
#' theta <- normal(mu, sigma)
#'
#' # a vector of 3 variables drawn from the same hierarchical distribution
#' thetas <- normal(mu, sigma, dim = 3)
#'
#' # a matrix of 12 variables drawn from the same hierarchical distribution
#' thetas <- normal(mu, sigma, dim = c(3, 4))
#'
#' # a multivariate normal variable, with correlation between two elements
#' # note that the parameter must be a row vector
#' Sig <- diag(4)
#' Sig[3, 4] <- Sig[4, 3] <- 0.6
#' theta <- multivariate_normal(t(rep(mu, 4)), Sig)
#'
#' # 10 independent replicates of that
#' theta <- multivariate_normal(t(rep(mu, 4)), Sig, n_realisations = 10)
#'
#' # 10 multivariate normal replicates, each with a different mean vector,
#' # but the same covariance matrix
#' means <- matrix(rnorm(40), 10, 4)
#' theta <- multivariate_normal(means, Sig, n_realisations = 10)
#' dim(theta)
#'
#' # a Wishart variable with the same covariance parameter
#' theta <- wishart(df = 5, Sigma = Sig)
#' }
NULL
# nolint end

#' @rdname distributions
#' @export
uniform <- function(min, max, dim = NULL) {
  distrib("uniform", min, max, dim)
}

#' @rdname distributions
#' @export
normal <- function(mean, sd, dim = NULL, truncation = c(-Inf, Inf)) {
  distrib("normal", mean, sd, dim, truncation)
}

#' @rdname distributions
#' @export
lognormal <- function(meanlog, sdlog, dim = NULL, truncation = c(0, Inf)) {
  distrib("lognormal", meanlog, sdlog, dim, truncation)
}

#' @rdname distributions
#' @export
bernoulli <- function(prob, dim = NULL) {
  distrib("bernoulli", prob, dim)
}

#' @rdname distributions
#' @export
binomial <- function(size, prob, dim = NULL) {
  check_in_family("binomial", size)
  distrib("binomial", size, prob, dim)
}

#' @rdname distributions
#' @export
beta_binomial <- function(size, alpha, beta, dim = NULL) {
  distrib("beta_binomial", size, alpha, beta, dim)
}

#' @rdname distributions
#' @export
negative_binomial <- function(size, prob, dim = NULL) {
  distrib("negative_binomial", size, prob, dim)
}

#' @rdname distributions
#' @export
hypergeometric <- function(m, n, k, dim = NULL) {
  distrib("hypergeometric", m, n, k, dim)
}

#' @rdname distributions
#' @export
poisson <- function(lambda, dim = NULL) {
  check_in_family("poisson", lambda)
  distrib("poisson", lambda, dim)
}

#' @rdname distributions
#' @export
gamma <- function(shape, rate, dim = NULL, truncation = c(0, Inf)) {
  distrib("gamma", shape, rate, dim, truncation)
}

#' @rdname distributions
#' @export
inverse_gamma <- function(alpha, beta, dim = NULL, truncation = c(0, Inf)) {
  distrib("inverse_gamma", alpha, beta, dim, truncation)
}

#' @rdname distributions
#' @export
weibull <- function(shape, scale, dim = NULL, truncation = c(0, Inf)) {
  distrib("weibull", shape, scale, dim, truncation)
}

#' @rdname distributions
#' @export
exponential <- function(rate, dim = NULL, truncation = c(0, Inf)) {
  distrib("exponential", rate, dim, truncation)
}

#' @rdname distributions
#' @export
pareto <- function(a, b, dim = NULL, truncation = c(0, Inf)) {
  distrib("pareto", a, b, dim, truncation)
}

#' @rdname distributions
#' @export
student <- function(df, mu, sigma, dim = NULL, truncation = c(-Inf, Inf)) {
  distrib("student", df, mu, sigma, dim, truncation)
}

#' @rdname distributions
#' @export
laplace <- function(mu, sigma, dim = NULL, truncation = c(-Inf, Inf)) {
  distrib("laplace", mu, sigma, dim, truncation)
}

#' @rdname distributions
#' @export
beta <- function(shape1, shape2, dim = NULL, truncation = c(0, 1)) {
  distrib("beta", shape1, shape2, dim, truncation)
}

#' @rdname distributions
#' @export
cauchy <- function(location, scale, dim = NULL, truncation = c(-Inf, Inf)) {
  distrib("cauchy", location, scale, dim, truncation)
}

#' @rdname distributions
#' @export
chi_squared <- function(df, dim = NULL, truncation = c(0, Inf)) {
  distrib("chi_squared", df, dim, truncation)
}

#' @rdname distributions
#' @export
logistic <- function(location, scale, dim = NULL, truncation = c(-Inf, Inf)) {
  distrib("logistic", location, scale, dim, truncation)
}

#' @rdname distributions
#' @export
f <- function(df1, df2, dim = NULL, truncation = c(0, Inf)) {
  distrib("f", df1, df2, dim, truncation)
}

# nolint start
#' @rdname distributions
#' @export
multivariate_normal <- function(mean, Sigma,
                                n_realisations = NULL, dimension = NULL) {
  # nolint end
  distrib(
    "multivariate_normal", mean, Sigma,
    n_realisations, dimension
  )
}

#' @rdname distributions
#' @export
wishart <- function(df, Sigma) { # nolint
  distrib("wishart", df, Sigma)
}

#' @rdname distributions
#' @export
lkj_correlation <- function(eta, dimension = 2) {
  distrib("lkj_correlation", eta, dimension)
}

#' @rdname distributions
#' @export
multinomial <- function(size, prob, n_realisations = NULL, dimension = NULL) {
  distrib("multinomial", size, prob, n_realisations, dimension)
}

#' @rdname distributions
#' @export
categorical <- function(prob, n_realisations = NULL, dimension = NULL) {
  distrib("categorical", prob, n_realisations, dimension)
}

#' @rdname distributions
#' @export
dirichlet <- function(alpha, n_realisations = NULL, dimension = NULL) {
  distrib("dirichlet", alpha, n_realisations, dimension)
}

#' @rdname distributions
#' @export
dirichlet_multinomial <- function(size, alpha,
                                  n_realisations = NULL, dimension = NULL) {
  distrib(
    "dirichlet_multinomial",
    size, alpha, n_realisations, dimension
  )
}
