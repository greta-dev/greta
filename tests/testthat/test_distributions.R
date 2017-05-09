context('distributions')

test_that('normal distribution has correct density', {

  source('helpers.R')

  flush()

  difference <- compare_distribution(greta::normal,
                                     stats::dnorm,
                                     parameters = list(mean = -2, sd = 3),
                                     x = rnorm(100, -2, 3))

  expect_true(all(difference < 1e-4))

})

test_that('uniform distribution has correct density', {

  source('helpers.R')

  flush()

  difference <- compare_distribution(greta::uniform,
                                     stats::dunif,
                                     parameters = list(min = -2.1, max = -1.2),
                                     x = runif(100, -2.1, -1.2))

  expect_true(all(difference < 1e-4))

})

test_that('lognormal distribution has correct density', {

  source('helpers.R')

  flush()

  difference <- compare_distribution(greta::lognormal,
                                     stats::dlnorm,
                                     parameters = list(meanlog = 1, sdlog = 3),
                                     x = rlnorm(100, 1, 3))

  expect_true(all(difference < 1e-4))

})

test_that('bernoulli distribution has correct density', {

  source('helpers.R')

  flush()

  # r version of the bernoulli density
  dbern <- function (x, prob, log = FALSE)
    dbinom(x, size = 1, prob = prob, log = log)

  difference <- compare_distribution(greta::bernoulli,
                                     dbern,
                                     parameters = list(prob = 0.3),
                                     x = rbinom(100, 1, 0.3))

  expect_true(all(difference < 1e-4))

})

test_that('binomial distribution has correct density', {

  source('helpers.R')

  flush()

  difference <- compare_distribution(greta::binomial,
                                     stats::dbinom,
                                     parameters = list(size = 10, prob = 0.8),
                                     x = rbinom(100, 10, 0.8))

  expect_true(all(difference < 1e-4))

})

test_that('negative binomial distribution has correct density', {

  source('helpers.R')

  flush()

  difference <- compare_distribution(greta::negative_binomial,
                                     stats::dnbinom,
                                     parameters = list(size = 3.3, prob = 0.2),
                                     x = rnbinom(100, 3.3, 0.2))

  expect_true(all(difference < 1e-4))

})

test_that('poisson distribution has correct density', {

  source('helpers.R')

  flush()

  difference <- compare_distribution(greta::poisson,
                                     stats::dpois,
                                     parameters = list(lambda = 17.2),
                                     x = rpois(100, 17.2))

  expect_true(all(difference < 1e-4))

})

test_that('gamma distribution has correct density', {

  source('helpers.R')

  flush()

  difference <- compare_distribution(greta::gamma,
                                     stats::dgamma,
                                     parameters = list(shape = 1.2, rate = 2.3),
                                     x = rgamma(100, 1.2, 2.3))

  expect_true(all(difference < 1e-4))

})

test_that('exponential distribution has correct density', {

  source('helpers.R')

  flush()

  difference <- compare_distribution(greta::exponential,
                                     stats::dexp,
                                     parameters = list(rate = 1.9),
                                     x = rexp(100, 1.9))

  expect_true(all(difference < 1e-4))

})

test_that('student distribution has correct density', {

  source('helpers.R')

  flush()

  # use location-scale version of student T; related to R's via this function:
  dt_ls <- function (x, df, location, scale, log = FALSE) {
    ans <- stats::dt((x - location) / scale, df) / scale
    if (log)
      ans <- log(ans)
    ans
  }

  difference <- compare_distribution(greta::student,
                                     dt_ls,
                                     parameters = list(df = 3, location = -0.9, scale = 2),
                                     x = rnorm(100, -0.9, 2))

  expect_true(all(difference < 1e-4))

})

test_that('beta distribution has correct density', {

  source('helpers.R')

  flush()

  difference <- compare_distribution(greta::beta,
                                     stats::dbeta,
                                     parameters = list(shape1 = 2.3, shape2 = 3.4),
                                     x = rbeta(100, 2.3, 3.4))

  expect_true(all(difference < 1e-4))

})

test_that('multivariate normal distribution has correct density', {

  source('helpers.R')

  flush()

  # parameters to test
  m <- 5
  mn <- rnorm(m)
  sig <- rWishart(1, m + 1, diag(m))[, , 1]

  # function converting Sigma to sigma
  dmvnorm2 <- function (x, mean, Sigma, log = FALSE)
    mvtnorm::dmvnorm(x = x, mean = mean, sigma = Sigma, log = log)

  difference <- compare_distribution(greta::multivariate_normal,
                                     dmvnorm2,
                                     parameters = list(mean = mn, Sigma = sig),
                                     x = mvtnorm::rmvnorm(100, mn, sig))

  expect_true(all(difference < 1e-4))

})

test_that('Wishart distribution has correct density', {

  source('helpers.R')

  flush()

  # parameters to test
  m <- 5
  df <- m + 1
  sig <- rWishart(1, df, diag(m))[, , 1]

  # wrapper for argument names
  dwishart <- function (x, df, Sigma, log = FALSE) {
    ans <- MCMCpack::dwish(W = x, v = df, S = Sigma)
    if (log)
      ans <- log(ans)
    ans
  }

  # no vectorised wishart, so loop through all of these
  difference <- replicate(10,
                          compare_distribution(greta::wishart,
                                               dwishart,
                                               parameters = list(df = df, Sigma = sig),
                                               x = rWishart(1, df, sig)[, , 1]))

  expect_true(all(difference < 1e-4))

})

test_that('scalar-valued distributions can be defined in models', {

  source('helpers.R')

  flush()

  x <- randn(5)
  y <- round(randu(5))
  p <- iprobit(normal(0, 1))

  # density-free and discrete data need a bit of help
  define_model((distribution(x) = normal(free(), 1)))
  define_model((distribution(y) = bernoulli(p)))
  define_model((distribution(y) = binomial(1, p)))
  define_model((distribution(y) = negative_binomial(1, p)))
  define_model((distribution(y) = poisson(p)))

  flush()

  # continuous distributions
  define_model(normal(-2, 3))
  define_model(student(5.6, -2, 2.3))
  define_model(lognormal(1.2, 0.2))

  flush()

  define_model(gamma(0.9, 1.3))
  define_model(exponential(6.3))
  define_model(beta(6.3, 5.9))

  flush()

  define_model(uniform(-13, 2.4))
  sig <- rWishart(4, 3, diag(3))[, , 1]
  define_model(multivariate_normal(rnorm(3), sig))
  define_model(wishart(4, sig))

})

test_that('array-valued distributions can be defined in models', {

  source('helpers.R')

  flush()

  dim <- c(5, 2)
  x <- randn(5, 2)
  y <- round(randu(5, 2))
  p <- iprobit(normal(0, 1, dim = dim))

  # density-free and discrete data need a bit of help
  define_model((distribution(x) = normal(free(dim = dim), 1)))
  define_model((distribution(y) = bernoulli(p)))
  define_model((distribution(y) = binomial(1, p)))
  define_model((distribution(y) = negative_binomial(1, p)))
  define_model((distribution(y) = poisson(p)))

  flush()

  # continuous distributions
  define_model(normal(-2, 3, dim = dim))
  define_model(student(5.6, -2, 2.3, dim = dim))
  define_model(lognormal(1.2, 0.2, dim = dim))

  flush()

  define_model(gamma(0.9, 1.3, dim = dim))
  define_model(exponential(6.3, dim = dim))
  define_model(beta(6.3, 5.9, dim = dim))
  define_model(uniform(-13, 2.4, dim = dim))

  flush()

  sig <- rWishart(4, 3, diag(3))[, , 1]
  define_model(multivariate_normal(rnorm(3), sig, dim = dim[1]))

})

test_that('distributions can be sampled from', {

  source('helpers.R')

  flush()

  x <- randn(100)
  y <- round(randu(100))
  p <- iprobit(normal(0, 1, dim = 100))

  # free (with a density)
  sample_distribution((distribution(x) = normal(free(), 1)))
  sample_distribution((distribution(x) = normal(free(lower = -1), 1)))
  sample_distribution((distribution(x) = normal(free(upper = -2), 1)))
  sample_distribution((distribution(x) = normal(free(lower = 1.2, upper = 1.3), 1)))

  # discrete
  sample_distribution((distribution(y) = bernoulli(p)))
  sample_distribution((distribution(y) = binomial(1, p)))
  sample_distribution((distribution(y) = negative_binomial(1, p)))
  sample_distribution((distribution(y) = poisson(p)))

  flush()

  # unconstrained
  sample_distribution(normal(-2, 3))
  sample_distribution(student(5.6, -2, 2.3))

  flush()

  # positive
  sample_distribution(lognormal(1.2, 0.2), lower = 0)
  sample_distribution(gamma(0.9, 1.3), lower = 0)
  sample_distribution(exponential(6.3), lower = 0)

  flush()

  # constrained
  sample_distribution(beta(6.3, 5.9), lower = 0, upper = 1)
  sample_distribution(uniform(-13, 2.4), lower = -13, upper = 2.4)

  flush()

  # multivariate
  sig <- rWishart(4, 3, diag(3))[, , 1]
  sample_distribution(multivariate_normal(rnorm(3), sig))
  sample_distribution(wishart(4, sig))

})


test_that('free distribution errors informatively', {

  source('helpers.R')

  flush()

  # bad types
  expect_error(free(upper = NA),
               'lower and upper must be numeric vectors of length 1')
  expect_error(free(upper = head),
               'lower and upper must be numeric vectors of length 1')
  expect_error(free(lower = 1:3),
               'lower and upper must be numeric vectors of length 1')

  # good types, bad values
  expect_error(free(lower = Inf),
               '^lower and upper must either be')
  expect_error(free(upper = -Inf),
               '^lower and upper must either be')

  # lower >= upper
  expect_error(free(lower = 1, upper = 1),
               'upper bound must be greater than lower bound')

})

test_that('uniform distribution errors informatively', {

  source('helpers.R')

  flush()

  # bad types
  expect_error(uniform(min = 0, max = NA),
               'min and max must be numeric vectors of length 1')
  expect_error(uniform(min = 0, max = head),
               'min and max must be numeric vectors of length 1')
  expect_error(uniform(min = 1:3, max = 5),
               'min and max must be numeric vectors of length 1')

  # good types, bad values
  expect_error(uniform(min = -Inf, max = Inf),
               'min and max must finite scalars')

  # lower >= upper
  expect_error(uniform(min = 1, max = 1),
               'max must be greater than min')

})

test_that('wishart distribution errors informatively', {

  source('helpers.R')

  flush()

  a <- randn(3, 3)
  b <- randn(3, 3, 3)
  c <- randn(3, 2)

  expect_true(inherits(wishart(3, a),
                       'greta_array'))
  expect_error(wishart(3, b),
               '^Sigma must be a square 2D greta array, but has dimensions')
  expect_error(wishart(3, c),
               '^Sigma must be a square 2D greta array, but has dimensions')


})

test_that('multivariate_normal distribution errors informatively', {

  source('helpers.R')

  flush()

  m_a <- randn(3)
  m_b <- randn(3, 1)
  m_c <- randn(1, 3)
  m_d <- randn(3, 2)

  a <- randn(3, 3)
  b <- randn(3, 3, 3)
  c <- randn(3, 2)
  d <- randn(4, 4)

  # good means
  expect_true(inherits(multivariate_normal(m_a, a),
                       'greta_array'))

  expect_true(inherits(multivariate_normal(m_b, a),
                       'greta_array'))

  # bad means
  expect_error(multivariate_normal(m_c, a),
                       'mean must be a 2D greta array with one column, but has dimensions 1 x 3')

  expect_error(multivariate_normal(m_d, a),
                       'mean must be a 2D greta array with one column, but has dimensions 3 x 2')

  # good sigmas
  expect_true(inherits(multivariate_normal(m_a, a),
                       'greta_array'))

  # bad sigmas
  expect_error(multivariate_normal(m_a, b),
               'Sigma must be a square 2D greta array, but has dimensions 3 x 3 x 3')
  expect_error(multivariate_normal(m_a, c),
               'Sigma must be a square 2D greta array, but has dimensions 3 x 2')

  # mismatched parameters
  expect_error(multivariate_normal(m_a, d),
               'mean and Sigma have different dimensions, 3 vs 4')

  # scalars
  expect_error(multivariate_normal(0, 1),
               'the multivariate normal distribution is for vectors, but the parameters were scalar')

  # bad dim
  expect_error(multivariate_normal(m_a, a, dim = -1),
               'dim must be a scalar positive integer, but was: -1')
  expect_error(multivariate_normal(m_a, a, dim = c(1, 3)),
               '^dim must be a scalar positive integer, but was:')

})

# sample free with different constraints
