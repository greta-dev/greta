context('distributions')

test_that('normal distribution has correct density', {

  skip_if_not(check_tf_version())
  source('helpers.R')

  difference <- compare_distribution(greta::normal,
                                     stats::dnorm,
                                     parameters = list(mean = -2, sd = 3),
                                     x = rnorm(100, -2, 3))

  expect_true(all(difference < 1e-4))

})

test_that('uniform distribution has correct density', {

  skip_if_not(check_tf_version())
  source('helpers.R')

  difference <- compare_distribution(greta::uniform,
                                     stats::dunif,
                                     parameters = list(min = -2.1, max = -1.2),
                                     x = runif(100, -2.1, -1.2))

  expect_true(all(difference < 1e-4))

})

test_that('lognormal distribution has correct density', {

  skip_if_not(check_tf_version())
  source('helpers.R')

  difference <- compare_distribution(greta::lognormal,
                                     stats::dlnorm,
                                     parameters = list(meanlog = 1, sdlog = 3),
                                     x = rlnorm(100, 1, 3))

  expect_true(all(difference < 1e-4))

})

test_that('bernoulli distribution has correct density', {

  skip_if_not(check_tf_version())
  source('helpers.R')

  difference <- compare_distribution(greta::bernoulli,
                                     extraDistr::dbern,
                                     parameters = list(prob = 0.3),
                                     x = rbinom(100, 1, 0.3))

  expect_true(all(difference < 1e-4))

})

test_that('binomial distribution has correct density', {

  skip_if_not(check_tf_version())
  source('helpers.R')

  difference <- compare_distribution(greta::binomial,
                                     stats::dbinom,
                                     parameters = list(size = 10, prob = 0.8),
                                     x = rbinom(100, 10, 0.8))

  expect_true(all(difference < 1e-4))

})

test_that('beta-binomial distribution has correct density', {

  skip_if_not(check_tf_version())
  source('helpers.R')

  difference <- compare_distribution(greta::beta_binomial,
                                     extraDistr::dbbinom,
                                     parameters = list(size = 10, alpha = 0.8, beta = 1.2),
                                     x = extraDistr::rbbinom(100, 10, 0.8, 1.2))

  expect_true(all(difference < 1e-4))

})

test_that('negative binomial distribution has correct density', {

  skip_if_not(check_tf_version())
  source('helpers.R')

  difference <- compare_distribution(greta::negative_binomial,
                                     stats::dnbinom,
                                     parameters = list(size = 3.3, prob = 0.2),
                                     x = rnbinom(100, 3.3, 0.2))

  expect_true(all(difference < 1e-4))

})

test_that('hypergeometric distribution has correct density', {

  skip_if_not(check_tf_version())
  source('helpers.R')

  difference <- compare_distribution(greta::hypergeometric,
                                     stats::dhyper,
                                     parameters = list(m = 11, n = 8, k = 5),
                                     x = rhyper(100, 11, 8, 5))

  expect_true(all(difference < 1e-4))

})

test_that('poisson distribution has correct density', {

  skip_if_not(check_tf_version())
  source('helpers.R')

  difference <- compare_distribution(greta::poisson,
                                     stats::dpois,
                                     parameters = list(lambda = 17.2),
                                     x = rpois(100, 17.2))

  expect_true(all(difference < 1e-4))

})

test_that('gamma distribution has correct density', {

  skip_if_not(check_tf_version())
  source('helpers.R')

  difference <- compare_distribution(greta::gamma,
                                     stats::dgamma,
                                     parameters = list(shape = 1.2, rate = 2.3),
                                     x = rgamma(100, 1.2, 2.3))

  expect_true(all(difference < 1e-4))

})


test_that('inverse gamma distribution has correct density', {

  skip_if_not(check_tf_version())
  source('helpers.R')

  difference <- compare_distribution(greta::inverse_gamma,
                                     extraDistr::dinvgamma,
                                     parameters = list(alpha = 1.2, beta = 0.9),
                                     x = extraDistr::rinvgamma(100, 1.2, 0.9))

  expect_true(all(difference < 1e-4))

})

test_that('weibull distribution has correct density', {

  skip_if_not(check_tf_version())
  source('helpers.R')

  difference <- compare_distribution(greta::weibull,
                                     dweibull,
                                     parameters = list(shape = 1.2, scale = 0.9),
                                     x = rweibull(100, 1.2, 0.9))

  expect_true(all(difference < 1e-4))

})

test_that('exponential distribution has correct density', {

  skip_if_not(check_tf_version())
  source('helpers.R')

  difference <- compare_distribution(greta::exponential,
                                     stats::dexp,
                                     parameters = list(rate = 1.9),
                                     x = rexp(100, 1.9))

  expect_true(all(difference < 1e-4))

})

test_that('pareto distribution has correct density', {

  skip_if_not(check_tf_version())
  source('helpers.R')

  difference <- compare_distribution(greta::pareto,
                                     extraDistr::dpareto,
                                     parameters = list(a = 1.9, b = 2.3),
                                     x = extraDistr::rpareto(100, 1.9, 2.3))

  expect_true(all(difference < 1e-4))

})

test_that('student distribution has correct density', {

  skip_if_not(check_tf_version())
  source('helpers.R')

  difference <- compare_distribution(greta::student,
                                     dstudent,
                                     parameters = list(df = 3, mu = -0.9, sigma = 2),
                                     x = rnorm(100, -0.9, 2))

  expect_true(all(difference < 1e-4))

})

test_that('laplace distribution has correct density', {

  skip_if_not(check_tf_version())
  source('helpers.R')

  difference <- compare_distribution(greta::laplace,
                                     extraDistr::dlaplace,
                                     parameters = list(mu = -0.9, sigma = 2),
                                     x = extraDistr::rlaplace(100, -0.9, 2))

  expect_true(all(difference < 1e-4))

})

test_that('beta distribution has correct density', {

  skip_if_not(check_tf_version())
  source('helpers.R')

  difference <- compare_distribution(greta::beta,
                                     stats::dbeta,
                                     parameters = list(shape1 = 2.3, shape2 = 3.4),
                                     x = rbeta(100, 2.3, 3.4))

  expect_true(all(difference < 1e-4))

})

test_that('cauchy distribution has correct density', {

  skip_if_not(check_tf_version())
  source('helpers.R')

  difference <- compare_distribution(greta::cauchy,
                                     stats::dcauchy,
                                     parameters = list(location = -1.3, scale = 3.4),
                                     x = rcauchy(100, -1.3, 3.4))

  expect_true(all(difference < 1e-4))

})

test_that('logistic distribution has correct density', {

  skip_if_not(check_tf_version())
  source('helpers.R')

  difference <- compare_distribution(greta::logistic,
                                     stats::dlogis,
                                     parameters = list(location = -1.3,
                                                       scale = 2.1),
                                     x = rlogis(100, -1.3, 2.1))

  expect_true(all(difference < 1e-4))

})

test_that('f distribution has correct density', {

  skip_if_not(check_tf_version())
  source('helpers.R')

  difference <- compare_distribution(greta::f,
                                     df,
                                     parameters = list(df1 = 5.9, df2 = 2),
                                     x = rf(100, 5.9, 2))

  expect_true(all(difference < 1e-4))

})

test_that('chi squared distribution has correct density', {

  skip_if_not(check_tf_version())
  source('helpers.R')

  difference <- compare_distribution(greta::chi_squared,
                                     stats::dchisq,
                                     parameters = list(df = 9.3),
                                     x = rchisq(100, 9.3))

  expect_true(all(difference < 1e-4))

})

test_that('multivariate normal distribution has correct density', {

  skip_if_not(check_tf_version())
  source('helpers.R')

  # parameters to test
  m <- 5
  mn <- rnorm(m)
  sig <- MCMCpack::rwish(m + 1, diag(m))

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

  skip_if_not(check_tf_version())
  source('helpers.R')

  # parameters to test
  m <- 5
  df <- m + 1
  sig <- MCMCpack::rwish(df, diag(m))

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
                                               x = MCMCpack::rwish(df, sig)))

  expect_true(all(difference < 1e-4))

})

test_that('lkj distribution has correct density', {

  skip_if_not(check_tf_version())
  source('helpers.R')

  # parameters to test
  m <- 5
  eta <- 3

  # lkj  density
  dlkj_correlation <- function (x, eta, log = FALSE) {
    res <- det(x) ^ (eta - 1)
    if (log) res <- log(res)
    return (res)
  }

  rcorrelation <- function (m) {
    wish <- MCMCpack::rwish(m + 1, diag(m))
    iwish <- solve(wish)
    cov2cor(iwish)
  }

  # no vectorised lkj, so loop through all of these
  difference <- replicate(10,
                          compare_distribution(greta::lkj_correlation,
                                               dlkj_correlation,
                                               parameters = list(eta = eta),
                                               x = rcorrelation(m)))

  expect_true(all(difference < 1e-4))

})

test_that('multinomial distribution has correct density', {

  skip_if_not(check_tf_version())
  source('helpers.R')

  # parameters to test
  m <- 5
  prob <- runif(m)
  size <- 5

  # vectorise R's density function
  dmultinom_vec <- function (x, size, prob)
    apply(x, 1, stats::dmultinom, size = size, prob = prob)

  difference <- compare_distribution(greta::multinomial,
                                     dmultinom_vec,
                                     parameters = list(size = size, prob = prob),
                                     x = t(rmultinom(100, size, prob)))

  expect_true(all(difference < 1e-4))

})

test_that('categorical distribution has correct density', {

  skip_if_not(check_tf_version())
  source('helpers.R')

  # parameters to test
  m <- 5
  prob <- runif(m)

  # vectorise R's density function
  dcategorical_vec <- function (x, prob)
    apply(x, 1, stats::dmultinom, size = 1, prob = prob)

  difference <- compare_distribution(greta::categorical,
                                     dcategorical_vec,
                                     parameters = list(prob = prob),
                                     x = t(rmultinom(100, 1, prob)))

  expect_true(all(difference < 1e-4))

})

test_that('dirichlet distribution has correct density', {

  skip_if_not(check_tf_version())
  source('helpers.R')

  # parameters to test
  m <- 5
  alpha <- runif(m)

  difference <- compare_distribution(greta::dirichlet,
                                     extraDistr::ddirichlet,
                                     parameters = list(alpha = alpha),
                                     x = extraDistr::rdirichlet(100, alpha))

  expect_true(all(difference < 1e-4))

})

test_that('dirichlet-multinomial distribution has correct density', {

  skip_if_not(check_tf_version())
  source('helpers.R')

  # parameters to test
  m <- 5
  size <- 10
  alpha <- runif(m)

  difference <- compare_distribution(greta::dirichlet_multinomial,
                                     extraDistr::ddirmnom,
                                     parameters = list(size = size, alpha = alpha),
                                     x = extraDistr::rdirmnom(100, size, alpha))

  expect_true(all(difference < 1e-4))

})

test_that('scalar-valued distributions can be defined in models', {

  skip_if_not(check_tf_version())
  source('helpers.R')

  x <- randn(5)
  y <- round(randu(5))
  p <- iprobit(normal(0, 1))

  # variable (need to define a likelihood)
  a = variable()
  distribution(x) = normal(a, 1)
  model(a)

  # univariate discrete distributions
  distribution(y) = bernoulli(p)
  model(p)

  distribution(y) = binomial(1, p)
  model(p)

  distribution(y) = beta_binomial(1, p, 0.2)
  model(p)

  distribution(y) = negative_binomial(1, p)
  model(p)

  distribution(y) = hypergeometric(5, 5, p)
  model(p)

  distribution(y) = poisson(p)
  model(p)

  # multivariate discrete distributions
  y <- extraDistr::rmnom(1, size = 4, prob = runif(3))
  p <- iprobit(normal(0, 1, dim = 3))
  distribution(y) = multinomial(4, p)
  model(p)

  y <- extraDistr::rmnom(1, size = 1, prob = runif(3))
  p <- iprobit(normal(0, 1, dim = 3))
  distribution(y) = categorical(p)
  model(p)

  y <- extraDistr::rmnom(1, size = 4, prob = runif(3))
  alpha <- lognormal(0, 1, dim = 3)
  distribution(y) = dirichlet_multinomial(4, alpha)
  model(alpha)

  # univariate continuous distributions
  model(normal(-2, 3))
  model(student(5.6, -2, 2.3))
  model(laplace(-1.2, 1.1))
  model(cauchy(-1.2, 1.1))
  model(logistic(-1.2, 1.1))

  model(lognormal(1.2, 0.2))
  model(gamma(0.9, 1.3))
  model(exponential(6.3))
  model(beta(6.3, 5.9))
  model(inverse_gamma(0.9, 1.3))
  model(weibull(2, 1.1))
  model(pareto(2.4, 1.5))
  model(chi_squared(4.3))
  model(f(24.3, 2.4))

  model(uniform(-13, 2.4))

  # multivariate continuous distributions
  sig <- MCMCpack::rwish(4, diag(3))

  model(multivariate_normal(rnorm(3), sig))
  model(wishart(4, sig))
  model(lkj_correlation(5, dim = 3))
  model(dirichlet(runif(3)))

})

test_that('array-valued distributions can be defined in models', {

  skip_if_not(check_tf_version())
  source('helpers.R')

  dim <- c(5, 2)
  x <- randn(5, 2)
  y <- round(randu(5, 2))

  # variable (need to define a likelihood)
  a <- variable(dim = dim)
  distribution(x) = normal(a, 1)
  model(a)

  # univariate discrete distributions
  p <- iprobit(normal(0, 1, dim = dim))
  distribution(y) = bernoulli(p)
  model(p)

  p <- iprobit(normal(0, 1, dim = dim))
  distribution(y) = binomial(1, p)
  model(p)

  p <- iprobit(normal(0, 1, dim = dim))
  distribution(y) = beta_binomial(1, p, 0.2)
  model(p)

  p <- iprobit(normal(0, 1, dim = dim))
  distribution(y) = negative_binomial(1, p)
  model(p)

  p <- iprobit(normal(0, 1, dim = dim))
  distribution(y) = hypergeometric(10, 5, p)
  model(p)

  p <- iprobit(normal(0, 1, dim = dim))
  distribution(y) = poisson(p)
  model(p)

  # multivariate discrete distributions
  y <- extraDistr::rmnom(5, size = 4, prob = runif(3))
  p <- iprobit(normal(0, 1, dim = 3))
  distribution(y) = multinomial(4, p, dim = 5)
  model(p)

  y <- extraDistr::rmnom(5, size = 1, prob = runif(3))
  p <- iprobit(normal(0, 1, dim = 3))
  distribution(y) = categorical(p, dim = 5)
  model(p)

  y <- extraDistr::rmnom(5, size = 4, prob = runif(3))
  alpha <- lognormal(0, 1, dim = 3)
  distribution(y) = dirichlet_multinomial(4, alpha, dim = 5)
  model(alpha)

  # univariate continuous distributions
  model(normal(-2, 3, dim = dim))
  model(student(5.6, -2, 2.3, dim = dim))
  model(laplace(-1.2, 1.1, dim = dim))
  model(cauchy(-1.2, 1.1, dim = dim))
  model(logistic(-1.2, 1.1, dim = dim))

  model(lognormal(1.2, 0.2, dim = dim))
  model(gamma(0.9, 1.3, dim = dim))
  model(exponential(6.3, dim = dim))
  model(beta(6.3, 5.9, dim = dim))
  model(uniform(-13, 2.4, dim = dim))
  model(inverse_gamma(0.9, 1.3, dim = dim))
  model(weibull(2, 1.1, dim = dim))
  model(pareto(2.4, 1.5, dim = dim))
  model(chi_squared(4.3, dim = dim))
  model(f(24.3, 2.4, dim = dim))

  # multivariate continuous distributions
  sig <- MCMCpack:::rwish(4, diag(3))
  model(multivariate_normal(rnorm(3), sig, dim = dim[1]))
  model(dirichlet(runif(3), dim = dim[1]))
  model(wishart(4, sig))
  model(lkj_correlation(3, dim = dim[1]))

})

test_that('distributions can be sampled from', {

  skip_if_not(check_tf_version())
  source('helpers.R')

  x <- randn(100)
  y <- round(randu(100))

  # variable (with a density)
  a <- variable()
  distribution(x) = normal(a, 1)
  sample_distribution(a)

  b <- variable(lower = -1)
  distribution(x) = normal(b, 1)
  sample_distribution(b)

  c <- variable(upper = -2)
  distribution(x) = normal(c, 1)
  sample_distribution(c)

  d <- variable(lower = 1.2, upper = 1.3)
  distribution(x) = normal(d, 1)
  sample_distribution(d)

  # univariate discrete
  p <- iprobit(normal(0, 1, dim = 100))
  distribution(y) = bernoulli(p)
  sample_distribution(p)

  p <- iprobit(normal(0, 1, dim = 100))
  distribution(y) = binomial(1, p)
  sample_distribution(p)

  p <- iprobit(normal(0, 1, dim = 100))
  distribution(y) = negative_binomial(1, p)
  sample_distribution(p)

  p <- iprobit(normal(0, 1, dim = 100))
  distribution(y) = hypergeometric(10, 5, p)
  sample_distribution(p)

  p <- iprobit(normal(0, 1, dim = 100))
  distribution(y) = poisson(p)
  sample_distribution(p)

  p <- iprobit(normal(0, 1, dim = 100))
  distribution(y) = beta_binomial(1, p, 0.3)
  sample_distribution(p)

  # multivariate discrete
  y <- extraDistr::rmnom(5, size = 4, prob = runif(3))
  p <- iprobit(normal(0, 1, dim = 3))
  distribution(y) = multinomial(4, p, dim = 5)
  sample_distribution(p)

  y <- extraDistr::rmnom(5, size = 1, prob = runif(3))
  p <- iprobit(normal(0, 1, dim = 3))
  distribution(y) = categorical(p, dim = 5)
  sample_distribution(p)

  y <- extraDistr::rmnom(5, size = 4, prob = runif(3))
  alpha <- lognormal(0, 1, dim = 3)
  distribution(y) = dirichlet_multinomial(4, alpha, dim = 5)
  sample_distribution(alpha)

  # univariate continuous
  sample_distribution(normal(-2, 3))
  sample_distribution(student(5.6, -2, 2.3))
  sample_distribution(laplace(-1.2, 1.1))
  sample_distribution(cauchy(-1.2, 1.1))
  sample_distribution(logistic(-1.2, 1.1))

  sample_distribution(lognormal(1.2, 0.2), lower = 0)
  sample_distribution(gamma(0.9, 1.3), lower = 0)
  sample_distribution(exponential(6.3), lower = 0)
  sample_distribution(beta(6.3, 5.9), lower = 0, upper = 1)
  sample_distribution(inverse_gamma(0.9, 1.3), lower = 0)
  sample_distribution(weibull(2, 1.1), lower = 0)
  sample_distribution(pareto(2.4, 1.5), lower = 0)
  sample_distribution(chi_squared(4.3), lower = 0)
  sample_distribution(f(24.3, 2.4), lower = 0)

  sample_distribution(uniform(-13, 2.4), lower = -13, upper = 2.4)

  # multivariate continuous
  sig <- MCMCpack::rwish(4, diag(3))
  sample_distribution(multivariate_normal(rnorm(3), sig))
  sample_distribution(wishart(4, sig))
  sample_distribution(lkj_correlation(4, dim = 3))
  sample_distribution(dirichlet(runif(3)))

})


test_that('variable() errors informatively', {

  source('helpers.R')

  # bad types
  expect_error(variable(upper = NA),
               'lower and upper must be numeric vectors of length 1')
  expect_error(variable(upper = head),
               'lower and upper must be numeric vectors of length 1')
  expect_error(variable(lower = 1:3),
               'lower and upper must be numeric vectors of length 1')

  # good types, bad values
  expect_error(variable(lower = Inf),
               '^lower and upper must either be')
  expect_error(variable(upper = -Inf),
               '^lower and upper must either be')

  # lower >= upper
  expect_error(variable(lower = 1, upper = 1),
               'upper bound must be greater than lower bound')

})

test_that('uniform distribution errors informatively', {

  source('helpers.R')

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


test_that('lkj_correlation distribution errors informatively', {

  source('helpers.R')

  dim <- 3

  expect_true(inherits(lkj_correlation(3, dim),
                       'greta_array'))

  expect_error(lkj_correlation(-1, dim),
               "^eta must be a positive scalar value, or a scalar greta array")

  expect_error(lkj_correlation(c(3, 3), dim),
               "^eta must be a positive scalar value, or a scalar greta array")

  expect_error(lkj_correlation(uniform(0, 1, dim = 2), dim),
               "^eta must be a scalar, but had dimensions")

  expect_error(lkj_correlation(4, dim = -1),
               "dim must be a scalar integer greater than one, but was:")

  expect_error(lkj_correlation(4, dim = c(3, 3)),
               "dim must be a scalar integer greater than one, but was:")

  expect_error(lkj_correlation(4, dim = NA),
               "dim must be a scalar integer greater than one, but was:")


})

test_that('multivariate_normal distribution errors informatively', {

  source('helpers.R')

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

test_that('multinomial distribution errors informatively', {

  source('helpers.R')

  p_a <- randu(3)
  p_b <- randu(3, 2)

  # good size & probs
  expect_true(inherits(multinomial(size = 10, p_a),
                       'greta_array'))

  # bad probs
  expect_error(multinomial(10, p_b),
               'prob must be a 2D greta array with one column, but has dimensions 3 x 2')

  # bad size
  expect_error(multinomial(c(1, 2), p_a),
               'size must be a scalar, but has dimensions 2 x 1')

  # scalars
  expect_error(multinomial(c(1), 1),
               'the multinomial distribution is for vectors, but the parameters were scalar')

  # bad dim
  expect_error(multinomial(10, p_a, dim = -1),
               'dim must be a scalar positive integer, but was: -1')
  expect_error(multinomial(10, p_a, dim = c(1, 3)),
               '^dim must be a scalar positive integer, but was:')

})

test_that('categorical distribution errors informatively', {

  source('helpers.R')

  p_a <- randu(3)
  p_b <- randu(3, 2)

  # good probs
  expect_true(inherits(categorical(p_a),
                       'greta_array'))

  # bad probs
  expect_error(categorical(p_b),
               'prob must be a 2D greta array with one column, but has dimensions 3 x 2')

  # scalars
  expect_error(categorical(c(1), 1),
               'the categorical distribution is for vectors, but the parameters were scalar')

  # bad dim
  expect_error(categorical(p_a, dim = -1),
               'dim must be a scalar positive integer, but was: -1')
  expect_error(categorical(p_a, dim = c(1, 3)),
               '^dim must be a scalar positive integer, but was:')

})

test_that('dirichlet distribution errors informatively', {

  source('helpers.R')

  alpha_a <- randu(3)
  alpha_b <- randu(3, 2)

  # good alpha
  expect_true(inherits(dirichlet(alpha_a),
                       'greta_array'))

  # bad probs
  expect_error(dirichlet(alpha_b),
               'alpha must be a 2D greta array with one column, but has dimensions 3 x 2')

  # scalars
  expect_error(dirichlet(1),
               'the dirichlet distribution is for vectors, but the parameters were scalar')

  # bad dim
  expect_error(dirichlet(alpha_a, dim = -1),
               'dim must be a scalar positive integer, but was: -1')
  expect_error(dirichlet(alpha_a, dim = c(1, 3)),
               '^dim must be a scalar positive integer, but was:')

})

test_that('dirichlet-multinomial distribution errors informatively', {

  source('helpers.R')

  size <- 4
  alpha_a <- randu(3)
  alpha_b <- randu(3, 2)

  # good alpha
  expect_true(inherits(dirichlet_multinomial(size, alpha_a),
                       'greta_array'))

  # bad probs
  expect_error(dirichlet_multinomial(size, alpha_b),
               'alpha must be a 2D greta array with one column, but has dimensions 3 x 2')

  # bad size
  expect_error(dirichlet_multinomial(c(1, 2), alpha_a),
               'size must be a scalar, but has dimensions 2 x 1')

  # scalars
  expect_error(dirichlet_multinomial(size, alpha = 1),
               'the dirichlet distribution is for vectors, but the parameters were scalar')

  # bad dim
  expect_error(dirichlet_multinomial(size, alpha_a, dim = -1),
               'dim must be a scalar positive integer, but was: -1')
  expect_error(dirichlet_multinomial(size, alpha_a, dim = c(1, 3)),
               '^dim must be a scalar positive integer, but was:')

})

test_that('Wishart can use a choleskied Sigma', {

  skip_if_not(check_tf_version())
  source('helpers.R')

  sig <- lkj_correlation(3, dim = 4)
  w <- wishart(5, sig)
  m <- model(w)
  draws <- mcmc(m, warmup = 10, n_samples = 10, verbose = FALSE)

})
