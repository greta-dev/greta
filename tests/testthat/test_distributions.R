context('distributions')

test_that('free distributions have no density', {

  source('helpers.R')

  none <- function (x, lower, upper, log) 0

  # no constraints
  difference <- compare_distribution(greta::free,
                                     none,
                                     parameters = list(lower = -Inf, upper = Inf),
                                     x = rnorm(100))

  expect_true(difference == 0)

  # lower constraint
  difference <- compare_distribution(greta::free,
                                     none,
                                     parameters = list(lower = -1, upper = Inf),
                                     x = rnorm(100))

  expect_true(difference == 0)

  # upper constraint
  difference <- compare_distribution(greta::free,
                                     none,
                                     parameters = list(lower = -Inf, upper = 1),
                                     x = rnorm(100))

  expect_true(difference == 0)

  # both constraints
  difference <- compare_distribution(greta::free,
                                     none,
                                     parameters = list(lower = -1, upper = 1),
                                     x = rnorm(100))

  expect_true(difference == 0)

})

test_that('normal distribution has correct density', {

  source('helpers.R')

  difference <- compare_distribution(greta::normal,
                                     stats::dnorm,
                                     parameters = list(mean = -2, sd = 3),
                                     x = rnorm(100, -2, 3))

  expect_true(all(difference < 1e-4))

})

test_that('lognormal distribution has correct density', {

  source('helpers.R')

  difference <- compare_distribution(greta::lognormal,
                                     stats::dlnorm,
                                     parameters = list(meanlog = 1, sdlog = 3),
                                     x = rlnorm(100, 1, 3))

  expect_true(all(difference < 1e-4))

})

test_that('bernoulli distribution has correct density', {

  source('helpers.R')

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

  difference <- compare_distribution(greta::binomial,
                                     stats::dbinom,
                                     parameters = list(size = 10, prob = 0.8),
                                     x = rbinom(100, 10, 0.8))

  expect_true(all(difference < 1e-4))

})

test_that('negative binomial distribution has correct density', {

  source('helpers.R')

  difference <- compare_distribution(greta::negative_binomial,
                                     stats::dnbinom,
                                     parameters = list(size = 3.3, prob = 0.2),
                                     x = rnbinom(100, 3.3, 0.2))

  expect_true(all(difference < 1e-4))

})

test_that('poisson distribution has correct density', {

  source('helpers.R')

  difference <- compare_distribution(greta::poisson,
                                     stats::dpois,
                                     parameters = list(lambda = 17.2),
                                     x = rpois(100, 17.2))

  expect_true(all(difference < 1e-4))

})

test_that('gamma distribution has correct density', {

  source('helpers.R')

  difference <- compare_distribution(greta::gamma,
                                     stats::dgamma,
                                     parameters = list(shape = 1.2, rate = 2.3),
                                     x = rgamma(100, 1.2, 2.3))

  expect_true(all(difference < 1e-4))

})

