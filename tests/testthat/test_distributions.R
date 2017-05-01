context('distributions')

test_that('normal distribution has correct density', {

  source('helpers.R')

  difference <- compare_distribution(greta::normal,
                                     stats::dnorm,
                                     parameters = list(mean = -2, sd = 3),
                                     x = rnorm(100, -2, 3))

  expect_true(all(difference < 1e-6))

})

test_that('lognormal distribution has correct density', {

  source('helpers.R')

  difference <- compare_distribution(greta::lognormal,
                                     stats::dlnorm,
                                     parameters = list(meanlog = 1, sdlog = 3),
                                     x = rlnorm(100, 1, 3))

  expect_true(all(difference < 1e-6))

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

  expect_true(all(difference < 1e-6))

})

