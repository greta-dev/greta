context("simulate")

test_that("simulate uses the local RNG seed", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  # fix variable
  a <- normal(0, 1)
  y <- normal(a, 1)
  m <- model(y)

  # the global RNG seed should change if the seed is *not* specified
  before <- rng_seed()
  sims <- simulate(m)
  after <- rng_seed()
  expect_false(identical(before, after))

  # the global RNG seed should not change if the seed *is* specified
  before <- rng_seed()
  sims <- simulate(m, seed = 12345)
  after <- rng_seed()
  expect_identical(before, after)

  # the samples should differ if the seed is *not* specified
  one <- simulate(m)
  two <- simulate(m)
  expect_false(identical(one, two))

  # the samples should differ if the seeds are specified differently
  one <- simulate(m, seed = 12345)
  two <- simulate(m, seed = 54321)
  expect_false(identical(one, two))

  # the samples should be the same if the seed is the same
  one <- simulate(m, seed = 12345)
  two <- simulate(m, seed = 12345)
  expect_identical(one, two)

})

test_that("simulate works with correct value lists", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  # fix variable
  a <- normal(0, 1)
  y <- normal(a, 1)
  m <- model(y)
  sims <- simulate(m, nsim = 100, values = list(a = 100))

  # with y ~ N(100, 1 ^ 2), it should be very unlikely that y <= 90
  # ( pnorm(90, 100, 1) = 7e-24 )
  expect_true(all(sims$y > 90))

  # fix variable and new data
  x <- as_data(1)
  a <- normal(0, 1)
  y <- normal(a * x, 1)
  m <- model(y)
  sims <- simulate(m, nsim = 100, values = list(a = 50, x = 2))

  expect_true(all(sims$y > 90))

})

test_that("simulate works with mcmc.list objects", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  samples <- 10

  n <- 100
  y <- as_data(rnorm(n))
  x <- as_data(1)
  a <- normal(0, 1)
  distribution(y) <- normal(a, x)
  m <- model(a)
  draws <- mcmc(m, warmup = 0, n_samples = samples, verbose = FALSE)

  sims <- simulate(m, values = draws)

  # correct dimensions
  expect_equal(dim(sims$a), c(samples, 1))
  # all valid values
  expect_true(all(is.finite(sims$a)))

})

test_that("simulate works if distribution-free variables are fixed", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  # fix variable
  a <- variable
  y <- normal(a, 1)
  m <- model(y)
  sims <- simulate(m, values = list(a = 100))

  # with y ~ N(100, 1 ^ 2), it should be very unlikely that y <= 90
  # ( pnorm(90, 100, 1) = 7e-24 )
  expect_true(all(sims$y > 90))

})

test_that("simulate errors if distribution-free variables are not fixed", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  # fix variable
  a <- variable
  y <- normal(a, 1)
  m <- model(y)
  expect_error(sims <- simulate(m),
               "specified in values")

})

test_that("simulate errors nicely if mcmc.list objects missing info", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  samples <- 10
  a <- normal(0, 1)
  y <- normal(a, 1)
  m <- model(y)
  draws <- mcmc(m, warmup = 0, n_samples = samples, verbose = FALSE)

  # scrub the model info
  attr(draws, "model_info") <- NULL

  # it should error nicely
  expect_error(simulate(m, values = draws),
               "perhaps it wasn't created by greta")

})

test_that("simulate errors nicely if values have incorrect dimensions", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  a <- normal(0, 1)
  y <- normal(a, 1)
  m <- model(y)

  # it should error nicely
  expect_error(simulate(m, values = list(a = c(1, 1))),
               "different number of elements than the greta array")

})

test_that("simulate errors nicely if target contains non-greta arrays", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  z <- 1
  x <- normal(0, 1)
  m <- model(x)

  # it should error nicely
  expect_error(simulate(m, targets = list(z)),
               "element of 'targets' is not a greta array")

})

test_that("simulate errors nicely if nsim is invalid", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  x <- normal(0, 1)
  m <- model(x)

  expect_error(simulate(m, nsim = 0),
               "must be a positive integer scalar")

  expect_error(simulate(m, nsim = -1),
               "must be a positive integer scalar")

  expect_error(simulate(m, nsim = matrix(1, 1)),
               "must be a positive integer scalar")

  expect_error(simulate(m, nsim = "five"),
               "must be a positive integer scalar")

})
