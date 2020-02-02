context("simulate")

test_that("simulate prodices the right number of samples", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  # fix variable
  a <- normal(0, 1)
  y <- normal(a, 1, dim = c(1, 3))
  m <- model(y, a)

  # should be vectors
  a_sims <- simulate(m)$a
  expect_true(identical(length(a_sims), 1L))

  a_sims <- simulate(m, 17)$a
  expect_true(identical(length(a_sims), 17L))

  y_sims <- simulate(m)$y
  expect_true(identical(length(y_sims), 3L))

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

test_that("simulate errors if distribution-free variables are not fixed", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  # fix variable
  a <- variable()
  y <- normal(a, 1)
  m <- model(y)
  expect_error(sims <- simulate(m),
               "do not have a distribution")

})


test_that("simulate errors if a distribution cannot be sampled from", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  # fix variable
  a <- lkj_correlation(3)
  y <- normal(a, 1)
  m <- model(y)
  expect_error(sims <- simulate(m),
               "sampling is not implemented")

})

test_that("simulate errors nicely if nsim is invalid", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  x <- normal(0, 1)
  m <- model(x)

  expect_error(simulate(m, nsim = 0),
               "must be a positive integer")

  expect_error(simulate(m, nsim = -1),
               "must be a positive integer")

  expect_error(simulate(m, nsim = "five"),
               "must be a positive integer")

})
