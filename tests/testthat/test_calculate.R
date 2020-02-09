context("calculate")

test_that("deterministic calculate works with correct lists", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  # unknown variable
  x <- as_data(c(1, 2))
  a <- normal(0, 1)
  y <- a * x
  y_value <- calculate(y, list(a = 3))
  expect_equal(y_value, matrix(c(3, 6)))

  # unknown variable and new data
  x <- as_data(c(1, 2))
  a <- normal(0, 1)
  y <- a * x
  y_value <- calculate(y, list(a = 6, x = c(2, 1)))
  expect_equal(y_value, matrix(c(12, 6)))

})

test_that("stochastic calculate works with correct lists", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  # fix variable
  a <- normal(0, 1)
  y <- normal(a, 1)
  sims <- calculate(list(a, y), nsim = 100, values = list(a = 100))

  # with y ~ N(100, 1 ^ 2), it should be very unlikely that y <= 90
  # ( pnorm(90, 100, 1) = 7e-24 )
  expect_true(all(sims$y > 90))

  # fix variable and new data
  x <- as_data(1)
  a <- normal(0, 1)
  y <- normal(a * x, 1)
  sims <- calculate(list(a, y), nsim = 100, values = list(a = 50, x = 2))

  expect_true(all(sims$y > 90))

})

test_that("deterministic calculate works with greta_mcmc_list objects", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  samples <- 10
  x <- as_data(c(1, 2))
  a <- normal(0, 1)
  y <- a * x
  m <- model(y)
  draws <- mcmc(m, warmup = 0, n_samples = samples, verbose = FALSE)

  # with an existing greta array
  y_values <- calculate(y, draws)
  # correct class
  expect_s3_class(y_values, "greta_mcmc_list")
  # correct dimensions
  expect_equal(dim(y_values[[1]]), c(10, 2))
  # all valid values
  expect_true(all(is.finite(as.vector(y_values[[1]]))))

  # with a new greta array, based on a different element in the model
  new_values <- calculate(a ^ 2, draws)
  # correct class
  expect_s3_class(new_values, "greta_mcmc_list")
  # correct dimensions
  expect_equal(dim(new_values[[1]]), c(10, 1))
  # all valid values
  expect_true(all(is.finite(as.vector(new_values[[1]]))))

})

test_that("stochastic calculate works with greta_mcmc_list objects", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  samples <- 10
  chains <- 2

  n <- 100
  y <- as_data(rnorm(n))
  x <- as_data(1)
  a <- normal(0, 1)
  distribution(y) <- normal(a, x)
  m <- model(a)
  draws <- mcmc(
    m,
    warmup = 0,
    n_samples = samples,
    chains = chains,
    verbose = FALSE
  )

  # this should error without nsim being specified (y is stochastic)
  expect_error(calculate(list(a, y), values = draws),
               "values have not been provided")

  # for a list of targets, the result should be a list
  nsim <- 10
  sims <- calculate(list(a, y), values = draws, nsim = nsim)

  # correct class, dimensions, and valid values
  expect_true(is.list(sims))
  expect_equal(names(sims), c("a", "y"))
  expect_equal(dim(sims$a), c(nsim, 1))
  expect_equal(dim(sims$y), c(nsim, n, 1))
  expect_true(all(is.finite(sims$a)) & all(is.finite(sims$y)))

  # a single array with these nsim observations
  sims <- calculate(y, values = draws, nsim = nsim)

  expect_true(is.numeric(sims))
  expect_equal(dim(sims), c(nsim, n, 1))
  expect_true(all(is.finite(sims)))

  # warn about resampling if nsim is greater than the number of elements in draws
  expect_warning(calculate(y, values = draws, nsim = samples * chains + 1),
                 "posterior samples drawn with replacement")

})

test_that("calculate errors nicely if greta_mcmc_list objects missing info", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  samples <- 10
  x <- as_data(c(1, 2))
  a <- normal(0, 1)
  y <- a * x
  m <- model(y)
  draws <- mcmc(m, warmup = 0, n_samples = samples, verbose = FALSE)

  # scrub the model info
  attr(draws, "model_info") <- NULL

  # it should error nicely
  expect_error(calculate(y, draws),
               "perhaps it wasn't created by greta")

})

test_that("calculate errors nicely if values for stochastics not passed", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  x <- as_data(c(1, 2))
  a <- normal(0, 1)
  y <- a * x

  # it should error nicely
  expect_error(calculate(y, list(x = c(2, 1))),
               paste("values have not been provided for all greta arrays on",
                     "which the target depends. Please provide values for the",
                     "greta array: a"))

  # but is should work fine if nsim is set
  expect_ok(calculate(y, list(x = c(2, 1)), nsim = 1))

})

test_that("calculate errors nicely if values have incorrect dimensions", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  x <- as_data(c(1, 2))
  a <- normal(0, 1)
  y <- a * x

  # it should error nicely
  expect_error(calculate(y, list(a = c(1, 1))),
               "different number of elements than the greta array")

})

test_that("calculate errors nicely if not used on a greta array", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  x <- as_data(c(1, 2))
  a <- normal(0, 1)
  y <- a * x
  z <- 1:5

  # it should error nicely
  expect_error(calculate(z, list(a = c(1, 1))),
               "'target' must be either a greta array")

})

test_that("calculate works with variable batch sizes", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  samples <- 100
  x <- as_data(c(1, 2))
  a <- normal(0, 1)
  y <- a * x
  m <- model(y)
  draws <- mcmc(m, warmup = 0, n_samples = samples, verbose = FALSE)

  # variable valid batch sizes
  val_1 <- calculate(y, draws, trace_batch_size = 1)
  val_10 <- calculate(y, draws, trace_batch_size = 10)
  val_100 <- calculate(y, draws, trace_batch_size = 100)
  val_inf <- calculate(y, draws, trace_batch_size = Inf)

  # check the first one
  expect_s3_class(val_1, "greta_mcmc_list")
  expect_equal(dim(val_1[[1]]), c(100, 2))
  expect_true(all(is.finite(as.vector(val_1[[1]]))))

  # check the others are the same
  expect_identical(val_10, val_1)
  expect_identical(val_100, val_1)
  expect_identical(val_inf, val_1)

})

test_that("calculate errors nicely with invalid batch sizes", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  samples <- 100
  x <- as_data(c(1, 2))
  a <- normal(0, 1)
  y <- a * x
  m <- model(y)
  draws <- mcmc(m, warmup = 0, n_samples = samples, verbose = FALSE)

  # variable valid batch sizes
  expect_error(calculate(y, draws, trace_batch_size = 0),
               "greater than or equal to 1")
  expect_error(calculate(y, draws, trace_batch_size = NULL),
               "greater than or equal to 1")
  expect_error(calculate(y, draws, trace_batch_size = NA),
               "greater than or equal to 1")

})

test_that("calculate returns a list or array based on target", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  a <- as_data(randn(3))
  b <- a ^ 2
  c <- sqrt(b)

  # if target is a single greta array, the output should be a single numeric
  result <- calculate(b, nsim = 10)
  expect_false(is.list(result))
  expect_true(is.numeric(result))

  # if target is a list, the output should be a list of numerics
  result <- calculate(list(b, c), nsim = 10)
  expect_true(is.list(result))

  # check contents
  are_numeric <- vapply(result, is.numeric, FUN.VALUE = logical(1))
  expect_true(all(are_numeric))

  # check names
  expect_true(identical(names(result), c("b", "c")))

})

test_that("calculate produces the right number of samples", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  # fix variable
  a <- normal(0, 1)
  y <- normal(a, 1, dim = c(1, 3))

  # should be vectors
  a_sims <- calculate(a, nsim = 1)
  expect_true(identical(length(a_sims), 1L))

  a_sims <- calculate(a, nsim = 17)
  expect_true(identical(length(a_sims), 17L))

  y_sims <- calculate(y, nsim = 1)
  expect_true(identical(length(y_sims), 3L))

  y_sims <- calculate(y, nsim = 19)
  expect_true(identical(dim(y_sims), c(19L, 3L)))

  # the global RNG seed should not change if the seed *is* specified
  before <- rng_seed()
  a_sims <- calculate(y, nsim = 1, seed = 12345)
  after <- rng_seed()
  expect_identical(before, after)

  # the samples should differ if the seed is *not* specified
  one <- calculate(y, nsim = 1)
  two <- calculate(y, nsim = 1)
  expect_false(identical(one, two))

  # the samples should differ if the seeds are specified differently
  one <- calculate(y, nsim = 1, seed = 12345)
  two <- calculate(y, nsim = 1, seed = 54321)
  expect_false(identical(one, two))

  # the samples should be the same if the seed is the same
  one <- calculate(y, nsim = 1, seed = 12345)
  two <- calculate(y, nsim = 1, seed = 12345)
  expect_identical(one, two)

})

test_that("calculate uses the local RNG seed", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  # fix variable
  a <- normal(0, 1)
  y <- normal(a, 1)

  # the global RNG seed should change if the seed is *not* specified
  before <- rng_seed()
  sims <- calculate(y, nsim = 1)
  after <- rng_seed()
  expect_false(identical(before, after))

  # the global RNG seed should not change if the seed *is* specified
  before <- rng_seed()
  sims <- calculate(y, nsim = 1, seed = 12345)
  after <- rng_seed()
  expect_identical(before, after)

  # the samples should differ if the seed is *not* specified
  one <- calculate(y, nsim = 1)
  two <- calculate(y, nsim = 1)
  expect_false(identical(one, two))

  # the samples should differ if the seeds are specified differently
  one <- calculate(y, nsim = 1, seed = 12345)
  two <- calculate(y, nsim = 1, seed = 54321)
  expect_false(identical(one, two))

  # the samples should be the same if the seed is the same
  one <- calculate(y, nsim = 1, seed = 12345)
  two <- calculate(y, nsim = 1, seed = 12345)
  expect_identical(one, two)

})

test_that("calculate works if distribution-free variables are fixed", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  # fix variable
  a <- variable()
  y <- normal(a, 1)
  sims <- calculate(list(a, y), nsim = 1, values = list(a = 100))

  # with y ~ N(100, 1 ^ 2), it should be very unlikely that y <= 90
  # ( pnorm(90, 100, 1) = 7e-24 )
  expect_true(all(sims$y > 90))

})

test_that("calculate errors if distribution-free variables are not fixed", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  # fix variable
  a <- variable()
  y <- normal(a, 1)
  expect_error(sims <- calculate(m, nsim = 1),
               "specified in values")

})

test_that("calculate errors if a distribution cannot be sampled from", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  # fix variable
  a <- lkj_correlation(3)
  y <- normal(a, 1)
  expect_error(sims <- calculate(y, nsim = 1),
               "sampling is not implemented")

})

test_that("calculate errors nicely if nsim is invalid", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  x <- normal(0, 1)

  expect_error(calculate(x, nsim = 0),
               "must be a positive integer")

  expect_error(calculate(x, nsim = -1),
               "must be a positive integer")

  expect_error(calculate(x, nsim = "five"),
               "must be a positive integer")

})
