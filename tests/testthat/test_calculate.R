context("calculate")

test_that("calculate works with correct lists", {

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

test_that("calculate works with greta_mcmc_list objects", {

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

test_that("calculate errors nicely if not all required values are passed", {

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
               "target' is not a greta array")

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
