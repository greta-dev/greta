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

test_that("calculate works with mcmc.list objects", {

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
  expect_s3_class(y_values, "mcmc.list")
  # correct dimensions
  expect_equal(dim(y_values[[1]]), c(10, 2))
  # all valid values
  expect_true(all(is.finite(as.vector(y_values[[1]]))))

  # with a new greta array, based on a different element in the model
  new_values <- calculate(a ^ 2, draws)
  # correct class
  expect_s3_class(new_values, "mcmc.list")
  # correct dimensions
  expect_equal(dim(new_values[[1]]), c(10, 1))
  # all valid values
  expect_true(all(is.finite(as.vector(new_values[[1]]))))

})

test_that("calculate errors nicely if mcmc.list objects missing info", {

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
