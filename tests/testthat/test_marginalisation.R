context("marginalisation")

test_that("marginalise errors nicely", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  # not a function
  expect_error(
    marginalise("apple", normal(0, 1), discrete_marginalisation()),
    "must be an R function"
  )

  # not a greta array
  expect_error(
    marginalise(I, 1:5, discrete_marginalisation()),
    "must be a variable greta array with a distribution"
  )

  # greta array but no distribution
  expect_error(
    marginalise(I, variable(), discrete_marginalisation()),
    "must be a variable greta array with a distribution"
  )

  # not a marginaliser
  expect_error(
    marginalise(I, normal(0, 1), mean),
    "'method' must be a valid marginalisation method"
  )

})

test_that("discrete_marginalisation errors nicely", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  # greta array, not a numeric
  expect_error(
    discrete_marginalisation(values = variable()),
    "must be an R numeric vector, not a greta array"
  )

  # not a numeric
  expect_error(
    discrete_marginalisation(values = c("apple", "banana")),
    "must be an R numeric vector$"
  )

  # mismatch with distribution
  expect_error(
    marginalise(I, normal(0, 1), discrete_marginalisation(values = 1:5)),
    "can only be used with discrete distributions"
  )

})
