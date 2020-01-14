context("greta_mcmc_list class")


test_that("window works", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  skip_if_not(check_tf_version())
  source("helpers.R")

  z <- normal(0, 1)
  m <- model(z)
  draws <- mcmc(m)

  # should have correct class
  expect_s3_class(draws, "greta_mcmc_list")

  start <- 200
  end <- 400
  thin <- 5

  draws_sub <- window(draws, start = start, end = end, thin = thin)

  # should still have correct class
  expect_s3_class(draws_sub, "greta_mcmc_list")

  # with correct windowing and thinning information
  expect_identical(start(draws_sub), start)
  expect_identical(end(draws_sub), end)
  times <- as.vector(time(draws_sub))
  expect_identical(times, seq(start, end, by = thin))

  # calculate should retain this info too
  z2 <- z ^ 2
  z2_draws_sub <- calculate(z2, draws_sub)

  expect_s3_class(z2_draws_sub, "greta_mcmc_list")
  expect_identical(start(z2_draws_sub), start)
  expect_identical(end(z2_draws_sub), end)
  z2_times <- as.vector(time(z2_draws_sub))
  expect_identical(z2_times, seq(start, end, by = thin))

})
