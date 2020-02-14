context("greta_mcmc_list class")

test_that("window works", {

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
  z2_draws_sub <- calculate(z2, values = draws_sub)

  expect_s3_class(z2_draws_sub, "greta_mcmc_list")
  expect_identical(start(z2_draws_sub), start)
  expect_identical(end(z2_draws_sub), end)
  z2_times <- as.vector(time(z2_draws_sub))
  expect_identical(z2_times, seq(start, end, by = thin))

})

test_that("windowing does not have spooky effects", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  chains <- 4
  samples <- 100
  n_samples <- chains * samples
  x <- normal(0, 1)
  m <- model(x)
  draws <- mcmc(m,
                warmup = 100,
                n_samples = samples,
                chains = chains,
                verbose = FALSE)

  raw_draws <- get_model_info(draws)$raw_draws

  expect_equal(dim(as.matrix(draws)), c(n_samples, 1))
  expect_equal(dim(as.matrix(raw_draws)), c(n_samples, 1))

  # drop the first half of the chain in this other object
  draws_2 <- window(draws, start = end(draws) / 2 + 1)
  raw_draws_2 <- get_model_info(draws_2)$raw_draws
  half_samples <- n_samples / 2
  expect_equal(dim(as.matrix(draws_2)), c(half_samples, 1))
  expect_equal(dim(as.matrix(raw_draws_2)), c(half_samples, 1))

  # the first object should not have changed
  raw_draws <- get_model_info(draws)$raw_draws
  expect_equal(dim(as.matrix(draws)), c(n_samples, 1))
  expect_equal(dim(as.matrix(raw_draws)), c(n_samples, 1))

})
