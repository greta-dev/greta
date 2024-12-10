set.seed(2020 - 02 - 11)

test_that("draws and raw draws should have the right iteration numbering", {
  skip_if_not(check_tf_version())

  samples <- 1000
  warmup <- 100
  z <- normal(0, 1)
  m <- model(z)
  draws <- mcmc(m, warmup = warmup, n_samples = samples, verbose = FALSE)

  expect_equal(start(draws), 1)
  expect_equal(end(draws), samples)

  raw_draws <- get_model_info(draws)$raw_draws
  expect_equal(start(raw_draws), 1)
  expect_equal(end(raw_draws), samples)

  # same after calculating
  y <- z^2

  y_draws <- calculate(y, values = draws)
  expect_equal(start(y_draws), 1)
  expect_equal(end(y_draws), samples)

  raw_draws <- get_model_info(y_draws)$raw_draws
  expect_equal(start(raw_draws), 1)
  expect_equal(end(raw_draws), samples)
})

test_that("window works", {
  skip_if_not(check_tf_version())

  z <- normal(0, 1)
  m <- model(z)
  draws <- mcmc(m, warmup = 100, verbose = FALSE)

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
  z2 <- z^2
  z2_draws_sub <- calculate(z2, values = draws_sub)

  expect_s3_class(z2_draws_sub, "greta_mcmc_list")
  expect_identical(start(z2_draws_sub), start)
  expect_identical(end(z2_draws_sub), end)
  z2_times <- as.vector(time(z2_draws_sub))
  expect_identical(z2_times, seq(start, end, by = thin))
})

test_that("windowing does not have spooky effects", {
  skip_if_not(check_tf_version())

  chains <- 4
  samples <- 100
  n_samples <- chains * samples
  x <- normal(0, 1)
  m <- model(x)
  draws <- mcmc(m,
    warmup = 100,
    n_samples = samples,
    chains = chains,
    verbose = FALSE
  )

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

test_that("greta_mcmc_list print method works", {
  skip_if_not(check_tf_version())
  samples <- 10
  warmup <- 10
  z <- normal(0, 1)
  m <- model(z)
  tensorflow::set_random_seed(2024-07-29-1217)
  draws <- mcmc(m, warmup = warmup, n_samples = samples, verbose = FALSE)
  expect_snapshot(
    draws
  )
})

test_that("greta_mcmc_list print method works with larger sample size", {
  skip_if_not(check_tf_version())
  samples <- 20
  warmup <- 20
  z <- normal(0, 1)
  m <- model(z)
  tensorflow::set_random_seed(2024-07-30-1233)
  draws <- mcmc(m, warmup = warmup, n_samples = samples, verbose = FALSE)
  expect_snapshot(
    draws
  )
  expect_snapshot(
    print(draws, n = 20)
  )
  expect_snapshot(
    print(draws, n = 19)
  )
  expect_snapshot(
    print(draws, n = 21)
  )
})

test_that("greta_mcmc_list print method works with smaller sample size", {
  skip_if_not(check_tf_version())
  samples <- 2
  warmup <- 2
  z <- normal(0, 1)
  m <- model(z)
  tensorflow::set_random_seed(2024-07-30-34)
  draws <- mcmc(m, warmup = warmup, n_samples = samples, verbose = FALSE)
  expect_snapshot(
    draws
  )
  expect_snapshot(
    print(draws, n = 1)
  )
  expect_snapshot(
    print(draws, n = 3)
  )
})
