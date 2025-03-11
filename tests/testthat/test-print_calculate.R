test_that("calculate print method is different for different inputs", {
  skip_if_not(check_tf_version())
  # ensure print method is the new MCMC one
  skip_on_cran()
  skip_on_ci()

  x <- normal(0, 1)
  m <- model(x)
  new_seed <- 2024 - 11 - 07 - 14 - 01
  x_sim_10 <- calculate(x, nsim = 10, seed = new_seed)
  expect_snapshot(names(x_sim_10))
  expect_snapshot(dim(x_sim_10$x))
  # this test was too sensitive! Something to potentially revisit
  # expect_snapshot(x_sim_10)
  set.seed(new_seed)
  draws <- mcmc(m, n_samples = 10, warmup = 10, verbose = FALSE)
  x_draws_values <- calculate(x, values = draws, seed = new_seed)
  expect_snapshot(names(x_draws_values))
  expect_s3_class(x_draws_values, "greta_mcmc_list")
  expect_s3_class(x_draws_values, "mcmc.list")
  expect_s3_class(x_draws_values$`11`, "mcmc")
  # this test was too sensitive! Something to potentially revisit
  # expect_snapshot(x_draws_values)
  x_draws_10 <- calculate(x, values = draws, nsim = 10, seed = new_seed)
  expect_snapshot(names(x_draws_10))
  expect_snapshot(dim(x_draws_10$x))
})
