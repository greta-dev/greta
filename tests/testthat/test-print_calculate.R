test_that("calculate print method is different for different inputs", {
  # ensure print method is the new MCMC one

  x <- normal(0,1)
  m <- model(x)
  tensorflow::set_random_seed(2024-07-30-1057)
  draws <- mcmc(m, n_samples = 10, warmup = 10, verbose = FALSE)
  x_sim_10 <- calculate(x, nsim = 10, seed = 2024-08-1004)
  x_draws_10 <- calculate(x, values = draws, seed = 2024-08-1004)
  x_draws_10 <- calculate(x, values = draws, nsim = 10, seed = 2024-08-1004)
  expect_snapshot(x_sim_10)
  expect_snapshot(x_draws_10)
  expect_snapshot(x_draws_10)
})
