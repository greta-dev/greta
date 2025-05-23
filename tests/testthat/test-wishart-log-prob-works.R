test_that("Wishart log_prob function does not return NaNs", {
  skip_if_not(check_tf_version())
  sigma <- matrix(
    data = c(1.2, 0.7, 0.7, 2.3),
    nrow = 2,
    ncol = 2
  )
  df <- 4
  x <- wishart(df, sigma)[1, 2]
  m <- model(x)
  new_log_prob <- m$dag$generate_log_prob_function()
  m$dag$define_tf_log_prob_function()
  prob_input <- matrix(rnorm(12), 4, 3)
  log_probs <- new_log_prob(prob_input)

  is_nan_adjusted <- all(is.nan(as.numeric(log_probs$adjusted)))
  is_nan_unadjusted <- all(is.nan(as.numeric(log_probs$unadjusted)))

  expect_false(is_nan_adjusted && is_nan_unadjusted)
})
