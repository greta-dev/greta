test_that("the log-density and trace functions trace once per model", {
  skip_if_not(check_tf_version())
  y <- as_data(rnorm(10))
  mu <- normal(0, 10)
  distribution(y) <- normal(mu, 1)
  m <- model(mu)

  # both are first called on one row, while checking initial values, then on
  # a batch: the chains inside the sampler, and the draws when tracing values
  mcmc(m, n_samples = 30, warmup = 10, chains = 2, verbose = FALSE)

  traces <- \(f) as.integer(f$experimental_get_tracing_count())
  expect_identical(traces(m$dag$tf_log_prob_function), 1L)
  expect_identical(traces(m$dag$tf_trace_values_batch), 1L)
})

test_that("opt() traces only a log-density function for one row", {
  skip_if_not(check_tf_version())
  y <- as_data(rnorm(10))
  mu <- normal(0, 10)
  distribution(y) <- normal(mu, 1)
  m <- model(mu)

  opt(m)

  traces <- \(f) as.integer(f$experimental_get_tracing_count())
  one_row <- m$dag$tf_log_prob_function_one_row
  n_rows <- one_row$input_signature[[1]]$shape$as_list()[[1]]
  expect_identical(as.integer(n_rows), 1L)
  expect_identical(traces(one_row), 1L)
  expect_identical(traces(m$dag$tf_log_prob_function), 0L)
})
