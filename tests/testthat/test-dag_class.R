test_that("the log-density and trace functions trace once per model", {
  skip_if_not(check_tf_version())
  y <- as_data(rnorm(10))
  mu <- normal(0, 10)
  distribution(y) <- normal(mu, 1)
  m <- model(mu)

  # each meets two batch sizes: the log-density function one row while
  # checking initial values and then the chains inside the sampler, and the
  # trace function one row per chain and then the draws
  mcmc(m, n_samples = 30, warmup = 10, chains = 2, verbose = FALSE)

  expect_identical(trace_count(m$dag$tf_log_prob_function), 1L)
  expect_identical(trace_count(m$dag$tf_trace_values_batch), 1L)
})

test_that("opt() traces only a log-density function for one row", {
  skip_if_not(check_tf_version())
  y <- as_data(rnorm(10))
  mu <- normal(0, 10)
  distribution(y) <- normal(mu, 1)
  m <- model(mu)

  opt(m)

  one_row <- m$dag$tf_log_prob_function_one_row
  n_rows <- one_row$input_signature[[1]]$shape$as_list()[[1]]
  expect_identical(as.integer(n_rows), 1L)
  expect_identical(trace_count(one_row), 1L)
  expect_identical(trace_count(m$dag$tf_log_prob_function), 0L)
})
