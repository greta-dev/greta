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

test_that("compile = TRUE has XLA compile the model's functions", {
  skip_if_not(check_tf_version())
  x <- normal(0, 1)
  m_compiled <- model(x, compile = TRUE)
  m_default <- model(x)
  opt(m_compiled)
  opt(m_default)

  compiled <- function(dag) {
    vapply(
      list(
        dag$tf_log_prob_function,
        dag$tf_log_prob_function_one_row,
        dag$tf_trace_values_batch
      ),
      xla_must_compile,
      logical(1)
    )
  }
  expect_identical(compiled(m_compiled$dag), rep(TRUE, 3))
  expect_identical(compiled(m_default$dag), rep(FALSE, 3))
})

test_that("a model with a covariance or correlation matrix runs uncompiled", {
  skip_if_not(check_tf_version())
  set.seed(2026 - 10 - 06)
  correlation <- lkj_correlation(2)

  expect_snapshot(m <- model(correlation, compile = TRUE))
  expect_false(xla_must_compile(m$dag$tf_log_prob_function))
  expect_ok(mcmc(m, warmup = 10, n_samples = 10, chains = 1, verbose = FALSE))
})
