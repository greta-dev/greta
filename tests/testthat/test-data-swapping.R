test_that("data can be swapped without retracing the log prob function", {
  skip_if_not(check_tf_version())

  x <- as_data(rep(0, 5))
  z <- normal(0, 1)
  distribution(x) <- normal(z, 1)
  m <- model(z)
  dag <- m$dag

  free_state <- matrix(0.5, 1, 1)
  log_prob <- function() {
    as.numeric(dag$tf_log_prob_function_adjusted(free_state))
  }
  traces <- function() {
    dag$tf_log_prob_function$experimental_get_tracing_count()
  }

  before <- log_prob()
  traces_before <- traces()

  dag$set_data_value(x, as.matrix(rep(3, 5)))

  # the density follows the new data, and nothing was retraced to do it:
  # rebuilding the graph per swap is what made the Geweke checks slow,
  # greta-dev/greta#739
  after <- log_prob()
  expect_false(identical(after, before))
  expect_true(is.finite(after))
  expect_identical(traces(), traces_before)
})

test_that("set_data_value() errors informatively on bad input", {
  skip_if_not(check_tf_version())

  x <- as_data(rep(0, 5))
  z <- normal(0, 1)
  distribution(x) <- normal(z, 1)
  m <- model(z)

  # not data
  expect_snapshot(error = TRUE, m$dag$set_data_value(z, 1))

  # right node, wrong shape: a variable's shape is fixed
  expect_snapshot(error = TRUE, m$dag$set_data_value(x, rep(1, 3)))
})

test_that("swapped data reaches the sampler, not just the log prob", {
  skip_if_not(check_tf_version())

  # a tight likelihood, so the posterior for z sits wherever the data is
  x <- as_data(rep(0, 10))
  z <- normal(0, 10)
  distribution(x) <- normal(z, 0.1)
  m <- model(z)

  draws <- mcmc(m, chains = 1, warmup = 200, n_samples = 100, verbose = FALSE)
  before <- mean(as.matrix(draws))

  # move the data a long way. extra_samples() goes through the sampler's own
  # traced function, not tf_log_prob_function, so this is the path that would
  # silently keep sampling against stale data
  m$dag$set_data_value(x, as.matrix(rep(5, 10)))
  after_draws <- extra_samples(draws, n_samples = 300, verbose = FALSE)
  after <- mean(tail(as.matrix(after_draws), 100))

  expect_lt(abs(before), 1)
  expect_gt(after, 3)
})

test_that("setting a node value and rebuilding still changes the graph", {
  skip_if_not(check_tf_version())

  x <- as_data(rep(0, 5))
  z <- normal(0, 1)
  distribution(x) <- normal(z, 1)
  m <- model(z)
  dag <- m$dag

  free_state <- matrix(0.5, 1, 1)
  log_prob <- function() {
    as.numeric(dag$tf_log_prob_function_adjusted(free_state))
  }
  before <- log_prob()

  # the older way of changing data, which set_data_value() has to keep working:
  # set the node's value, then rebuild. If the rebuild does not refresh the
  # variable it silently keeps computing on the original data
  get_node(x)$value(as.matrix(rep(3, 5)))
  dag$define_tf_log_prob_function()

  expect_false(identical(log_prob(), before))
})

test_that("a rebuild does not revert a value set with set_data_value()", {
  skip_if_not(check_tf_version())

  x <- as_data(rep(0, 5))
  z <- normal(0, 1)
  distribution(x) <- normal(z, 1)
  m <- model(z)
  dag <- m$dag

  free_state <- matrix(0.5, 1, 1)
  log_prob <- function() {
    as.numeric(dag$tf_log_prob_function_adjusted(free_state))
  }

  dag$set_data_value(x, as.matrix(rep(3, 5)))
  swapped <- log_prob()

  # rebuilding refreshes the variables from the node values, so set_data_value()
  # has to write through to the node or its value is silently reverted here
  dag$define_tf_log_prob_function()

  expect_identical(log_prob(), swapped)
  expect_true(is.finite(swapped))
})
