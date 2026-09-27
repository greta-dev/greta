test_that("as_tf_function() gets the batch size from inside a tf_while_loop", {
  skip_if_not(check_tf_version())
  set.seed(2026 - 09 - 09)

  # greta.dynamics is the only consumer of as_tf_function(), and it calls it
  # from inside a tf$while_loop, publishing the in-loop batch size for the
  # sub-dag to pick up. Nothing in greta exercised that, so renaming the stash
  # key broke it silently: greta-dev/greta#751, greta-dev/greta.dynamics#45
  tf_test_iterate <- function(state, niter, tf_transition) {
    body <- function(state, iter) {
      assign(".batch_size", tf$shape(state)[[0]], envir = greta_stash)
      list(tf_transition(state), iter + 1L)
    }
    cond <- function(state, iter) tf$squeeze(tf$less(iter, niter))
    out <- tf$while_loop(cond, body, list(state, tf$constant(0L)))
    out[[1]]
  }

  n <- 3
  z <- normal(0, 1, dim = c(n, 1))
  state <- ilogit(z)

  transition <- function(state) state * 0.9
  tf_transition <- as_tf_function(transition, state = state)

  result <- op(
    "test_iterate",
    state,
    operation_args = list(
      niter = tf$constant(3L),
      tf_transition = tf_transition
    ),
    tf_operation = "tf_test_iterate",
    tf_function_env = environment(),
    dim = dim(state)
  )

  y <- as.matrix(rnorm(n, 0.5, 0.1))
  distribution(y) <- normal(result, 0.1)
  m <- model(z)

  expect_ok(mcmc(m, chains = 1, warmup = 5, n_samples = 5, verbose = FALSE))
})
