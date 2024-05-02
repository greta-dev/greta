# test functions

set.seed(123)

# need to get tf object here
library(tensorflow)

# check an expr doesn't error
expect_ok <- function(expr) {
  expect_error(expr, NA)
}

# need to know if things are greta arrays
is_greta_array <- function(x) {
  inherits(x, "greta_array")
}

# check a greta operation and the equivalent R operation give the same output
# e.g. check_op(sum, randn(100, 3))
check_covariance <- function(kernel, X, X_prime = NULL, expected, tol = 1e-6) {
  tf$compat$v1$reset_default_graph()

  if (is.null(X_prime)) {
    X_prime <- X
  }

  if (!is_greta_array(expected)) {
    expected <- as.greta_array(expected)
  }

  greta_out <- greta::calculate(kernel(X, X_prime))[[1]]
  expected <- greta::calculate(expected)[[1]]
  difference <- as.vector(abs(expected - greta_out))
  expect_true(all(difference < tol))
}
