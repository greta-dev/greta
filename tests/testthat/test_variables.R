context("variables")

test_that("variable() errors informatively", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  # bad types
  expect_error(variable(upper = NA),
               "lower and upper must be numeric")
  expect_error(variable(upper = head),
               "lower and upper must be numeric")

  # good types, bad values
  expect_error(variable(lower = 0:2, upper = 1:2),
               "incompatible dimensions")

  # lower not below upper
  expect_error(variable(lower = 1, upper = 1),
               "upper bounds must be greater than lower bounds")

})

test_that("constrained variable constructors error informatively", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  expect_error(cholesky_variable(dim = 2:3),
               "cholesky variables must be square")
  expect_error(cholesky_variable(dim = rep(2, 3)),
               "a scalar or a vector of length 2")

  expect_error(simplex_variable(1),
               "more than one element")
  expect_error(simplex_variable(c(3, 1)),
               "more than one element")

  expect_error(ordered_variable(1),
               "more than one element")
  expect_error(ordered_variable(c(3, 1)),
               "more than one element")

})

test_that("variable() with universal bounds can be sampled correctly", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  x <- rnorm(3, 0, 10)
  mu <- variable(lower = 2,
                 upper = 6)
  distribution(x) <- normal(mu, 1)
  m <- model(mu)
  draws <- mcmc(m, n_samples = 100, warmup = 1, verbose = FALSE)

  samples <- as.matrix(draws)
  above_lower <- sweep(samples, 2, 2, `>=`)
  below_upper <- sweep(samples, 2, 6, `<=`)

  expect_true(all(above_lower & below_upper))

})

test_that("variable() with vectorised bounds can be sampled correctly", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  x <- rnorm(3, 0, 10)
  lower <- c(-3, -1, 2)
  upper <- c(0, 2, 3)
  mu <- variable(lower = lower,
                 upper = upper)
  distribution(x) <- normal(mu, 1)
  m <- model(mu)
  draws <- mcmc(m, n_samples = 100, warmup = 1, verbose = FALSE)

  samples <- as.matrix(draws)
  above_lower <- sweep(samples, 2, lower, `>=`)
  below_upper <- sweep(samples, 2, upper, `<=`)

  expect_true(all(above_lower & below_upper))

})

test_that("cholesky_variable() can be sampled correctly", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  n <- 3
  u <- cholesky_variable(3)
  sigma <- chol2symm(u)
  x <- t(rnorm(n, 0, 1))
  distribution(x) <- multivariate_normal(zeros(1, n), sigma)
  elems <- sigma[upper.tri(sigma)]
  variances <- diag(sigma)
  m <- model(variances)
  draws <- mcmc(m, n_samples = 100, warmup = 100, verbose = FALSE)

  samples <- as.matrix(draws)

  variances_positive <- sweep(samples, 2, 0, `>=`)
  expect_true(all(variances_positive))

})

test_that("cholesky_variable() correlation can be sampled correctly", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  n <- 3
  u <- cholesky_variable(3, correlation = TRUE)
  sigma <- chol2symm(u)
  x <- t(rnorm(n, 0, 1))
  distribution(x) <- multivariate_normal(zeros(1, n), sigma)
  variances <- diag(sigma)
  correlations <- sigma[upper.tri(sigma, diag = FALSE)]
  m <- model(variances)
  variance_draws <- mcmc(m, n_samples = 100, warmup = 100, verbose = FALSE)
  correlation_draws <- calculate(correlations, variance_draws)

  variance_samples <- as.matrix(variance_draws)
  correlation_samples <- as.matrix(correlation_draws)

  variances_one <- abs(variance_samples - 1) < 1e-3
  correlations_above_minus_one <- sweep(correlation_samples, 2, -1, `>=`)
  correlations_below_one <- sweep(correlation_samples, 2, 1, `<=`)
  expect_true(all(variances_one) &
                all(correlations_above_minus_one) &
                all(correlations_below_one)
  )

})

test_that("simplex_variable() can be sampled correctly", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  k <- 3
  n <- 10
  idx <- sample.int(k, n, replace = TRUE)
  x <- matrix(0, n , k)
  x[cbind(seq_len(n), idx)] <- 1
  prob <- simplex_variable(k)
  distribution(x) <- categorical(prob, n_realisations = n)
  m <- model(prob)
  draws <- mcmc(m, n_samples = 100, warmup = 100, verbose = FALSE)

  samples <- as.matrix(draws)
  positive <- samples > 0
  sum_to_one <- rowSums(samples) == 1
  expect_true(all(positive) & all(sum_to_one))

})

test_that("ordered_variable() can be sampled correctly", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  k <- 3
  n <- 10

  x <- cbind(rnorm(10), matrix(abs(rnorm(n * (k - 1))), n, k - 1))
  x <- t(apply(x, 1, cumsum))
  mu <- ordered_variable(k)
  means <- sweep(ones(n, k), 2, t(mu), "+")
  distribution(x) <- normal(means, 1)
  m <- model(mu)
  draws <- mcmc(m, n_samples = 100, warmup = 100, verbose = FALSE)

  samples <- as.matrix(draws)
  increasing <- t(apply(samples, 1, diff)) > 0
  expect_true(all(increasing))

})
