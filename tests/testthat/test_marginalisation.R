context("marginalisation")

test_that("marginalise errors nicely", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  # not a function
  expect_error(
    marginalise("apple", poisson(0.1), discrete_marginalisation(0:2)),
    "must be an R function"
  )

  # not a greta array
  expect_error(
    marginalise(I, 1:5, discrete_marginalisation(0:2)),
    "must be a variable greta array with a distribution"
  )

  # greta array but no distribution
  expect_error(
    marginalise(I, variable(), discrete_marginalisation()),
    "must be a variable greta array with a distribution"
  )

  # not a marginaliser
  expect_error(
    marginalise(I, poisson(0.1), mean),
    "'method' must be a valid marginalisation method"
  )

  # the following can only be assessed on final model definition

  # function adds variables
  fun <- function(x) {
    x + normal(0, 1)
  }
  lambda <- variable()
  marginalise(fun, poisson(lambda), discrete_marginalisation(0:2))
  expect_error(
    mcmc(model(lambda)),
    "must not create any new variables"
  )

  # function has no distribution
  fun <- function(x) {
    x * 2
  }
  lambda <- variable()
  marginalise(fun, poisson(lambda), discrete_marginalisation(0:2))
  expect_error(
    mcmc(model(lambda)),
    "must constain at least one distribution over data"
  )

})

test_that("discrete_marginalisation errors nicely", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  # greta array, not a numeric
  expect_error(
    discrete_marginalisation(values = variable()),
    "must be an R numeric vector, not a greta array"
  )

  # not a numeric
  expect_error(
    discrete_marginalisation(values = c("apple", "banana")),
    "must be an R numeric vector$"
  )

  # mismatch with distribution
  expect_error(
    marginalise(I, normal(0, 1), discrete_marginalisation(values = 1:5)),
    "can only be used with discrete distributions"
  )

})

test_that("inference runs with discrete marginalisation", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  # spike+slab poisson glm (in reality this is poorly identified)
  n <- 100
  x <- runif(n, -5, 5)
  y <- rpois(n, exp(1))
  p <- variable(lower = 0, upper = 1)
  alpha <- variable()
  beta <- variable()
  likelihood <- function(z, alpha, beta) {
    eta <- alpha + beta * x * z
    lambda <- exp(eta)
    distribution(y) <- poisson(lambda)
  }
  marginalise(likelihood,
              bernoulli(p),
              method = discrete_marginalisation(0:1),
              alpha = alpha,
              beta = beta)
  m <- model(alpha, beta, p)

  expect_ok(o <- opt(m))
  expect_ok(draws <- mcmc(m, warmup = 20, n_samples = 20, verbose = FALSE))

})

test_that("discrete marginalisation gives correct densities", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  # no constraints to no adjustment to account for
  lambda <- variable()
  y <- rnorm(100)
  likelihood <- function(z) {
    distribution(y) <- normal(z, 1)
  }
  values <- 0:5
  marginalise(likelihood,
              poisson(lambda),
              method = discrete_marginalisation(values))
  m <- model(lambda)
  o <- opt(m)

  lambda_val <- runif(1, 0, 5)

  # manually compute expected marginal likelihood given this value of lambda
  wt <- dpois(values, lambda_val)
  wt <- wt / sum(wt)
  dens <- vapply(y, dnorm, values, 1,
                 FUN.VALUE = rep(1, length(values)))
  dens <- apply(dens, 1, prod)
  expected <- log(sum(wt * dens))

  # compute with greta
  m$dag$send_parameters(lambda_val)
  observed <- m$dag$log_density()

  # compare results (within tolerance)
  compare_op(expected, observed)

})

test_that("laplace_approximation errors nicely", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  # bad tolerance
  expect_error(
    laplace_approximation(tolerance = -1),
    "must be a positive, scalar numeric value"
  )

  # bad max iterations
  expect_error(
    laplace_approximation(max_iterations = 0),
    "must be a positive, scalar integer value"
  )

  # mismatch with distribution
  expect_error(
    marginalise(I, normal(0, 1), laplace_approximation()),
    "can only be used with a multivariate normal distribution"
  )

})

test_that("inference runs with laplace approximation", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  # the eight schools model
  treatment_effects <- c(28.39, 7.94, -2.75 , 6.82, -0.64, 0.63, 18.01, 12.16)
  treatment_stddevs <- c(14.9, 10.2, 16.3, 11.0, 9.4, 11.4, 10.4, 17.6)

  int <- variable()
  sd <- variable(lower = 0)
  lik <- function(theta) {
    distribution(treatment_effects) <- normal(t(theta), treatment_stddevs)
  }

  # mock up as a multivariate normal distribution
  sigma <- diag(8) * sd ^ 2
  mu <- ones(1, 8) * int
  marginalise(lik,
              multivariate_normal(mu, sigma),
              laplace_approximation(diagonal_hessian = TRUE))

  m <- model(int, sd)

  expect_ok(o <- opt(m))
  expect_ok(draws <- mcmc(m, warmup = 20, n_samples = 20,
                          verbose = FALSE))

})

test_that("laplace approximation converges on correct posterior", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  # nolint start
  # test vs analytic posterior on 8 schools data with no pooling:
  #   y_i ~ N(theta_i, obs_sd_i ^ 2)
  #   theta_i ~ N(mu, sd ^ 2)
  # the posterior for theta is normal, so laplace should be exact
  # nolint end

  # eight schools data
  y <- c(28.39, 7.94, -2.75 , 6.82, -0.64, 0.63, 18.01, 12.16)
  obs_sd <- c(14.9, 10.2, 16.3, 11.0, 9.4, 11.4, 10.4, 17.6)

  # prior parameters for int, and fixed variance of theta
  mu <- rnorm(1)
  sd <- abs(rnorm(1))

  # analytic solution:

  # nolint start
  # Marginalising int we can write:
  # Bayes theorum gives:
  #   p(theta | y) \propto p(y|theta) p(theta)
  # which with normal densities is:
  #   p(theta_i | y_i) \propto N(y_i | theta_i, obs_sd_i ^ 2) *
  #         N(theta_i | mu, sd ^ 2)
  # which is equivalent to:
  #   p(theta_i | y_i) \propto N(theta_mu_i, theta_var_i)
  #   theta_var_i = 1 / (1 / sd ^ 2 + 1 / obs_sd_i ^ 2)
  #   theta_mu_i = (mu / sd ^ 2 + y_i / obs_sd_i ^ 2) * theta_var_i
  # conjugate prior, see Wikipedia conjugate prior table
  # nolint end

  obs_prec <- 1 / (obs_sd ^ 2)
  prec <- 1 / (sd ^ 2)
  theta_var <- 1 / (obs_prec + prec)
  theta_mu <- (y * obs_prec  + mu * prec) * theta_var
  theta_sd <- sqrt(theta_var)

  # mock up as a multivariate normal distribution
  mean <- ones(1, 8) * mu
  sigma <- diag(8) * sd ^ 2
  lik <- function(theta) {
    distribution(y) <- normal(t(theta), obs_sd)
  }
  out <- marginalise(lik,
                     multivariate_normal(mean, sigma),
                     laplace_approximation(diagonal_hessian = TRUE))
  res <- calculate(mean =  t(out$mean), diag_sigma = diag(out$sigma))
  theta_mu_est <- res$mean
  theta_var_est <- res$diag_sigma

  analytic <- cbind(mean = theta_mu, sd = sqrt(theta_var))
  laplace <- cbind(mean = theta_mu_est, sd = sqrt(theta_var_est))

  # compare these to within a tolerance
  compare_op(analytic, laplace)


})
