context("posteriors")

test_that("posterior is correct (binomial)", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  # analytic solution to the posterior of the paramter of a binomial
  # distribution, with uniform prior
  n <- 100
  pos <- rbinom(1, n, runif(1))
  library(greta)
  theta <- uniform(0, 1)
  distribution(pos) <- binomial(n, theta)
  m <- model(theta)

  draws <- get_enough_draws(m, n_effective = 5000)

  samples <- as.matrix(draws)

  # analytic solution to posterior is beta(1 + pos, 1 + N - pos)
  shape1 <- 1 + pos
  shape2 <- 1 + n - pos

  # qq plot against true quantiles
  quants <- (1:99) / 100
  q_target <- qbeta(quants, shape1, shape2)
  q_est <- quantile(samples, quants)
  plot(q_target ~ q_est, main = "binomial posterior")
  abline(0, 1)

  n_draws <- round(coda::effectiveSize(draws))
  comparison <- rbeta(n_draws, shape1, shape2)
  suppressWarnings(test <- ks.test(samples, comparison))
  expect_gte(test$p.value, 0.05)

})

test_that("posterior is correct (normal)", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  # nolint start
  # test vs analytic posterior on 8 schools data with no pooling:
  #   y_i ~ N(theta_i, obs_sd_i ^ 2)
  #   theta_i ~ N(mu, sd ^ 2)
  # nolint end

  # eight schools data
  y <- c(28.39, 7.94, -2.75 , 6.82, -0.64, 0.63, 18.01, 12.16)
  obs_sd <- c(14.9, 10.2, 16.3, 11.0, 9.4, 11.4, 10.4, 17.6)

  # prior parameters for int, and fixed variance of theta
  mu <- rnorm(1)
  sd <- abs(rnorm(1))

  # nolint start
  # Bayes theorum gives:
  #   p(theta | y) \propto p(y|theta) p(theta)
  # which with normal densities is:
  #   p(theta_i | y_i) \propto N(y_i | theta_i, obs_sd_i ^ 2) * N(theta_i | mu, sd ^ 2)
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
  analytic <- cbind(mean = theta_mu, sd = sqrt(theta_var))

  # mcmc solution:
  theta <- normal(mu, sd, dim = 8)
  distribution(y) <- normal(theta, obs_sd)
  m <- model(theta)

  # draw at least 5000 effective samples of each parameter from the posterior
  draws <- get_enough_draws(m, n_effective = 5000)

  mcmc_stats <- summary(draws)$statistics
  mcmc <- mcmc_stats[, 1:2]
  error <- abs(mcmc - analytic)
  mcmc_se <- mcmc_stats[, 4]

  # ratio of error to that expected due to Monte Carlo noise
  error_multiples <- error / cbind(mcmc_se, mcmc_se)

  within_error_margin <- error_multiples < qnorm(1 - 0.005)
  expect_true(all(within_error_margin))

})

test_that("samplers are unbiased for bivariate normals", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  check_mvn_samples(hmc())
  check_mvn_samples(rwmh())
  check_mvn_samples(slice())

})

test_that("samplers are unbiased for chi-squared", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  df <- 5
  x <- chi_squared(df)
  iid <- function(n) rchisq(n, df)

  check_samples(x, iid)

})

test_that("samplers are unbiased for standard uniform", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  x <- uniform(0, 1)
  iid <- runif

  check_samples(x, iid)

})

test_that("samplers are unbiased for LKJ", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  x <- lkj_correlation(3, 2)[1, 2]
  iid <- function(n)
    rlkjcorr(n, 3, 2)[, 1, 2]

  check_samples(x, iid)

})

test_that("samplers are unbiased for Wishart", {

  skip_if_not(check_tf_version())
  source("helpers.R")
  skip_if_not_release()

  sigma <- matrix(c(1.2, 0.7, 0.7, 2.3),
                  2, 2)
  df <- 4
  x <- wishart(df, sigma)[1, 2]
  iid <- function(n)
    rWishart(n, df, sigma)[1, 2, ]

  check_samples(x, iid, one_by_one = TRUE)

})

test_that("samplers pass geweke tests", {

  skip_if_not(check_tf_version())
  source("helpers.R")
  skip_if_not_release()

  # nolint start
  # run geweke tests on this model:
  # theta ~ normal(mu1, sd1)
  # x[i] ~ normal(theta, sd2)
  # for i in N
  # nolint end

  n <- 10
  mu1 <- rnorm(1, 0, 3)
  sd1 <- rlnorm(1)
  sd2 <- rlnorm(1)

  # prior (n draws)
  p_theta <- function(n)
    rnorm(n, mu1, sd1)

  # likelihood
  p_x_bar_theta <- function(theta)
    rnorm(n, theta, sd2)

  # define the greta model (single precision for slice sampler)
  x <- as_data(rep(0, n))
  greta_theta <- normal(mu1, sd1)
  distribution(x) <- normal(greta_theta, sd2)
  model <- model(greta_theta, precision = "single")

  # run tests on all available samplers
  check_geweke(sampler = hmc(),
               model = model,
               data = x,
               p_theta = p_theta,
               p_x_bar_theta = p_x_bar_theta,
               title = "HMC Geweke test")

  check_geweke(sampler = rwmh(),
               model = model,
               data = x,
               p_theta = p_theta,
               p_x_bar_theta = p_x_bar_theta,
               warmup = 2000,
               title = "RWMH Geweke test")

  check_geweke(sampler = slice(),
               model = model,
               data = x,
               p_theta = p_theta,
               p_x_bar_theta = p_x_bar_theta,
               title = "slice sampler Geweke test")

})
