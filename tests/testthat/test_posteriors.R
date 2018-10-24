context("posteriors")

test_that("posterior is correct (binomial)", {

  skip_if_not(check_tf_version())
  source("helpers.R")
  skip_if_not_release()

  # analytic solution to the posterior of the paramter of a binomial
  # distribution, with uniform prior
  N <- 100
  pos <- rbinom(1, N, runif(1))
  library(greta)
  theta <- uniform(0, 1)
  distribution(pos) <- binomial(N, theta)
  m <- model(theta)

  draws <- get_enough_draws(m, hmc(), 2000, verbose = FALSE)

  samples <- as.matrix(draws)

  # analytic solution to posterior is beta(1 + pos, 1 + N - pos)
  shape1 <- 1 + pos
  shape2 <- 1 + N - pos

  # qq plot against true quantiles
  quants <- (1:99) / 100
  q_target <- qbeta(quants, shape1, shape2)
  q_est <- quantile(samples, quants)
  plot(q_target ~ q_est, main = "binomial posterior")
  abline(0, 1)

  n_draws <- round(coda::effectiveSize(draws))
  comparison <- rbeta(n_draws, shape1, shape2)
  suppressWarnings(test <- ks.test(samples, comparison))
  expect_gte(test$p.value, 0.01)

})

test_that("samplers are unbiased for bivariate normals", {

  skip_if_not(check_tf_version())
  source("helpers.R")
  skip_if_not_release()

  check_mvn_samples(hmc())
  check_mvn_samples(rwmh())
  check_mvn_samples(slice())

})

test_that("samplers are unbiased for chi-squared", {

  skip_if_not(check_tf_version())
  source("helpers.R")
  skip_if_not_release()

  df <- 5
  x <- chi_squared(df)
  iid <- function(n) rchisq(n, df)

  check_samples(x, iid)

})

test_that("samplers are unbiased for standard uniform", {

  skip_if_not(check_tf_version())
  source("helpers.R")
  skip_if_not_release()

  x <- uniform(0, 1)
  iid <- runif

  check_samples(x, iid)

})

test_that("samplers are unbiased for LKJ", {

  skip_if_not(check_tf_version())
  source("helpers.R")
  skip_if_not_release()

  x <- lkj_correlation(3, 2)[1, 2]
  iid <- function(n)
    rlkjcorr(n, 2, 3)[, 1, 2]

  check_samples(x, iid, hmc(), one_by_one = TRUE)

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

  # Begin Exclude Linting
  # run geweke tests on this model:
  # theta ~ normal(mu1, sd1)
  # x[i] ~ normal(theta, sd2)
  # for i in N
  # End Exclude Linting

  N <- 10
  mu1 <- rnorm(1, 0, 3)
  sd1 <- rlnorm(1)
  sd2 <- rlnorm(1)

  # prior (n draws)
  p_theta <- function(n)
    rnorm(n, mu1, sd1)

  # likelihood
  p_x_bar_theta <- function(theta)
    rnorm(N, theta, sd2)

  # define the greta model (single precision for slice sampler)
  x <- as_data(rep(0, N))
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
               title = "RWMH Geweke test")

  check_geweke(sampler = slice(),
               model = model,
               data = x,
               p_theta = p_theta,
               p_x_bar_theta = p_x_bar_theta,
               title = "slice sampler Geweke test")

})
