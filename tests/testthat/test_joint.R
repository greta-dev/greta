context("joint distributions")

test_that("continuous joint variables can be sampled from", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  x <- joint(normal(0, 1),
             normal(0, 2),
             normal(0, 3))

  sample_distribution(x)

})

test_that("fixed continuous joint distributions can be sampled from", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  obs <- matrix(rnorm(300, 0, 2), 100, 3)
  mu <- variable(dim = 3)
  distribution(obs) <- joint(normal(mu[1], 1),
                             normal(mu[2], 2),
                             normal(mu[3], 3),
                             dim = 100)

  sample_distribution(mu)

})

test_that("fixed discrete joint distributions can be sampled from", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  obs <- matrix(rbinom(300, 1, 0.5), 100, 3)
  probs <- variable(0, 1, dim = 3)
  distribution(obs) <- joint(bernoulli(probs[1]),
                             bernoulli(probs[2]),
                             bernoulli(probs[3]),
                             dim = 100)

  sample_distribution(probs)

})

test_that("joint of fixed and continuous distributions errors", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  expect_error(joint(bernoulli(0.5),
                     normal(0, 1)),
               "combination of discrete and continuous")

})

test_that("joint with insufficient distributions errors", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  expect_error(joint(normal(0, 2)),
               "at least two distributions")

  expect_error(joint(),
               "at least two distributions")

})

test_that("joint of normals has correct density", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  joint_greta <- function(means, sds, dim) {
    joint(normal(means[1], sds[1]),
          normal(means[2], sds[2]),
          normal(means[3], sds[3]),
          dim = dim)
  }

  joint_r <- function(x, means, sds) {
    densities <- matrix(NA,
                        nrow = length(x),
                        ncol = length(means))
    for (i in seq_along(means))
      densities[, i] <- dnorm(x[, i], means[i], sds[i], log = TRUE)

    exp(rowSums(densities))
  }

  params <- list(means = c(-2, 2, 5),
                 sds = c(3, 0.5, 1))

  compare_distribution(joint_greta,
                       joint_r,
                       parameters = params,
                       x = matrix(rnorm(300, -2, 3), 100, 3))

})

test_that("joint of Poissons has correct density", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  joint_greta <- function(rates, dim) {
    joint(poisson(rates[1]),
          poisson(rates[2]),
          poisson(rates[3]),
          dim = dim)
  }

  joint_r <- function(x, rates) {
    densities <- matrix(NA,
                        nrow = length(x),
                        ncol = length(rates))
    for (i in seq_along(rates))
      densities[, i] <- dpois(x[, i], rates[i], log = TRUE)
    exp(rowSums(densities))
  }

  params <- list(rates = c(0.1, 2, 5))

  compare_distribution(joint_greta,
                       joint_r,
                       parameters = params,
                       x = matrix(rpois(300, 3), 100, 3))

})
