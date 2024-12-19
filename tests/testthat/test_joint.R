test_that("continuous joint variables can be sampled from", {
  skip_if_not(check_tf_version())

  x <- joint(
    normal(0, 1),
    normal(0, 2),
    normal(0, 3)
  )

  sample_distribution(x)
})

test_that("truncated continuous joint variables can be sampled from", {
  skip_if_not(check_tf_version())

  x <- joint(
    normal(0, 1, truncation = c(0, Inf)),
    normal(0, 2, truncation = c(0, Inf)),
    normal(0, 3, truncation = c(0, Inf))
  )

  sample_distribution(x, lower = 0, upper = Inf)
})

test_that("uniform joint variables can be sampled from", {
  skip_if_not(check_tf_version())

  x <- joint(
    uniform(0, 1),
    uniform(0, 2),
    uniform(-1, 0)
  )

  sample_distribution(x, lower = c(0, 0, -1), upper = c(1, 2, 0))
})

test_that("joint normals with different truncation types can be sampled", {
  skip_if_not(check_tf_version())

  x <- joint(
    normal(0, 1, truncation = c(0, Inf)),
    normal(0, 2, truncation = c(-Inf, 0)),
    normal(-1, 1, truncation = c(1, 2))
  )

  sample_distribution(x, lower = c(0, -Inf, 1), upper = c(Inf, 0, 2))
})

test_that("fixed continuous joint distributions can be sampled from", {
  skip_if_not(check_tf_version())

  obs <- matrix(rnorm(3, 0, 2), 100, 3)
  mu <- variable(dim = 3)
  distribution(obs) <- joint(normal(mu[1], 1),
    normal(mu[2], 2),
    normal(mu[3], 3),
    dim = 100
  )

  sample_distribution(mu)
})

test_that("fixed discrete joint distributions can be sampled from", {
  skip_if_not(check_tf_version())

  obs <- matrix(rbinom(300, 1, 0.5), 100, 3)
  probs <- variable(0, 1, dim = 3)
  distribution(obs) <- joint(
    bernoulli(probs[1]),
    bernoulli(probs[2]),
    bernoulli(probs[3]),
    dim = 100
  )

  sample_distribution(probs)
})

test_that("joint of fixed and continuous distributions errors", {
  skip_if_not(check_tf_version())

  expect_snapshot(error = TRUE,
    joint(
      bernoulli(0.5),
      normal(0, 1)
    )
  )
})

test_that("joint with insufficient distributions errors", {
  skip_if_not(check_tf_version())

  expect_snapshot(error = TRUE,
    joint(normal(0, 2))
  )

  expect_snapshot(error = TRUE,
    joint()
  )
})

test_that("joint with non-scalar distributions errors", {
  skip_if_not(check_tf_version())

  expect_snapshot(error = TRUE,
    joint(
      normal(0, 2, dim = 3),
      normal(0, 1, dim = 3)
    )
  )
})

test_that("joint of normals has correct density", {
  skip_if_not(check_tf_version())

  joint_greta <- function(means, sds, dim) {
    joint(normal(means[1], sds[1]),
      normal(means[2], sds[2]),
      normal(means[3], sds[3]),
      dim = dim
    )
  }

  joint_r <- function(x, means, sds) {
    densities <- matrix(NA,
      nrow = length(x),
      ncol = length(means)
    )
    for (i in seq_along(means)) {
      densities[, i] <- dnorm(x[, i], means[i], sds[i], log = TRUE)
    }

    exp(rowSums(densities))
  }

  params <- list(
    means = c(-2, 2, 5),
    sds = c(3, 0.5, 1)
  )

  compare_distribution(joint_greta,
    joint_r,
    parameters = params,
    x = matrix(rnorm(300, -2, 3), 100, 3)
  )
})

test_that("joint of truncated normals has correct density", {
  skip_if_not(check_tf_version())

  joint_greta <- function(means, sds, lower, upper, dim) {
    joint(normal(means[1], sds[1], truncation = c(lower[1], upper[1])),
      normal(means[2], sds[2], truncation = c(lower[2], upper[2])),
      normal(means[3], sds[3], truncation = c(lower[3], upper[3])),
      dim = dim
    )
  }

  joint_r <- function(x, means, sds, lower, upper) {
    densities <- matrix(NA,
      nrow = length(x),
      ncol = length(means)
    )
    for (i in seq_along(means)) {
      densities[, i] <- truncdist::dtrunc(x[, i],
        "norm",
        a = lower[i],
        b = upper[i],
        mean = means[i],
        sd = sds[i]
      )
    }
    densities <- log(densities)

    exp(rowSums(densities))
  }

  params <- list(
    means = c(-2, 2, 5),
    sds = c(3, 0.5, 1),
    lower = c(0, -1, -Inf),
    upper = c(Inf, 1, 0)
  )

  fun <- function(mean, sd, lower, upper) {
    truncdist::rtrunc(100, "norm", lower, upper, mean = mean, sd = sd)
  }
  x <- mapply(fun, params$means, params$sds, params$lower, params$upper)

  compare_distribution(joint_greta,
    joint_r,
    parameters = params,
    x = x
  )
})

test_that("joint of uniforms has correct density", {
  skip_if_not(check_tf_version())

  joint_greta <- function(lower, upper, dim) {
    joint(uniform(lower[1], upper[1]),
      uniform(lower[2], upper[2]),
      uniform(lower[3], upper[3]),
      dim = dim
    )
  }

  joint_r <- function(x, lower, upper) {
    densities <- matrix(NA,
      nrow = length(x),
      ncol = length(lower)
    )
    for (i in seq_along(lower)) {
      densities[, i] <- dunif(x[, i], lower[i], upper[i], log = TRUE)
    }

    exp(rowSums(densities))
  }

  params <- list(
    lower = c(-2, 0.5, 1),
    upper = c(3, 2, 5)
  )

  fun <- function(lower, upper) {
    runif(100, lower, upper)
  }
  x <- mapply(fun, params$lower, params$upper)

  compare_distribution(joint_greta,
    joint_r,
    parameters = params,
    x = x
  )
})

test_that("joint of Poissons has correct density", {
  skip_if_not(check_tf_version())

  joint_greta <- function(rates, dim) {
    joint(poisson(rates[1]),
      poisson(rates[2]),
      poisson(rates[3]),
      dim = dim
    )
  }

  joint_r <- function(x, rates) {
    densities <- matrix(NA,
      nrow = length(x),
      ncol = length(rates)
    )
    for (i in seq_along(rates)) {
      densities[, i] <- dpois(x[, i], rates[i], log = TRUE)
    }
    exp(rowSums(densities))
  }

  params <- list(rates = c(0.1, 2, 5))

  compare_distribution(joint_greta,
    joint_r,
    parameters = params,
    x = matrix(rpois(300, 3), 100, 3)
  )
})
