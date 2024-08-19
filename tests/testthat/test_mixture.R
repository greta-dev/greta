test_that("continuous mixture variables can be sampled from", {
  skip_if_not(check_tf_version())

  weights <- uniform(0, 1, 3)
  x <- mixture(normal(0, 1),
    normal(0, 2),
    normal(0, 3),
    weights = weights
  )

  sample_distribution(x)
})

test_that("fixed continuous mixture distributions can be sampled from", {
  skip_if_not(check_tf_version())

  weights <- uniform(0, 1, 3)
  obs <- rnorm(100, 0, 2)
  distribution(obs) <- mixture(normal(0, 1),
    normal(0, 2),
    normal(0, 3),
    weights = weights
  )

  sample_distribution(weights)
})

test_that("fixed discrete mixture distributions can be sampled from", {
  skip_if_not(check_tf_version())

  weights <- uniform(0, 1, 3)
  obs <- rbinom(100, 1, 0.5)
  distribution(obs) <- mixture(bernoulli(0.1),
    bernoulli(0.5),
    bernoulli(0.9),
    weights = weights
  )

  sample_distribution(weights)
})

test_that("mixtures of fixed and continuous distributions errors", {
  skip_if_not(check_tf_version())

  weights <- uniform(0, 1, dim = 2)
  expect_snapshot(error = TRUE,
    mixture(
      bernoulli(0.5),
      normal(0, 1),
      weights = weights
    )
  )
})

test_that("mixtures of multivariate and univariate errors", {
  skip_if_not(check_tf_version())

  weights <- uniform(0, 1, dim = 2)
  expect_snapshot(error = TRUE,
    mixture(
      multivariate_normal(zeros(1, 3), diag(3)),
      normal(0, 1, dim = c(1, 3)),
      weights = weights
    )
  )
})

test_that("mixtures of supports errors", {
  skip_if_not(check_tf_version())

  weights <- c(0.5, 0.5)

  # due to truncation
  expect_snapshot(error = TRUE,
    mixture(
      normal(0, 1, truncation = c(0, Inf)),
      normal(0, 1),
      weights = weights
    )
  )

  # due to bounds
  expect_snapshot(error = TRUE,
    mixture(
      lognormal(0, 1),
      normal(0, 1),
      weights = weights
    )
  )
})

test_that("incorrectly-shaped weights errors", {
  skip_if_not(check_tf_version())

  weights <- uniform(0, 1, dim = c(1, 2))
  expect_snapshot(error = TRUE,
    mixture(
      normal(0, 1),
      normal(0, 2),
      weights = weights
    )
  )
})

test_that("mixtures with insufficient distributions errors", {
  skip_if_not(check_tf_version())

  weights <- uniform(0, 1)

  expect_snapshot(error = TRUE,
    mixture(
      normal(0, 2),
      weights = weights
    )
  )

  expect_snapshot(error = TRUE,
    mixture(weights = weights)
  )

})

test_that("mixture of normals has correct density", {
  skip_if_not(check_tf_version())

  mix_greta <- function(means, sds, weights, dim) {
    mixture(normal(means[1], sds[1], dim),
      normal(means[2], sds[2], dim),
      normal(means[3], sds[3], dim),
      weights = weights
    )
  }

  mix_r <- function(x, means, sds, weights) {
    densities <- matrix(NA,
      nrow = length(x),
      ncol = length(means)
    )
    for (i in seq_along(means)) {
      densities[, i] <- dnorm(x, means[i], sds[i])
    }

    densities <- sweep(densities, 2, weights, "*")
    rowSums(densities)
  }

  params <- list(
    means = c(-2, 2, 5),
    sds = c(3, 0.5, 1),
    weights = c(0.3, 0.6, 0.1)
  )

  compare_distribution(mix_greta,
    mix_r,
    parameters = params,
    x = rnorm(100, -2, 3)
  )
})

test_that("mixture of truncated normals has correct density", {
  skip_if_not(check_tf_version())

  mix_greta <- function(means, sds, weights, dim) {
    mixture(normal(means[1], sds[1], dim, truncation = c(0, Inf)),
      normal(means[2], sds[2], dim, truncation = c(0, Inf)),
      normal(means[3], sds[3], dim, truncation = c(0, Inf)),
      weights = weights
    )
  }

  mix_r <- function(x, means, sds, weights) {
    densities <- matrix(NA,
      nrow = length(x),
      ncol = length(means)
    )

    for (i in seq_along(means)) {
      densities[, i] <- truncdist::dtrunc(
        x = x,
        spec = "norm",
        a = 0,
        b = Inf,
        mean = means[i],
        sd = sds[i]
      )
    }

    densities <- sweep(densities, 2, weights, "*")
    rowSums(densities)
  }

  params <- list(
    means = c(-2, 2, 5),
    sds = c(3, 0.5, 1),
    weights = c(0.3, 0.6, 0.1)
  )

  compare_distribution(mix_greta,
    mix_r,
    parameters = params,
    x = abs(rnorm(100, -2, 3))
  )
})

test_that("mixture of Poissons has correct density", {
  skip_if_not(check_tf_version())

  mix_greta <- function(rates, weights, dim) {
    mixture(poisson(rates[1], dim),
      poisson(rates[2], dim),
      poisson(rates[3], dim),
      weights = weights
    )
  }

  mix_r <- function(x, rates, weights) {
    densities <- matrix(NA,
      nrow = length(x),
      ncol = length(rates)
    )
    for (i in seq_along(rates)) {
      densities[, i] <- dpois(x, rates[i])
    }

    densities <- sweep(densities, 2, weights, "*")
    rowSums(densities)
  }

  params <- list(
    rates = c(0.1, 2, 5),
    weights = c(0.3, 0.6, 0.1)
  )

  compare_distribution(mix_greta,
    mix_r,
    parameters = params,
    x = rpois(100, 3)
  )
})

test_that("mixture of normals with varying weights has correct density", {
  skip_if_not(check_tf_version())

  mix_greta <- function(means, sds, weights, dim) {
    mixture(normal(means[1], sds[1], dim),
      normal(means[2], sds[2], dim),
      normal(means[3], sds[3], dim),
      weights = weights
    )
  }

  mix_r <- function(x, means, sds, weights) {
    out_dim <- dim(x)
    densities <- array(NA, c(length(means), prod(out_dim)))
    for (i in seq_along(means)) {
      densities[i, ] <- dnorm(x, means[i], sds[i])
    }
    dim(weights) <- dim(densities)
    densities <- colSums(densities * weights)
    dim(densities) <- out_dim
    densities
  }

  dim <- c(10, 5, 4)
  n <- prod(dim)

  weights <- matrix(runif(n * 3), 3, n)
  weights <- sweep(weights, 2, colSums(weights), "/")
  dim(weights) <- c(3, dim)

  x <- array(rnorm(n, -2, 3), dim)

  params <- list(
    means = c(-2, 2, 5),
    sds = c(3, 0.5, 1),
    weights = weights
  )

  compare_distribution(mix_greta,
    mix_r,
    parameters = params,
    x = x,
    dim = dim
  )
})
