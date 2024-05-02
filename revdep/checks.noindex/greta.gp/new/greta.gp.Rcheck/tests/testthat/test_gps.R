test_that("gaussian processes work with numeric vectors", {
  skip_if_not(check_tf_version())

  x <- 1:10
  x2 <- 11:30
  u <- c(2, 4, 6, 8)
  k <- rbf(1, 1)

  # full
  expect_ok(f <- gp(x, k))
  expect_ok(f2 <- project(f, x2))

  # sparse
  expect_ok(f <- gp(x, k, inducing = u))
  expect_ok(f2 <- project(f, x2))
})

test_that("gaussian processes work with numeric matrices", {
  skip_if_not(check_tf_version())

  x <- cbind(1:10, 2:11)
  x2 <- cbind(11:30, 12:31)
  u <- cbind(
    c(2, 4, 6, 8),
    c(3, 5, 7, 9)
  )
  k <- rbf(1, 1)

  # full
  expect_ok(f <- gp(x, k))
  expect_ok(f2 <- project(f, x2))

  # sparse
  expect_ok(f <- gp(x, k, inducing = u))
  expect_ok(f2 <- project(f, x2))
})

test_that("gaussian processes work with greta array matrices", {
  skip_if_not(check_tf_version())

  x <- as_data(cbind(1:10, 2:11))
  x2 <- as_data(cbind(11:30, 12:31))
  u <- as_data(cbind(
    c(2, 4, 6, 8),
    c(3, 5, 7, 9)
  ))
  k <- rbf(1, 1)

  # full
  expect_ok(f <- gp(x, k))
  expect_ok(f2 <- project(f, x2))

  # sparse
  expect_ok(f <- gp(x, k, inducing = u))
  expect_ok(f2 <- project(f, x2))
})

test_that("gaussian processes can be defined in models and sampled from", {
  skip_if_not(check_tf_version())

  len <- greta::uniform(0.2, 0.4)
  var <- greta::lognormal(0., 1.)

  k <- rbf(len, var)

  # full
  f <- gp(1:10, k)
  expect_ok(m <- model(f))
  expect_ok(mcmc(m, warmup = 2, n_samples = 2))

  # sparse
  f <- gp(1:10, k,
    inducing = c(2, 4, 6, 8)
  )
  expect_ok(m <- model(f))
  expect_ok(mcmc(m, warmup = 2, n_samples = 2))
})

test_that("gaussian processes can be projected to new data", {
  skip_if_not(check_tf_version())

  k <- rbf(1, 1)

  # full
  f <- gp(1:10, k)
  f2 <- project(f, 15:20)
  expect_ok(m <- model(f2))
  expect_ok(mcmc(m, warmup = 2, n_samples = 2))

  # sparse
  f <- gp(1:10, k,
    inducing = c(2, 4, 6, 8)
  )
  f2 <- project(f, 15:20)
  expect_ok(m <- model(f2))
  expect_ok(mcmc(m, warmup = 2, n_samples = 2))

  # full & multiple
  f <- gp(1:10, k, n = 3)
  f2 <- project(f, 15:20)
  expect_ok(m <- model(f2))
  expect_ok(mcmc(m, warmup = 2, n_samples = 2))
})

test_that("gaussian processes can be projected with a different kernel", {
  skip_if_not(check_tf_version())

  k1 <- rbf(1, 1)
  k2 <- periodic(1, 1, 1)

  # full
  f <- gp(1:10, k1)
  f2 <- project(f, 15:20, k2)
  expect_ok(m <- model(f2))
  expect_ok(mcmc(m, warmup = 2, n_samples = 2))

  # sparse
  f <- gp(1:10, k1,
    inducing = c(2, 4, 6, 8)
  )
  f2 <- project(f, 15:20, k2)
  expect_ok(m <- model(f2))
  expect_ok(mcmc(m, warmup = 2, n_samples = 2))

  # full & multiple
  f <- gp(1:10, k1, n = 3)
  f2 <- project(f, 15:20, k2)
  expect_ok(m <- model(f2))
  expect_ok(mcmc(m, warmup = 2, n_samples = 2))
})

test_that("project() errors as expected", {
  skip_if_not(check_tf_version())

  f <- normal(0, 1, dim = 10)
  expect_snapshot_error(
    f2 <- project(f, 15:20)
  )
})
