test_that("base kernels evaluate self-covariance correctly", {
  skip_if_not(check_tf_version())

  n <- 5
  x_ <- rnorm(n)
  x <- as_data(x_)

  per <- runif(1)
  var <- runif(1)
  len <- runif(1)
  deg <- 2.
  offs <- runif(1)
  alph <- 2.

  r <- as.matrix(dist(x_ / len))

  # bias
  check_covariance(bias(var),
    x,
    expected = matrix(var, n, n)
  )

  # constant
  check_covariance(constant(var),
    x,
    expected = matrix(var, n, n)
  )

  # white
  check_covariance(white(var),
    x,
    expected = diag(var, n)
  )

  # iid
  check_covariance(iid(var),
    x,
    expected = diag(var, n)
  )

  # iid
  x2 <- as_data(rep(1:2, c(3, 4)))
  expected <- matrix(0, 7, 7)
  expected[1:3, 1:3] <- expected[4:7, 4:7] <- var
  check_covariance(iid(var),
    x2,
    expected = expected
  )

  # linear
  check_covariance(linear(var),
    x,
    expected = outer(x_, x_) * var
  )

  # polynomial
  check_covariance(polynomial(var, offset = offs, degree = deg),
    x,
    expected = (offs + outer(x_, x_) * var)^deg
  )

  # rbf
  check_covariance(rbf(len, var),
    x,
    expected = var * exp(-0.5 * r^2)
  )

  # expo
  check_covariance(expo(len, var),
    x,
    expected = var * exp(-0.5 * r)
  )

  # mat12
  check_covariance(mat12(len, var),
    x,
    expected = var * exp(-r)
  )

  # mat32
  check_covariance(mat32(len, var),
    x,
    expected = var * (1 + sqrt(3) * r) * exp(-sqrt(3) * r)
  )

  # mat52
  check_covariance(mat52(len, var),
    x,
    expected = var * (1 + sqrt(5) * r + 5 / 3 * r^2) *
      exp(-sqrt(5) * r)
  )

  # rational_quadratic
  check_covariance(rational_quadratic(len, var, alph),
    x,
    expected = var * (1 + r^2 / (2 * alph))^(-alph)
  )

  # cosine
  check_covariance(cosine(len, var),
    x,
    expected = var * cos(r)
  )

  # periodic
  exp_arg <- pi * as.matrix(dist(x)) / per
  exp_arg <- sin(exp_arg) / len[1]
  check_covariance(periodic(per, len[1], var),
    x,
    expected = var * exp(-0.5 * exp_arg^2), tol = 1e-4
  )
})

test_that("base kernels evaluate covariance with different number of rows", {
  skip_if_not(check_tf_version())

  nc <- 3
  nr1 <- 5
  nr2 <- 10
  x <- matrix(rnorm(nc * nr1), nrow = nr1)
  x2 <- matrix(rnorm(nc * nr2), nrow = nr2)

  per <- runif(1)
  var <- runif(1)
  vars <- runif(nc)
  len <- runif(nc)
  deg <- 2.
  offs <- runif(1)
  alph <- 2.

  # redefine this for two matrices
  z <- sweep(x, 2, len, "/")
  z2 <- sweep(x2, 2, len, "/")
  r2 <- -2. * z %*% t(z2)
  z_sq <- rowSums(z^2)
  z2_sq <- rowSums(z2^2)
  r2 <- sweep(r2, 1, z_sq, "+")
  r2 <- sweep(r2, 2, z2_sq, "+")
  r <- sqrt(r2)

  # bias
  check_covariance(bias(var),
    x, x2,
    expected = matrix(var, nr1, nr2)
  )

  # constant
  check_covariance(constant(var),
    x, x2,
    expected = matrix(var, nr1, nr2)
  )

  # white
  check_covariance(white(var),
    x, x2,
    expected = zeros(nr1, nr2)
  )

  # linear
  check_covariance(linear(vars),
    x, x2,
    expected = sweep(x, 2, vars, "*") %*% t(x2)
  )

  # polynomial
  check_covariance(polynomial(vars, offset = offs, degree = deg),
    x, x2,
    expected = (offs + sweep(x, 2, vars, "*") %*% t(x2))^deg
  )

  # rbf
  check_covariance(rbf(len, var),
    x, x2,
    expected = var * exp(-0.5 * r2)
  )

  # expo
  check_covariance(expo(len, var),
    x, x2,
    expected = var * exp(-0.5 * r)
  )

  # mat12
  check_covariance(mat12(len, var),
    x, x2,
    expected = var * exp(-r)
  )

  # mat32
  check_covariance(mat32(len, var),
    x, x2,
    expected = var * (1 + sqrt(3) * r) * exp(-sqrt(3) * r)
  )

  # mat52
  check_covariance(mat52(len, var),
    x, x2,
    expected = var * (1 + sqrt(5) * r + 5 / 3 * r^2) *
      exp(-sqrt(5) * r)
  )

  # rational_quadratic
  check_covariance(rational_quadratic(len, var, alph),
    x, x2,
    expected = var * (1 + r^2 / (2 * alph))^(-alph)
  )

  # cosine
  check_covariance(cosine(len, var),
    x, x2,
    expected = var * cos(r)
  )

  # periodic
  r2_ <- -2. * x %*% t(x2)
  x_sq <- rowSums(x^2)
  x2_sq <- rowSums(x2^2)
  r2_ <- sweep(r2_, 1, x_sq, "+")
  r2_ <- sweep(r2_, 2, x2_sq, "+")
  r_ <- sqrt(r2_)
  exp_arg <- pi * r_ / per
  exp_arg <- sin(exp_arg) / len[1]
  check_covariance(periodic(per, len[1], var),
    x, x2,
    expected = var * exp(-0.5 * exp_arg^2), tol = 1e-4
  )
})

test_that("compound kernels evaluate self-covariance correctly", {
  skip_if_not(check_tf_version())

  n <- 5
  x_ <- rnorm(n)
  x <- as_data(x_)

  var <- runif(1)
  len <- runif(1)

  r <- as.matrix(dist(x_ / len))

  # additive
  check_covariance(linear(var) + rbf(len, var),
    x,
    expected = (outer(x_, x_) * var) + (var * exp(-0.5 * r^2))
  )

  # multiplicative
  check_covariance(linear(var) * rbf(len, var),
    x,
    expected = (outer(x_, x_) * var) * (var * exp(-0.5 * r^2))
  )
})

test_that("compound kernels can act on specific dimensions", {
  skip_if_not(check_tf_version())

  n <- 5
  x_ <- cbind(rnorm(n), runif(n))
  x <- as_data(x_)

  var <- runif(1)
  len <- runif(1)

  x_1 <- x_[, 1]
  r_2 <- as.matrix(dist(x_[, 2] / len))

  # additive
  check_covariance(linear(var, columns = 1) + rbf(len, var, columns = 2),
    x,
    expected = (outer(x_1, x_1) * var) + (var * exp(-0.5 * r_2^2))
  )

  # multiplicative
  check_covariance(linear(var, columns = 1) * rbf(len, var, columns = 2),
    x,
    expected = (outer(x_1, x_1) * var) * (var * exp(-0.5 * r_2^2))
  )
})

test_that("kernels error on badly shaped inputs", {
  skip_if_not(check_tf_version())

  kernel <- rbf(1, 1)

  bad_x <- greta_array(1:24, dim = c(2, 3, 4))
  x1 <- greta_array(1:10)
  x2 <- greta_array(1:10, dim = c(2, 5))

  expect_snapshot_error(
    kernel(bad_x)
  )
  expect_snapshot_error(
    kernel(x1, x2)
  )
})

test_that("kernel constructors error on bad columns", {
  skip_if_not(check_tf_version())

  expect_snapshot_error(
    rbf(1, 1, columns = 1:2)
  )

  expect_snapshot_error(
    rbf(1, 1, columns = -1)
  )
})

test_that("kernels error if combined with other things", {
  skip_if_not(check_tf_version())

  expect_snapshot_error(
    bias(1) + 1
  )
  expect_snapshot_error(
    1 + bias(1)
  )

  expect_snapshot_error(
    bias(1) * 1
  )
  expect_snapshot_error(
    1 * bias(1)
  )
})

test_that("kernels print their own names", {
  skip_if_not(check_tf_version())

  expect_snapshot_output(
    print(bias(1))
  )

  expect_snapshot_output(
    print(linear(1))
  )

  expect_snapshot_output(
    print(rbf(1, 1))
  )

  expect_snapshot_output(
    print(expo(1, 1))
  )

  expect_snapshot_output(
    print(mat12(1, 1))
  )

  expect_snapshot_output(
    print(mat32(1, 1))
  )

  expect_snapshot_output(
    print(mat52(1, 1))
  )

  expect_snapshot_output(
    print(periodic(1, 1, 1))
  )

  expect_snapshot_output(
    print(bias(1) + bias(1))
  )

  expect_snapshot_output(
    print(bias(1) * bias(1))
  )
})
