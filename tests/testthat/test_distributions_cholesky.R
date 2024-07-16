set.seed(2024 - 07 - 04)

test_that("Wishart can use a choleskied Sigma", {
  skip_if_not(check_tf_version())

  sig <- lkj_correlation(2, dim = 2)
  w <- wishart(5, sig)
  m <- model(w, precision = "double")
  expect_ok(draws <- mcmc(m, warmup = 0, n_samples = 5, verbose = FALSE))
})

test_that("Cholesky factor of Wishart should be an upper triangular matrix", {
  skip_if_not(check_tf_version())

  x <- wishart(df = 4, Sigma = diag(3))
  chol_x <- chol(x)
  expect_snapshot(
    calculate(chol_x, nsim = 1)
  )
  calc_x <- calculate(x, nsim = 1)
  calc_chol <- suppressWarnings(calculate(chol_x, nsim = 1))
  expect_equal(dim(calc_chol$chol_x), c(1,3,3))
  calc_chol_mat <- matrix(calc_chol$chol_x, nrow = 3, ncol = 3)
  expect_equal(calc_chol_mat[lower.tri(calc_chol_mat)], c(0,0,0))
})

test_that("Cholesky factor of LJK_correlation should be an upper triangular matrix", {
  skip_if_not(check_tf_version())

  x <- lkj_correlation(eta = 3, dimension = 3)
  chol_x <- chol(x)
  calc_chol <- suppressWarnings(calculate(x, chol_x, nsim = 1))
  expect_equal(dim(calc_chol$chol_x), c(1,3,3))
  calc_chol_mat <- matrix(calc_chol$chol_x, nrow = 3, ncol = 3)
  expect_equal(calc_chol_mat[lower.tri(calc_chol_mat)], c(0,0,0))
})


test_that("Cholesky factor of Wishart should be an upper triangular matrix", {
  skip_if_not(check_tf_version())

  x <- wishart(df = 4, Sigma = diag(3))
  chol_x <- chol(x)
  expect_snapshot_warning(
    calc_chol <- calculate(x, chol_x, nsim = 1)
    )
  expect_equal(dim(calc_chol$chol_x), c(1,3,3))
  calc_chol_mat <- matrix(calc_chol$chol_x, nrow = 3, ncol = 3)
  expect_equal(calc_chol_mat[lower.tri(calc_chol_mat)], c(0,0,0))
})

test_that("Cholesky factor of LJK_correlation should be an upper triangular matrix", {
  skip_if_not(check_tf_version())

  x <- lkj_correlation(eta = 3, dimension = 3)
  chol_x <- chol(x)
  expect_snapshot_warning(
    calc_chol <- calculate(x, chol_x, nsim = 1)
  )
  expect_equal(dim(calc_chol$chol_x), c(1,3,3))
  calc_chol_mat <- matrix(calc_chol$chol_x, nrow = 3, ncol = 3)
  expect_equal(calc_chol_mat[lower.tri(calc_chol_mat)], c(0,0,0))
})

