set.seed(2024 - 07 - 04)

test_that("Wishart can use a choleskied Sigma", {
  skip_if_not(check_tf_version())

  sig <- lkj_correlation(2, dim = 2)
  w <- wishart(5, sig)
  m <- model(w, precision = "double")
  expect_ok(draws <- mcmc(m, warmup = 0, n_samples = 5, verbose = FALSE))
})

test_that("Cholesky factor of Wishart should be a lower triangular matrix", {
  skip_if_not(check_tf_version())

  x <- wishart(df = 4, Sigma = diag(3))
  chol_x <- chol(x)
  expect_snapshot(
    calculate(chol_x, nsim = 1)
  )
  calc_x <- calculate(x, nsim = 1)
  calc_chol <- calculate(chol_x, nsim = 1)

  expect_lower_tri(calc_chol$chol_x)
  expect_square(calc_chol$chol_x)
})

test_that("Cholesky factor of Wishart should be a lower triangular matrix", {
  skip_if_not(check_tf_version())

  x <- wishart(df = 4, Sigma = diag(3))
  chol_x <- chol(x)
  expect_snapshot(
    (calc_chol <- calculate(x, chol_x, nsim = 1))
    )
  expect_square(calc_chol$chol_x)
  expect_lower_tri(calc_chol$chol_x)
})


test_that("Cholesky factor of LJK_correlation should be a lower triangular matrix", {
  skip_if_not(check_tf_version())

  x <- lkj_correlation(eta = 3, dimension = 3)
  chol_x <- chol(x)
  calc_chol <- calculate(x, chol_x, nsim = 1)
  expect_square(calc_chol$chol_x)
  expect_lower_tri(calc_chol$chol_x)
})

test_that("Cholesky factor of LJK_correlation remains a lower triangular matrix", {
  skip_if_not(check_tf_version())

  x <- lkj_correlation(eta = 3, dimension = 3)
  chol_x <- chol(x)
  expect_snapshot(
    (calc_chol <- calculate(x, chol_x, nsim = 1))
  )
  expect_square(calc_chol$chol_x)
  expect_lower_tri(calc_chol$chol_x)

})

## TODO
# # continue writing tests to
## resolve concerns around https://github.com/greta-dev/greta/issues/585

# m <- model(x)
# draws <- mcmc(m, warmup = 1, n_samples = 1)
#
# # ensure that the symmetric matrix is still symmetric
# expect_snapshot(
#   calculate(x,
#             chol(x),
#             nsim = 1)
# )

## TODO
## Also write out an issue demonstrating the potential memory issue
## with new cholesky approach
## borrowing from Goldings code where we directly force the cholesky
## to be calculated


## TODO
# further ensure all of the issues in
## https://github.com/greta-dev/greta/labels/cholesky
## are resolved in this branch
