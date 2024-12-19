set.seed(2024 - 07 - 04)

test_that("Wishart can use a choleskied Sigma", {
  skip_if_not(check_tf_version())

  sig <- lkj_correlation(2, dim = 2)
  w <- wishart(5, sig)
  m <- model(w, precision = "double")
  tensorflow::set_random_seed(2024-07-30-1520)
  expect_ok(draws <- mcmc(m, warmup = 0, n_samples = 5, verbose = FALSE))
})

test_that("Cholesky factor of Wishart should be a lower triangular matrix", {
  skip_if_not(check_tf_version())

  ## Test if we just do calculate on chol_x
  x <- wishart(df = 4, Sigma = diag(3))
  chol_x <- chol(x)
  expect_snapshot(
    calculate(chol_x, nsim = 1, seed = 2024-10-31-1338)$chol_x[1,,]
  )
  calc_x <- calculate(x, nsim = 1)
  calc_chol <- calculate(chol_x, nsim = 1)

  expect_upper_tri(calc_chol$chol_x)
  expect_square(calc_chol$chol_x)

  ## Test if we do calculate on x and chol_x
  x <- wishart(df = 4, Sigma = diag(3))
  chol_x <- chol(x)
  calc_chol <- calculate(x, chol_x, nsim = 1, seed = 2024-10-31-1342)
  expect_snapshot(
    calc_chol$x[1,,]
  )
  expect_snapshot(
    calc_chol$chol_x[1,,]
  )
  expect_square(calc_chol$chol_x)
  expect_upper_tri(calc_chol$chol_x)
})

test_that("Cholesky factor of LJK_correlation should be a lower triangular matrix", {
  skip_if_not(check_tf_version())

  ## Test if we just do calculate on chol_x
  x <- lkj_correlation(eta = 3, dimension = 3)
  chol_x <- chol(x)
  expect_snapshot(
    calculate(chol_x, nsim = 1, seed = 2024-07-30-1431)$chol_x[1,,]
  )
  calc_x <- calculate(x, nsim = 1, seed = 2024-07-30-1431)
  calc_chol <- calculate(chol_x, nsim = 1, seed = 2024-07-30-1431)

  expect_upper_tri(calc_chol$chol_x)
  expect_square(calc_chol$chol_x)

  ## Test if we do calculate on x and chol_x
  x <- lkj_correlation(eta = 3, dimension = 3)
  chol_x <- chol(x)
  calc_chol <- calculate(x, chol_x, nsim = 1, seed = 2024-07-30-1431)
  expect_snapshot(
    calc_chol$x[1,,]
  )
  expect_snapshot(
    calc_chol$chol_x[1,,]
  )
  expect_square(calc_chol$chol_x)
  expect_upper_tri(calc_chol$chol_x)
})

test_that("Post-MCMC, Wishart distribution stays symmetric, chol remains lower tri",{
  skip_if_not(check_tf_version())
# From https://github.com/greta-dev/greta/issues/585
  x <- wishart(df = 4, Sigma = diag(3))
  m <- model(x)
  tensorflow::set_random_seed(2024-07-30-1431)
  draws <- mcmc(m, warmup = 1, n_samples = 1)

  calcs <- calculate(x, chol(x), nsim = 1, seed = 2024-07-30-1431)
  # ensure that the symmetric matrix is still symmetric
  expect_snapshot(
    calcs
  )

  expect_square(calcs$x)
  expect_square(calcs$`chol(x)`)
  expect_symmetric(calcs$x)
  expect_upper_tri(calcs$`chol(x)`)

})

test_that("Post-MCMC, LKJ distribution stays symmetric, chol remains lower tri",{
  skip_if_not(check_tf_version())
  # From https://github.com/greta-dev/greta/issues/585
  x <- lkj_correlation(eta = 3, dimension = 3)
  m <- model(x)
  tensorflow::set_random_seed(2024-07-30-1431)
  draws <- mcmc(m, warmup = 1, n_samples = 1)

  calcs <- calculate(x, chol(x), nsim = 1, seed = 2024-07-30-1431)
  # ensure that the symmetric matrix is still symmetric
  expect_snapshot(
    calcs
  )

  expect_square(calcs$x)
  expect_square(calcs$`chol(x)`)
  expect_symmetric(calcs$x)
  expect_upper_tri(calcs$`chol(x)`)

})
