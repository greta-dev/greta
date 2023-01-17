set.seed(2020 - 02 - 11)

test_that("opt converges with TF optimisers", {
  skip_if_not(check_tf_version())

  x <- rnorm(5, 2, 0.1)
  z <- variable(dim = 5)
  distribution(x) <- normal(z, 0.1)

  m <- model(z)

  optimisers <- tibble::lst(
    gradient_descent,
    adadelta,
    adagrad,
    adam,
    ftrl,
    rms_prop
  )

  opt_df <- opt_df_run(optimisers, m, x)
  tidied_opt <- tidy_optimisers(opt_df, tolerance = 1e-2)

  expect_true(all(tidied_opt$convergence == 0))
  expect_true(all(tidied_opt$iterations <= 200))
  expect_true(all(tidied_opt$close_to_truth))


})

test_that("opt gives appropriate warning with deprecated optimisers in TFP", {
  skip_if_not(check_tf_version())

  x <- rnorm(5, 2, 0.1)
  z <- variable(dim = 5)
  distribution(x) <- normal(z, 0.1)

  m <- model(z)

  expect_snapshot_warning(
    opt(m, optimiser = adagrad_da())
  )
  expect_snapshot_warning(
    opt(m, optimiser = proximal_adagrad())
  )
  expect_snapshot_warning(
    opt(m, optimiser = proximal_gradient_descent())
  )

})

test_that("opt converges with TFP optimisers", {
  skip_if_not(check_tf_version())

  x <- rnorm(3, 2, 0.1)
  z <- variable(dim = 3)
  distribution(x) <- normal(z, 0.1)

  m <- model(z)

  # There are only 2 TFP optimisers: bfgs & nelder_mead
  # check through each individually
  expect_snapshot(
    o <- opt(m, optimiser = bfgs(), max_iterations = 500)
  )

  # should have converged in fewer than 500 iterations and be close to truth
  expect_equal(o$convergence, 0)
  expect_lte(o$iterations, 500)
  expect_true(all(abs(x - o$par$z) < 1e-2))

  expect_snapshot(
    o <- opt(m, optimiser = nelder_mead(), max_iterations = 500)
  )

  # should have converged in fewer than 500 iterations and be close to truth
  expect_equal(o$convergence, 0)
  expect_lte(o$iterations, 500)
  expect_true(all(abs(x - o$par$z) < 1e-2))

})

test_that("opt fails with defunct optimisers", {

  skip_if_not(check_tf_version())

  x <- rnorm(3, 2, 0.1)
  z <- variable(dim = 3)
  distribution(x) <- normal(z, 0.1)

  m <- model(z)

  # check that the right ones error about defunct
  expect_snapshot_error(o <- opt(m, optimiser = powell()))
  expect_snapshot_error(o <- opt(m, optimiser = momentum()))
  expect_snapshot_error(o <- opt(m, optimiser = cg()))
  expect_snapshot_error(o <- opt(m, optimiser = newton_cg()))
  expect_snapshot_error(o <- opt(m, optimiser = l_bfgs_b()))
  expect_snapshot_error(o <- opt(m, optimiser = tnc()))
  expect_snapshot_error(o <- opt(m, optimiser = cobyla()))
  expect_snapshot_error(o <- opt(m, optimiser = slsqp()))

})

test_that("opt accepts initial values for TF optimisers", {
  skip_if_not(check_tf_version())

  x <- rnorm(5, 2, 0.1)
  z <- variable(dim = 5)
  distribution(x) <- normal(z, 0.1)

  m <- model(z)
  o <- opt(m,
           initial_values = initials(z = rnorm(5)),
           optimiser = gradient_descent())

  # should have converged
  expect_equal(o$convergence, 0)

  # should be fewer than 100 iterations
  expect_lte(o$iterations, 100)

  # should be close to the truth
  expect_true(all(abs(x - o$par$z) < 1e-3))
})

test_that("opt accepts initial values for TFP optimisers", {
  skip_if_not(check_tf_version())

  x <- rnorm(5, 2, 0.1)
  z <- variable(dim = 5)
  distribution(x) <- normal(z, 0.1)

  m <- model(z)
  o <- opt(m,
           initial_values = initials(z = rnorm(5)),
           optimiser = bfgs())

  # should have converged
  expect_equal(o$convergence, 0)

  # should be fewer than 100 iterations
  expect_lte(o$iterations, 100)

  # should be close to the truth
  expect_true(all(abs(x - o$par$z) < 1e-3))
})

test_that("TF opt returns hessians", {
  skip_if_not(check_tf_version())

  sd <- runif(5)
  x <- rnorm(5, 2, 0.1)
  z <- variable(dim = 5)
  distribution(x) <- normal(z, sd)

  m <- model(z)
  o <- opt(m, hessian = TRUE, optimiser = gradient_descent())

  hess <- o$hessian$z

  # should be a 5x5 numeric matrix
  expect_true(inherits(hess, "matrix"))
  expect_true(is.numeric(hess))
  expect_true(identical(dim(hess), c(5L, 5L)))

  # the model density is IID normal, so we should be able to recover the SD
  approx_sd <- sqrt(diag(solve(hess)))
  expect_true(all(abs(approx_sd - sd) < 1e-9))
})

test_that("TFP opt returns hessians", {
  skip_if_not(check_tf_version())

  sd <- runif(5)
  x <- rnorm(5, 2, 0.1)
  z <- variable(dim = 5)
  distribution(x) <- normal(z, sd)

  m <- model(z)
  o <- opt(m, hessian = TRUE)

  hess <- o$hessian$z

  # should be a 5x5 numeric matrix
  expect_true(inherits(hess, "matrix"))
  expect_true(is.numeric(hess))
  expect_true(identical(dim(hess), c(5L, 5L)))

  # the model density is IID normal, so we should be able to recover the SD
  approx_sd <- sqrt(diag(solve(hess)))
  expect_true(all(abs(approx_sd - sd) < 1e-9))
})
