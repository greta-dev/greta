set.seed(2020 - 02 - 11)

test_that("opt converges with TF optimisers", {
  skip_if_not(check_tf_version())

  x <- rnorm(5, 2, 0.1)
  z <- variable(dim = 5)
  distribution(x) <- normal(z, 0.1)

  m <- model(z)

  # loop through optimisers that might be expected to work
  optimisers <- list(
    gradient_descent,
    adadelta,
    adagrad,
    adagrad_da,
    adam,
    ftrl,
    proximal_gradient_descent,
    proximal_adagrad,
    rms_prop
  )

  for (optmr in optimisers) {
    (o <- opt(m,
              optimiser = optmr(),
              max_iterations = 200
    ))

    # should have converged in fewer than 200 iterations and be close to truth
    expect_equal(o$convergence, 0)
    expect_lte(o$iterations, 200)
    expect_true(all(abs(x - o$par$z) < 1e-2))
  }
})

test_that("opt converges with SciPy optimisers", {
  skip_if_not(check_tf_version())

  x <- rnorm(3, 2, 0.1)
  z <- variable(dim = 3)
  distribution(x) <- normal(z, 0.1)

  m <- model(z)

  # loop through optimisers that might be expected to work
  optimisers <- list(
    nelder_mead,
    powell,
    cg,
    bfgs,
    newton_cg,
    l_bfgs_b,
    tnc,
    cobyla,
    slsqp
  )

  # check that the right ones warn about deprecation
  deprecated <- list(
    powell,
    momentum,
    cg,
    newton_cg,
    l_bfgs_b,
    tnc,
    cobyla,
    slsqp
  )

  for (optmr in optimisers) {

    # see if it's a deprecated optimiser
    matches <- vapply(deprecated, identical, optmr, FUN.VALUE = logical(1))
    msg <- ifelse(any(matches), "deprecated", NA)

    expect_snapshot(
      o <- opt(m, optimiser = optmr(), max_iterations = 500)
    )

    # should have converged in fewer than 500 iterations and be close to truth

    # can't tell that from output of cobyla
    if (!identical(optmr, cobyla)) {
      expect_equal(o$convergence, 0)
      expect_lte(o$iterations, 500)
    }

    expect_true(all(abs(x - o$par$z) < 1e-2))
  }
})

test_that("opt accepts initial values", {
  skip_if_not(check_tf_version())

  x <- rnorm(5, 2, 0.1)
  z <- variable(dim = 5)
  distribution(x) <- normal(z, 0.1)

  m <- model(z)
  o <- opt(m, initial_values = initials(z = rnorm(5)))

  # should have converged
  expect_equal(o$convergence, 0)

  # should be fewer than 100 iterations
  expect_lte(o$iterations, 100)

  # should be close to the truth
  expect_true(all(abs(x - o$par$z) < 1e-3))
})

test_that("opt returns hessians", {
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
