set.seed(2025 - 02 - 13)

test_that("adaptive_hmc() errors when given 1 chain", {
  skip_if_not(check_tf_version())
  x <- normal(0, 1)
  m <- model(x)
  expect_snapshot(
    error = TRUE,
    draws <- mcmc(
      m,
      n_samples = 1,
      warmup = 1,
      chains = 1,
      sampler = adaptive_hmc()
    )
  )
})

test_that("adaptive_hmc() does not error when given 0 warmup", {
  skip_if_not(check_tf_version())
  x <- normal(0, 1)
  m <- model(x)
  expect_no_error(
    draws <- mcmc(
      m,
      n_samples = 1,
      warmup = 0,
      chains = 2,
      sampler = adaptive_hmc()
    )
  )
})

test_that("bad mcmc proposals are rejected", {
  skip_if_not(check_tf_version())

  # set up for numerical rejection of initial location
  x <- rnorm(10000, 1e60, 1)
  z <- normal(-1e60, 1e-60)
  distribution(x) <- normal(z, 1e-60)
  m <- model(z, precision = "single")

  # # catch badness in the progress bar
  out <- get_output(
    mcmc(
      m,
      n_samples = 10,
      warmup = 0,
      pb_update = 10,
      sampler = adaptive_hmc()
    )
  )

  expect_match(out, "100% bad")

  expect_snapshot(
    error = TRUE,
    draws <- mcmc(
      m,
      chains = 2,
      n_samples = 2,
      warmup = 0,
      verbose = FALSE,
      sampler = adaptive_hmc(),
      initial_values = initials(z = 1e120)
    )
  )

  # really bad proposals
  x <- rnorm(100000, 1e120, 1)
  z <- normal(-1e120, 1e-120)
  distribution(x) <- normal(z, 1e-120)
  m <- model(z, precision = "single")
  expect_snapshot(
    error = TRUE,
    mcmc(
      m,
      chains = 1,
      n_samples = 1,
      warmup = 0,
      sampler = adaptive_hmc(),
      verbose = FALSE
    )
  )

  # proposals that are fine, but rejected anyway
  z <- normal(0, 1)
  m <- model(z, precision = "single")
  expect_ok(mcmc(
    m,
    adaptive_hmc(
      epsilon = 100,
    ),
    chains = 2,
    n_samples = 5,
    warmup = 0,
    verbose = FALSE
  ))
})
