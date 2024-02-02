if (check_tf_version()) {
  tensorflow::tf$compat$v1$reset_default_graph()
}

set.seed(2020 - 02 - 11)


test_that("opt converges with TF optimisers", {
  skip_if_not(check_tf_version())
  skip_on_cran()

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
    momentum,
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
  skip_on_cran()

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
  skip_on_cran()

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
  skip_on_cran()

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

test_that("greta arrays passed into mcmc fail appropriately", {
  skip_if_not(check_tf_version())
  skip_on_cran()
  x <- normal(0, 1)
  expect_snapshot_error(
    mcmc(x)
  )
})

test_that("bad mcmc proposals are rejected", {
  skip_if_not(check_tf_version())
  skip_on_cran()

  # set up for numerical rejection of initial location
  x <- rnorm(10000, 1e6, 1)
  z <- normal(-1e6, 1e-6)
  distribution(x) <- normal(z, 1e6)
  m <- model(z, precision = "single")

  # # catch badness in the progress bar
  # with_mock(
  #   `greta:::create_progress_bar` = mock_create_progress_bar,
  #   out <- get_output(mcmc(m, n_samples = 10, warmup = 0, pb_update = 10)),
  #   expect_match(out, "100% bad")
    # expect_snapshot(
    #   draws <- mcmc(m, n_samples = 10, warmup = 0, pb_update = 10)
    # )
  # )
    out <- get_output(
      mcmc(m, n_samples = 10, warmup = 0, pb_update = 10)
      )
    expect_match(out, "100% bad")

    expect_snapshot_error(
      mcmc(m,
           chains = 1,
           n_samples = 1,
           warmup = 0,
           verbose = FALSE,
           initial_values = initials(z = 1e20)
      )
    )

  # really bad proposals
  x <- rnorm(100000, 1e12, 1)
  z <- normal(-1e12, 1e-12)
  distribution(x) <- normal(z, 1e-12)
  m <- model(z, precision = "single")
  expect_snapshot_error(
    mcmc(m, chains = 1, n_samples = 1, warmup = 0, verbose = FALSE)
  )

  # proposals that are fine, but rejected anyway
  z <- normal(0, 1)
  m <- model(z, precision = "single")
  expect_ok(mcmc(m,
    hmc(
      epsilon = 100,
      Lmin = 1,
      Lmax = 1
    ),
    chains = 1,
    n_samples = 5,
    warmup = 0,
    verbose = FALSE
  ))
})

test_that("mcmc works with verbosity and warmup", {
  skip_if_not(check_tf_version())
  skip_on_cran()

  x <- rnorm(10)
  z <- normal(0, 1)
  distribution(x) <- normal(z, 1)
  m <- model(z)
  quietly(expect_ok(mcmc(m, n_samples = 50, warmup = 50, verbose = TRUE)))
})

test_that("mcmc works with multiple chains", {
  skip_if_not(check_tf_version())
  skip_on_cran()

  x <- rnorm(10)
  z <- normal(0, 1)
  distribution(x) <- normal(z, 1)
  m <- model(z)

  # multiple chains, automatic initial values
  quietly(expect_ok(mcmc(m, warmup = 10, n_samples = 10, chains = 2,
                         verbose = FALSE)))

  # multiple chains, user-specified initial values
  inits <- list(initials(z = 1), initials(z = 2))
  quietly(expect_ok(mcmc(m,
    warmup = 10, n_samples = 10, chains = 2,
    initial_values = inits,
    verbose = FALSE
  )))
})

test_that("mcmc handles initial values nicely", {
  skip_if_not(check_tf_version())
  skip_on_cran()

  # preserve R version
  current_r_version <- paste0(R.version$major,".", R.version$minor)
  required_r_version <- "3.6.0"
  old_rng_r <- compareVersion(required_r_version, current_r_version) <= 0

  if (old_rng_r) {
    suppressWarnings(expr = {
      RNGkind(sample.kind = "Rounding")
      set.seed(2020 - 02 - 11)
    })
  }

  x <- rnorm(10)
  z <- normal(0, 1)
  distribution(x) <- normal(z, 1)
  m <- model(z)

  # too many sets of initial values
  inits <- replicate(3, initials(z = rnorm(1)), simplify = FALSE)
  expect_snapshot_error(
    mcmc(m,
         warmup = 10, n_samples = 10, verbose = FALSE,
         chains = 2, initial_values = inits
    )
  )

  # initial values have the wrong length
  inits <- replicate(2, initials(z = rnorm(2)), simplify = FALSE)
  expect_snapshot_error(
    mcmc(m,
         warmup = 10, n_samples = 10, verbose = FALSE,
         chains = 2, initial_values = inits
    )
  )

  inits <- initials(z = rnorm(1))
  quietly(
    expect_snapshot(
    mcmc(m,
      warmup = 10, n_samples = 10,
      chains = 2, initial_values = inits,
      verbose = FALSE
    )
    )
  )
})

test_that("progress bar gives a range of messages", {
  skip_if_not(check_tf_version())
  skip_on_cran()

  # 10/1010 should be <1%
  expect_snapshot(draws <- mock_mcmc(1010))

  # 10/500 should be 2%
  expect_snapshot(draws <- mock_mcmc(500))

  # 10/10 should be 100%
  expect_snapshot(draws <- mock_mcmc(10))

})

test_that("extra_samples works", {
  skip_if_not(check_tf_version())
  skip_on_cran()

  # set up model
  a <- normal(0, 1)
  m <- model(a)

  draws <- mcmc(m, warmup = 10, n_samples = 10, verbose = FALSE)

  more_draws <- extra_samples(draws, 20, verbose = FALSE)

  expect_true(inherits(more_draws, "greta_mcmc_list"))
  expect_true(coda::niter(more_draws) == 30)
  expect_true(coda::nchain(more_draws) == 4)
})

test_that("trace_batch_size works", {
  skip_if_not(check_tf_version())
  skip_on_cran()

  # set up model
  a <- normal(0, 1)
  m <- model(a)

  draws <-
    mcmc(
      m,
      warmup = 10,
      n_samples = 10,
      verbose = FALSE,
      trace_batch_size = 3
    )

  more_draws <- extra_samples(draws, 20, verbose = FALSE, trace_batch_size = 6)

  expect_true(inherits(more_draws, "greta_mcmc_list"))
  expect_true(coda::niter(more_draws) == 30)
  expect_true(coda::nchain(more_draws) == 4)
})

test_that("stashed_samples works", {
  skip_if_not(check_tf_version())
  skip_on_cran()

  # set up model
  a <- normal(0, 1)
  m <- model(a)

  draws <- mcmc(m, warmup = 10, n_samples = 10, verbose = FALSE)

  # with a completed sample, this should be NULL
  ans <- stashed_samples()
  expect_null(ans)

  # mock up a stash
  stash <- greta:::greta_stash
  samplers_stash <- replicate(2, list(
    traced_free_state = list(as.matrix(rnorm(17))),
    traced_values = list(as.matrix(rnorm(17))),
    thin = 1,
    model = m
  ), simplify = FALSE)
  assign("samplers", samplers_stash, envir = stash)

  # should convert to a greta_mcmc_list
  ans <- stashed_samples()
  expect_s3_class(ans, "greta_mcmc_list")

  # model_info attribute should have raw draws and the model
  model_info <- attr(ans, "model_info")
  expect_true(inherits(model_info, "list"))
  expect_s3_class(model_info$raw_draws, "mcmc.list")
  expect_true(inherits(model_info$model, "greta_model"))
})

test_that("samples has object names", {
  skip_if_not(check_tf_version())
  skip_on_cran()

  a <- normal(0, 1)
  b <- normal(a, 1, dim = 3)
  m <- model(a, b)

  # mcmc should give the right names
  draws <- mcmc(m, warmup = 2, n_samples = 10, verbose = FALSE)
  expect_snapshot(rownames(summary(draws)$statistics))

  # so should calculate
  c <- b^2
  c_draws <- calculate(c, values = draws)
  expect_snapshot(rownames(summary(c_draws)$statistics))
})


test_that("model errors nicely", {
  skip_if_not(check_tf_version())
  skip_on_cran()

  # model should give a nice error if passed something other than a greta array
  a <- 1
  b <- normal(0, a)
  expect_snapshot_error(
    model(a, b)
  )
})

test_that("mcmc supports rwmh sampler with normal proposals", {
  skip_if_not(check_tf_version())
  skip_on_cran()

  x <- normal(0, 1)
  m <- model(x)
  expect_ok(draws <- mcmc(m,
    sampler = rwmh("normal"),
    n_samples = 100, warmup = 100,
    verbose = FALSE
  ))
})

test_that("mcmc supports rwmh sampler with uniform proposals", {
  skip_if_not(check_tf_version())
  skip_on_cran()

  set.seed(5)
  x <- uniform(0, 1)
  m <- model(x)
  expect_ok(draws <- mcmc(m,
    sampler = rwmh("uniform"),
    n_samples = 100, warmup = 100,
    verbose = FALSE
  ))
})

test_that("mcmc supports slice sampler with single precision models", {
  skip_if_not(check_tf_version())
  skip_on_cran()

  set.seed(5)
  x <- uniform(0, 1)
  m <- model(x, precision = "single")
  expect_ok(draws <- mcmc(m,
    sampler = slice(),
    n_samples = 100, warmup = 100,
    verbose = FALSE
  ))
})

test_that("mcmc doesn't support slice sampler with double precision models", {
  skip_if_not(check_tf_version())
  skip_on_cran()

  set.seed(5)
  x <- uniform(0, 1)
  m <- model(x, precision = "double")
  expect_snapshot_error(
    draws <- mcmc(m,
                  sampler = slice(),
                  n_samples = 100, warmup = 100,
                  verbose = FALSE
    )
  )
})

test_that("numerical issues are handled in mcmc", {
  skip_if_not(check_tf_version())
  skip_on_cran()

  # this should have a cholesky decomposition problem at some point
  alpha <- normal(0, 1)
  x <- matrix(rnorm(6), 3, 2)
  y <- t(rnorm(3))
  z <- alpha * x
  sigma <- z %*% t(z)
  distribution(y) <- multivariate_normal(zeros(1, 3), sigma)
  m <- model(alpha)

  # running with bursts should error informatively
  expect_snapshot_error(
    draws <- mcmc(m, verbose = FALSE)
  )

  # setting one_by_one = TRUE should handle those errors as bad samples
  expect_ok(draws <- mcmc(m,
    warmup = 100, n_samples = 10,
    one_by_one = TRUE,
    verbose = FALSE
  ))
})

# this is the test that says: 'Loaded Tensorflow version 1.14.0'
test_that("mcmc works in parallel", {
  skip_if_not(check_tf_version())
  skip_on_cran()

  m <- model(normal(0, 1))

  op <- future::plan()
  # put the future plan back as we found it
  withr::defer(future::plan(op))
  future::plan(future::multisession)

  # one chain
  expect_ok(draws <- mcmc(m,
    warmup = 10, n_samples = 10,
    chains = 1,
    verbose = FALSE
  ))

  expect_true(inherits(draws, "greta_mcmc_list"))
  expect_true(coda::niter(draws) == 10)
  rm(draws)

  # multiple chains
  expect_ok(draws <- mcmc(m,
    warmup = 10, n_samples = 10,
    chains = 2,
    verbose = FALSE
  ))

  expect_true(inherits(draws, "greta_mcmc_list"))
  expect_true(coda::niter(draws) == 10)

})

test_that("mcmc errors for invalid parallel plans", {
  skip_if_not(check_tf_version())
  skip_on_ci()
  skip_on_cran()

  m <- model(normal(0, 1))

  op <- future::plan()

  # silence future's warning about multicore support
  # put the future plan back as we found it
  withr::local_envvar("R_FUTURE_SUPPORTSMULTICORE_UNSTABLE" = "quiet")
  # reset warning setting
  withr::defer(future::plan(op))

  future::plan(future::multicore)
  expect_snapshot_error(
    mcmc(m, verbose = FALSE)
  )

  cl <- parallel::makeForkCluster(2L)
  future::plan(future::cluster, workers = cl)
  expect_snapshot_error(
    mcmc(m, verbose = FALSE)
  )

})

test_that("parallel reporting works", {
  skip_if_not(check_tf_version())
  skip_on_cran()

  m <- model(normal(0, 1))

  op <- future::plan()
  # put the future plan back as we found it
  withr::defer(future::plan(op))
  future::plan(future::multisession)

  # should report each sampler's progress with a fraction
  out <- get_output(. <- mcmc(m, warmup = 50, n_samples = 50, chains = 2))
  expect_match(out, "2 samplers in parallel")
  expect_match(out, "50/50")

})

test_that("initials works", {
  skip_if_not(check_tf_version())
  skip_on_cran()

  # errors on bad objects
  expect_snapshot_error(
    initials(a = FALSE)
  )

  expect_snapshot_error(
    initials(FALSE)
  )

  # prints nicely
  expect_snapshot(
    initials(a = 3)
  )

})

test_that("prep_initials errors informatively", {
  skip_if_not(check_tf_version())
  skip_on_cran()

  a <- normal(0, 1)
  b <- uniform(0, 1)
  d <- lognormal(0, 1)
  e <- variable(upper = -1)
  f <- ones(1)
  z <- a * b * d * e * f
  m <- model(z)

  # bad objects:
  expect_snapshot_error(
    mcmc(m, initial_values = FALSE, verbose = FALSE)
  )

  expect_snapshot_error(
    mcmc(m, initial_values = list(FALSE), verbose = FALSE)
  )

  # an unrelated greta array
  g <- normal(0, 1)
  expect_snapshot_error(
    mcmc(m, chains = 1, initial_values = initials(g = 1), verbose = FALSE)
  )

  # non-variable greta arrays
  expect_snapshot_error(
    mcmc(m, chains = 1, initial_values = initials(f = 1), verbose = FALSE)
  )

  expect_snapshot_error(
    mcmc(m, chains = 1, initial_values = initials(z = 1), verbose = FALSE)
  )

  # out of bounds errors
  expect_snapshot_error(
    mcmc(m, chains = 1, initial_values = initials(b = -1), verbose = FALSE)
  )

  expect_snapshot_error(
    mcmc(m, chains = 1, initial_values = initials(d = -1), verbose = FALSE)
  )

  expect_snapshot_error(
    mcmc(m, chains = 1, initial_values = initials(e = 2), verbose = FALSE)
  )
})

test_that("samplers print informatively", {
  skip_if_not(check_tf_version())
  skip_on_cran()

  expect_snapshot(
    hmc()
  )
  expect_snapshot(
    rwmh()
  )
  expect_snapshot(
    slice()
  )
  expect_snapshot(
    hmc(Lmin = 1)
  )

  # # check print sees changed parameters
  # out <- capture_output(hmc(Lmin = 1), TRUE)
  # expect_match(out, "Lmin = 1")
})

test_that("pb_update > thin to avoid bursts with no saved iterations", {
  skip_if_not(check_tf_version())
  skip_on_cran()

  set.seed(5)
  x <- uniform(0, 1)
  m <- model(x)
  expect_ok(draws <- mcmc(m,
    n_samples = 100, warmup = 100,
    thin = 3, pb_update = 2, verbose = FALSE
  ))
  expect_identical(thin(draws), 3)
})
