context("inference methods")

test_that("opt converges with TF optimisers", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  x <- rnorm(5, 2, 0.1)
  z <- variable(dim = 5)
  distribution(x) <- normal(z, 0.1)

  m <- model(z)

  # loop through optimisers that might be expected to work
  optimisers <- list(gradient_descent,
                     adadelta,
                     adagrad,
                     adagrad_da,
                     momentum,
                     adam,
                     ftrl,
                     proximal_gradient_descent,
                     proximal_adagrad,
                     rms_prop)

  for (optmr in optimisers) {

    (o <- opt(m,
              optimiser = optmr(),
              max_iterations = 200))

    # should have converged in fewer than 200 iterations and be close to truth
    expect_equal(o$convergence, 0)
    expect_lte(o$iterations, 200)
    expect_true(all(abs(x - o$par$z) < 1e-2))

  }

})

test_that("opt converges with SciPy optimisers", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  x <- rnorm(3, 2, 0.1)
  z <- variable(dim = 3)
  distribution(x) <- normal(z, 0.1)

  m <- model(z)

  # loop through optimisers that might be expected to work
  optimisers <- list(nelder_mead,
                     powell,
                     cg,
                     bfgs,
                     newton_cg,
                     l_bfgs_b,
                     tnc,
                     cobyla,
                     slsqp)

  for (optmr in optimisers) {

    (o <- opt(m,
              optimiser = optmr(),
              max_iterations = 500))

    # should have converged in fewer than 500 iterations and be close to truth

    # can't tell that from output of cobyla
    if (!identical(optmr(), cobyla())) {
      expect_equal(o$convergence, 0)
      expect_lte(o$iterations, 500)
    }

    expect_true(all(abs(x - o$par$z) < 1e-2))

  }

})

test_that("opt accepts initial values", {

  skip_if_not(check_tf_version())
  source("helpers.R")

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
  source("helpers.R")

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

test_that("bad mcmc proposals are rejected", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  # set up for numerical rejection of initial location
  x <- rnorm(10000, 1e6, 1)
  z <- normal(-1e6, 1e-6)
  distribution(x) <- normal(z, 1e6)
  m <- model(z, precision = "single")

  # catch badness in the progress bar
  with_mock(
    `greta:::create_progress_bar` = mock_create_progress_bar,
    out <- get_output(mcmc(m, n_samples = 10, warmup = 0, pb_update = 10)),
    expect_match(out, "100% bad")
  )

  # bad initial values
  expect_error(mcmc(m, chains = 1, n_samples = 1, warmup = 0,
                    initial_values = initials(z = 1e20)),
               "could not be evaluated at these initial values")

  # really bad proposals
  x <- rnorm(100000, 1e12, 1)
  z <- normal(-1e12, 1e-12)
  distribution(x) <- normal(z, 1e-12)
  m <- model(z, precision = "single")
  expect_error(mcmc(m, chains = 1, n_samples = 1, warmup = 0),
               "Could not find reasonable starting values after 20 attempts")

  # proposals that are fine, but rejected anyway
  z <- normal(0, 1)
  m <- model(z, precision = "single")
  expect_ok(mcmc(m,
                 hmc(epsilon = 100,
                     Lmin = 1,
                     Lmax = 1),
                 chains = 1,
                 n_samples = 5,
                 warmup = 0,
                 verbose = FALSE))

})

test_that("mcmc works with verbosity and warmup", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  x <- rnorm(10)
  z <- normal(0, 1)
  distribution(x) <- normal(z, 1)
  m <- model(z)
  quietly(expect_ok(mcmc(m, n_samples = 50, warmup = 50, verbose = TRUE)))

})

test_that("mcmc works with multiple chains", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  x <- rnorm(10)
  z <- normal(0, 1)
  distribution(x) <- normal(z, 1)
  m <- model(z)

  # multiple chains, automatic initial values
  quietly(expect_ok(mcmc(m, warmup = 10, n_samples = 10, chains = 2)))

  # multiple chains, user-specified initial values
  inits <- list(initials(z = 1), initials(z = 2))
  quietly(expect_ok(mcmc(m, warmup = 10, n_samples = 10, chains = 2,
                         initial_values = inits)))

})

test_that("mcmc handles initial values nicely", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  x <- rnorm(10)
  z <- normal(0, 1)
  distribution(x) <- normal(z, 1)
  m <- model(z)

  # too many sets of initial values
  inits <- replicate(3, initials(z = rnorm(1)), simplify = FALSE)
  expect_error(mcmc(m, warmup = 10, n_samples = 10, verbose = FALSE,
                    chains = 2, initial_values = inits),
               "sets of initial values were provided, but there are")

  # initial values have the wrong length
  inits <- replicate(2, initials(z = rnorm(2)), simplify = FALSE)
  expect_error(mcmc(m, warmup = 10, n_samples = 10, verbose = FALSE,
                    chains = 2, initial_values = inits),
               "initial values provided have different dimensions")

  inits <- initials(z = rnorm(1))
  quietly(expect_message(mcmc(m, warmup = 10, n_samples = 10,
                              chains = 2, initial_values = inits,
                              verbose = FALSE),
                         "only one set of initial values was provided"))

})

test_that("progress bar gives a range of messages", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  # 10/1010 should be <1%
  with_mock(
    `greta:::create_progress_bar` = mock_create_progress_bar,
    `greta:::mcmc` = mock_mcmc,
    out <- get_output(mcmc(1010)),
    expect_match(out, "<1% bad")
  )

  # 10/500 should be 2%
  with_mock(
    `greta:::create_progress_bar` = mock_create_progress_bar,
    `greta:::mcmc` = mock_mcmc,
    out <- get_output(mcmc(500)),
    expect_match(out, "2% bad")
  )

  # 10/10 should be 100%
  with_mock(
    `greta:::create_progress_bar` = mock_create_progress_bar,
    `greta:::mcmc` = mock_mcmc,
    out <- get_output(mcmc(10)),
    expect_match(out, "100% bad")
  )

})

test_that("extra_samples works", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  # set up model
  a <- normal(0, 1)
  m <- model(a)

  draws <- mcmc(m, warmup = 10, n_samples = 10, verbose = FALSE)

  more_draws <- extra_samples(draws, 20, verbose = FALSE)

  expect_true(inherits(more_draws, "mcmc.list"))
  expect_true(coda::niter(more_draws) == 30)
  expect_true(coda::nchain(more_draws) == 4)

})

test_that("stashed_samples works", {

  skip_if_not(check_tf_version())
  source("helpers.R")

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
    model = m
  ), simplify = FALSE)
  assign("samplers", samplers_stash, envir = stash)

  # should convert to an mcmc.list
  ans <- stashed_samples()
  expect_s3_class(ans, "mcmc.list")

  # model_info attribute should have raw draws and the model
  model_info <- attr(ans, "model_info")
  expect_true(inherits(model_info, "environment"))
  expect_s3_class(model_info$raw_draws, "mcmc.list")
  expect_true(inherits(model_info$model, "greta_model"))

})

test_that("model errors nicely", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  # model should give a nice error if passed something other than a greta array
  a <- 1
  b <- normal(0, a)
  expect_error(model(a, b),
               "^The following object")
})

test_that("mcmc supports rwmh sampler with normal proposals", {

  skip_if_not(check_tf_version())
  x <- normal(0, 1)
  m <- model(x)
  expect_ok(draws <- mcmc(m, sampler = rwmh("normal"),
                          n_samples = 100, warmup = 100))

})

test_that("mcmc supports rwmh sampler with uniform proposals", {

  skip_if_not(check_tf_version())
  set.seed(5)
  x <- uniform(0, 1)
  m <- model(x)
  expect_ok(draws <- mcmc(m, sampler = rwmh("uniform"),
                          n_samples = 100, warmup = 100))

})

test_that("mcmc supports slice sampler with single precision models", {

  skip_if_not(check_tf_version())
  set.seed(5)
  x <- uniform(0, 1)
  m <- model(x, precision = "single")
  expect_ok(draws <- mcmc(m, sampler = slice(),
                          n_samples = 100, warmup = 100))

})

test_that("mcmc doesn't support slice sampler with double precision models", {

  skip_if_not(check_tf_version())
  set.seed(5)
  x <- uniform(0, 1)
  m <- model(x, precision = "double")
  expect_error(draws <- mcmc(m, sampler = slice(),
                             n_samples = 100, warmup = 100),
               "models defined with single precision")

})

test_that("numerical issues are handled in mcmc", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  # this should have a cholesky decomposition problem at some point
  k <- 2
  Sigma <- lkj_correlation(1, k)
  x <- wishart(k + 1, Sigma)
  m <- model(x, precision = "single")

  # running with bursts should error informatively
  expect_error(draws <- mcmc(m, verbose = FALSE),
               "TensorFlow hit a numerical problem")

  # setting one_by_one = TRUE should handle those errors as bad samples
  expect_ok(draws <- mcmc(m, warmup = 100, n_samples = 10,
                          one_by_one = TRUE,
                          verbose = FALSE))

})

test_that("mcmc works in parallel", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  m <- model(normal(0, 1))

  library(future)
  op <- plan()
  plan(multisession)

  # one chain
  expect_ok(draws <- mcmc(m, warmup = 10, n_samples = 10,
                          chains = 1,
                          verbose = FALSE))

  expect_true(inherits(draws, "mcmc.list"))
  expect_true(coda::niter(draws) == 10)
  rm(draws)

  # multiple chains
  expect_ok(draws <- mcmc(m, warmup = 10, n_samples = 10,
                          chains = 2,
                          verbose = FALSE))

  expect_true(inherits(draws, "mcmc.list"))
  expect_true(coda::niter(draws) == 10)

  # put the future plan back as we found it
  plan(op)

})

test_that("mcmc errors for invalid parallel plans", {

  skip_if_not(check_tf_version())
  skip_on_travis()
  source("helpers.R")

  m <- model(normal(0, 1))

  library(future)
  op <- plan()

  # handle handle forks, so only accept multisession, or multi session clusters
  plan(multiprocess)
  expect_error(draws <- mcmc(m),
               "parallel mcmc samplers cannot be run with")

  plan(multicore)
  expect_error(draws <- mcmc(m),
               "parallel mcmc samplers cannot be run with")

  cl <- parallel::makeForkCluster(2L)
  plan(cluster, workers = cl)
  expect_error(draws <- mcmc(m),
               "parallel mcmc samplers cannot be run with")

  # put the future plan back as we found it
  plan(op)

})

test_that("parallel reporting works", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  m <- model(normal(0, 1))

  library(future)
  op <- plan()
  plan(multisession)

  # should report each sampler's progress with a percentage
  out <- get_output(. <- mcmc(m, warmup = 50, n_samples = 50, chains = 2))
  expect_match(out, "2 samplers in parallel")
  expect_match(out, "100%")

  # put the future plan back as we found it
  plan(op)

})

test_that("initials works", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  # errors on bad objects
  expect_error(initials(a = FALSE),
               "must be numeric")

  expect_error(initials(FALSE),
               "must be named")

  # prints nicely
  init <- initials(a = 3)
  out <- capture.output(print(init))
  out <- paste(out, collapse = "\n")
  expect_match(out, "a greta initials object")
  expect_match(out, "\\$a")

})

test_that("prep_initials errors informatively", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  a <- normal(0, 1)
  b <- uniform(0, 1)
  d <- lognormal(0, 1)
  e <- variable(upper = -1)
  f <- ones(1)
  z <- a * b * d * e * f
  m <- model(z)

  # bad objects:
  expect_error(mcmc(m, initial_values = FALSE),
               "must be an initials object created with initials()")

  expect_error(mcmc(m, initial_values = list(FALSE)),
               "must be an initials object created with initials()")

  # an unrelated greta array
  g <- normal(0, 1)
  expect_error(mcmc(m, chains = 1, initial_values = initials(g = 1)),
               "not associated with the model: g")

  # non-variable greta arrays
  expect_error(mcmc(m, chains = 1, initial_values = initials(f = 1)),
               "can only be set for variable greta arrays")
  expect_error(mcmc(m, chains = 1, initial_values = initials(z = 1)),
               "can only be set for variable greta arrays")

  # out of bounds errors
  expect_error(mcmc(m, chains = 1, initial_values = initials(b = -1)),
               "outside the range of values")
  expect_error(mcmc(m, chains = 1, initial_values = initials(d = -1)),
               "outside the range of values")
  expect_error(mcmc(m, chains = 1, initial_values = initials(e = 2)),
               "outside the range of values")

})

test_that("samplers print informatively", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  out <- capture_output(hmc(), TRUE)
  expect_match(out, "hmc sampler")

  out <- capture_output(rwmh(), TRUE)
  expect_match(out, "rwmh sampler")

  out <- capture_output(slice(), TRUE)
  expect_match(out, "slice sampler")

  # check print sees changed parameters
  out <- capture_output(hmc(Lmin = 1), TRUE)
  expect_match(out, "Lmin = 1")

})
