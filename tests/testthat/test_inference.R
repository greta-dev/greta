context('inference methods')

test_that('opt converges', {

  skip_if_not(check_tf_version())
  source('helpers.R')

  x <- rnorm(5, 2, 0.1)
  z = variable(dim = 5)
  distribution(x) = normal(z, 0.1)

  m <- model(z)
  o <- opt(m)

  # should have converged
  expect_equal(o$convergence, 0)

  # should be fewer than 100 iterations
  expect_lte(o$iterations, 100)

  # should be close to the truth
  expect_true(all(abs(x - o$par) < 1e-3))

})


test_that('opt accepts initial values', {

  skip_if_not(check_tf_version())
  source('helpers.R')

  x <- rnorm(5, 2, 0.1)
  z = variable(dim = 5)
  distribution(x) = normal(z, 0.1)

  m <- model(z)
  o <- opt(m, initial_values = rnorm(5))

  # should have converged
  expect_equal(o$convergence, 0)

  # should be fewer than 100 iterations
  expect_lte(o$iterations, 100)

  # should be close to the truth
  expect_true(all(abs(x - o$par) < 1e-3))

})

test_that('bad mcmc proposals are rejected', {

  skip_if_not(check_tf_version())
  source('helpers.R')

  # set up for numerical rejection of initial location
  x <- rnorm(10000, 1e6, 1)
  z = normal(-1e6, 1e-6)
  distribution(x) = normal(z, 1e6)
  m <- model(z)

  # catch badness in the progress bar
  with_mock(
    `greta:::create_progress_bar` = mock_create_progress_bar,
    out <- get_output(mcmc(m, n_samples = 10, warmup = 0)),
    expect_match(out, '100% bad')
  )

  # bad initial values
  expect_error(mcmc(m, n_samples = 1, warmup = 0, initial_values = 1e20),
               'could not be evaluated at these initial values')

  # really bad proposals
  x <- rnorm(100000, 1e12, 1)
  z = normal(-1e12, 1e-12)
  distribution(x) = normal(z, 1e-12)
  m <- model(z)
  expect_error(mcmc(m, n_samples = 1, warmup = 0),
               'Could not find reasonable starting values after 20 attempts')

  # proposals that are fine, but rejected anyway (long chain required)
  z = normal(0, 1)
  m <- model(z)
  expect_ok(mcmc(m, n_samples = 5, warmup = 0, verbose = FALSE,
                 control = list(epsilon = 100, Lmin = 1, Lmax = 1)))

})

test_that('mcmc works with verbosity and warmup', {

  skip_if_not(check_tf_version())
  source('helpers.R')

  x <- rnorm(10)
  z = normal(0, 1)
  distribution(x) = normal(z, 1)
  m <- model(z)
  quietly(expect_ok( mcmc(m, n_samples = 50, warmup = 50, verbose = TRUE) ))

})

test_that('mcmc works with multiple chains', {

  skip_if_not(check_tf_version())
  source('helpers.R')

  x <- rnorm(10)
  z = normal(0, 1)
  distribution(x) = normal(z, 1)
  m <- model(z)

  # multiple chains, automatic initial values
  quietly(expect_ok( mcmc(m, warmup = 10, n_samples = 10, chains = 2) ))

  # multiple chains, user-specified initial values
  quietly(expect_ok( mcmc(m, warmup = 10, n_samples = 10, chains = 2, initial_values = list(1, 2)) ))

})

test_that('mcmc handles initial values nicely', {

  skip_if_not(check_tf_version())
  source('helpers.R')

  x <- rnorm(10)
  z = normal(0, 1)
  distribution(x) = normal(z, 1)
  m <- model(z)

  # too many sets of initial values
  expect_error( mcmc(m, warmup = 10, n_samples = 10, verbose = FALSE,
                     chains = 2, initial_values = list(1)),
                "sets of initial values were provided, but there are")

  # initial values have the wrong length
  expect_error( mcmc(m, warmup = 10, n_samples = 10, verbose = FALSE,
                     chains = 2, initial_values = list(1:2, 2:3)),
                "each set of initial values must be a vector of length")

  quietly(expect_message(mcmc(m, warmup = 10, n_samples = 10,
                      chains = 2, initial_values = 1),
                 "only one set of was initial values given, and was used for all chains"))

})

test_that('progress bar gives a range of messages', {

  skip_if_not(check_tf_version())
  source('helpers.R')

  # 10/1010 should be <1%
  with_mock(
    `greta:::create_progress_bar` = mock_create_progress_bar,
    `greta:::mcmc` = mock_mcmc,
    out <- get_output(mcmc(1010)),
    expect_match(out, '<1% bad')
  )

  # 10/500 should be 2%
  with_mock(
    `greta:::create_progress_bar` = mock_create_progress_bar,
    `greta:::mcmc` = mock_mcmc,
    out <- get_output(mcmc(500)),
    expect_match(out, '2% bad')
  )

  # 10/10 should be 100%
  with_mock(
    `greta:::create_progress_bar` = mock_create_progress_bar,
    `greta:::mcmc` = mock_mcmc,
    out <- get_output(mcmc(10)),
    expect_match(out, '100% bad')
  )

})

test_that('stashed_samples works', {

  skip_if_not(check_tf_version())
  source('helpers.R')

  # set up model
  a <- normal(0, 1)
  m <- model(a)

  draws <- mcmc(m, warmup = 10, n_samples = 10, verbose = FALSE)

  # with a completed sample, this should be NULL
  ans <- stashed_samples()
  expect_null(ans)

  # mock up a stash
  stash <- greta:::greta_stash
  trace_stash <- list(trace = as.matrix(rnorm(17)),
                      raw = as.matrix(rnorm(17)))
  assign('trace_stash', trace_stash, envir = stash)

  # should convert to an mcmc.list
  ans <- stashed_samples()
  expect_s3_class(ans, 'mcmc.list')

  # model_info attribute should have raw draws and the model
  model_info <- attr(ans, "model_info")
  expect_true(inherits(model_info, "environment"))
  expect_s3_class(model_info$raw_draws, 'mcmc.list')
  expect_true(inherits(model_info$model, "greta_model"))

})


test_that('model errors nicely', {

  skip_if_not(check_tf_version())
  source('helpers.R')

  # model should give a nice error if passed something other than a greta array
  a <- 1
  b <- normal(0, a)
  expect_error(model(a, b),
               "^The following object")
})
