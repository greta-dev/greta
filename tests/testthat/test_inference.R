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

test_that('rejected mcmc proposals', {

  skip_if_not(check_tf_version())
  source('helpers.R')

  # set up for numerical rejection of initial location
  x <- rnorm(10000, 1e6, 1)
  z = normal(-1e6, 1e-6)
  distribution(x) = normal(z, 1e6)
  m <- model(z)

  with_mock(
    `greta:::create_progress_bar` = mock_create_progress_bar,
    m <- model(z),
    out <- capture_output(mcmc(m, n_samples = 10, warmup = 0)),
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
               'Could not find reasonable starting values after 10 attempts')

})

test_that('mcmc works with verbosity and warmup', {

  skip_if_not(check_tf_version())
  source('helpers.R')

  x <- rnorm(10)
  z = normal(0, 1)
  distribution(x) = normal(z, 1)
  m <- model(z)
  mcmc(m, n_samples = 50, warmup = 50, verbose = TRUE)

})

test_that('progress bar gives a range of messages', {

  skip_if_not(check_tf_version())
  source('helpers.R')

  # 10/1010 should be <1%
  with_mock(
    `greta:::create_progress_bar` = mock_create_progress_bar,
    `greta:::mcmc` = mock_mcmc,
    out <- capture_output(mcmc(1010)),
    expect_match(out, '<1% bad')
  )

  # 10/500 should be 50%
  with_mock(
    `greta:::create_progress_bar` = mock_create_progress_bar,
    `greta:::mcmc` = mock_mcmc,
    out <- capture_output(mcmc(500)),
    expect_match(out, '2% bad')
  )

  # 10/10 should be 100%
  with_mock(
    `greta:::create_progress_bar` = mock_create_progress_bar,
    `greta:::mcmc` = mock_mcmc,
    out <- capture_output(mcmc(10)),
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
  assign('trace_stash', as.matrix(rnorm(17)), envir = stash)

  # should convert to an mcmc.list
  ans <- stashed_samples()
  expect_s3_class(ans, 'mcmc.list')

})
