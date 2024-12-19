test_that("check_future_plan() works when only one core available", {
  # temporarily set the envvar to have only 1 core available
  withr::local_envvar("R_PARALLELLY_AVAILABLE_CORES" = "1")
  op <- future::plan()
  # put the future plan back as we found it
  withr::defer(future::plan(op))
  future::plan(future::multisession)

  # one chain
  expect_snapshot_output(
    check_future_plan()
    )

})

test_that("check_future_plan() works", {
  op <- future::plan()
  # put the future plan back as we found it
  withr::defer(future::plan(op))
  future::plan(future::multisession)

  # one chain
  expect_snapshot_output(
    check_future_plan()
    )

})

test_that("mcmc errors for invalid parallel plans", {
  op <- future::plan()
  # put the future plan back as we found it
  withr::defer(future::plan(op))

  # temporarily silence future's warning about multicore support
  withr::local_envvar("R_FUTURE_SUPPORTSMULTICORE_UNSTABLE" = "quiet")

  # handle forks, so only accept multisession, or multi session clusters
  future::plan(future::multisession)
  expect_snapshot_output(
    check_future_plan()
  )

  future::plan(future::multicore)
  expect_snapshot(error = TRUE,
    check_future_plan()
  )

  # skip on windows
  if (.Platform$OS.type != "windows"){
    cl <- parallel::makeCluster(2L, type = "FORK")
    future::plan(future::cluster, workers = cl)
    expect_snapshot(error = TRUE,
      check_future_plan()
    )
  }

})

test_that("parallel reporting works", {
  skip_if_not(check_tf_version())

  m <- model(normal(0, 1))

  op <- future::plan()
  # put the future plan back as we found it
  withr::defer(future::plan(op))
  future::plan(future::multisession)

  # should report each sampler's progress with a fraction
  #out <- get_output(. <- mcmc(m, warmup = 50, n_samples = 50, chains = 2))
  expect_match(get_output(. <- mcmc(m, warmup = 50, n_samples = 50, chains = 2)), "2 samplers in parallel")
  expect_match(get_output(. <- mcmc(m, warmup = 50, n_samples = 50, chains = 2)), "50/50")

})


test_that("mcmc errors for invalid parallel plans", {
  skip_if_not(check_tf_version())
  skip_on_os(os = "windows")

  m <- model(normal(0, 1))

  op <- future::plan()

  # silence future's warning about multicore support
  # put the future plan back as we found it
  withr::local_envvar("R_FUTURE_SUPPORTSMULTICORE_UNSTABLE" = "quiet")
  # reset warning setting
  withr::defer(future::plan(op))

  future::plan(future::multicore)
  expect_snapshot(error = TRUE,
    mcmc(m, verbose = FALSE)
  )

  cl <- parallel::makeForkCluster(2L)
  future::plan(future::cluster, workers = cl)
  expect_snapshot(error = TRUE,
    mcmc(m, verbose = FALSE)
  )

})

# this is the test that says: 'Loaded Tensorflow version 1.14.0'
test_that("mcmc works in parallel", {
  skip_if_not(check_tf_version())

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
