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
  expect_snapshot(error = TRUE, check_future_plan())

  # skip on windows
  if (.Platform$OS.type != "windows") {
    cl <- parallel::makeCluster(2L, type = "FORK")
    future::plan(future::cluster, workers = cl)
    expect_snapshot(error = TRUE, check_future_plan())
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
  expect_match(
    get_output(. <- mcmc(m, warmup = 50, n_samples = 50, chains = 2)),
    "2 samplers in parallel"
  )
  expect_match(
    get_output(. <- mcmc(m, warmup = 50, n_samples = 50, chains = 2)),
    "50/50"
  )
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
  expect_snapshot(error = TRUE, mcmc(m, verbose = FALSE))

  cl <- parallel::makeForkCluster(2L)
  future::plan(future::cluster, workers = cl)
  expect_snapshot(error = TRUE, mcmc(m, verbose = FALSE))
})

# this is the test that says: 'Loaded Tensorflow version 1.14.0'
test_that("mcmc works in parallel", {
  skip_if_not(check_tf_version())

  op <- future::plan()
  # put the future plan back as we found it
  withr::defer(future::plan(op))
  # two workers, so every run below reuses ones that have already loaded
  # TensorFlow
  future::plan(future::multisession, workers = 2)

  # one chain. The target is so wide that rwmh accepts every proposal, so each
  # step is the proposal noise alone, and extra_samples() in a worker must draw
  # new noise rather than replaying the first run's
  x <- normal(0, 1e6)
  wide <- model(x)
  expect_ok(
    draws <- mcmc(
      wide,
      sampler = rwmh(),
      warmup = 0,
      n_samples = 5,
      chains = 1,
      initial_values = initials(x = 0),
      verbose = FALSE
    )
  )
  expect_true(inherits(draws, "greta_mcmc_list"))
  expect_true(coda::niter(draws) == 5)

  draws <- extra_samples(draws, 5, verbose = FALSE)
  steps <- diff(as.vector(draws[[1]]))
  expect_false(isTRUE(all.equal(steps[1:4], steps[6:9])))

  # multiple chains, seeded distinctly and reproducibly
  m <- model(normal(0, 1))
  draw <- function() {
    mcmc(m, warmup = 10, n_samples = 10, chains = 2, verbose = FALSE)
  }
  expect_ok(one <- withr::with_seed(2026, draw()))
  expect_true(inherits(one, "greta_mcmc_list"))
  expect_true(coda::niter(one) == 10)

  perturb_tf_seed()
  two <- withr::with_seed(2026, draw())
  expect_identical(lapply(one, as.vector), lapply(two, as.vector))
  expect_false(identical(as.vector(one[[1]]), as.vector(one[[2]])))
})
