test_that("check_future_plan() works when only one core available", {
  # temporarily set the envvar to have only 1 core available
  withr::local_envvar("R_PARALLELLY_AVAILABLE_CORES" = "1")
  op <- future::plan()
  # put the future plan back as we found it
  withr::defer(future::plan(op))
  future::plan(future::multisession)

  # one chain
  expect_error(check_future_plan(), NA)

})

test_that("check_future_plan() works", {
  op <- future::plan()
  # put the future plan back as we found it
  withr::defer(future::plan(op))
  future::plan(future::multisession)

  # one chain
  expect_error(check_future_plan(), NA)

})

test_that("mcmc errors for invalid parallel plans", {
  op <- future::plan()
  # put the future plan back as we found it
  withr::defer(future::plan(op))

  # temporarily silence future's warning about multicore support
  withr::local_envvar("R_FUTURE_SUPPORTSMULTICORE_UNSTABLE" = "quiet")

  # handle handle forks, so only accept multisession, or multi session clusters
  expect_snapshot_error(
    check_future_plan()
  )

  future::plan(future::multicore)
  expect_snapshot_error(
    check_future_plan()
  )

  # skip on windows
  if (.Platform$OS.type != "windows"){
    cl <- parallel::makeCluster(2L, type = "FORK")
    future::plan(future::cluster, workers = cl)
    expect_snapshot_error(
      check_future_plan()
    )
  }

})
