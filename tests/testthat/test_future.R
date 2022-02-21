test_that("check_future_plan() works", {
  library(future)
  op <- plan()
  # put the future plan back as we found it
  withr::defer(plan(op))
  plan(multisession)

  # one chain
  expect_error(check_future_plan(), NA)

})

test_that("mcmc errors for invalid parallel plans", {
  library(future)
  op <- plan()
  # put the future plan back as we found it
  withr::defer(plan(op))

  # temporarily silence future's warning about multicore support
  withr::local_envvar("R_FUTURE_SUPPORTSMULTICORE_UNSTABLE" = "quiet")

  # handle handle forks, so only accept multisession, or multi session clusters
  suppressWarnings(plan(multiprocess))
  expect_snapshot_error(
    check_future_plan()
  )

  plan(multicore)
  expect_snapshot_error(
    check_future_plan()
  )

  # skip on windows
  if (.Platform$OS.type != "windows"){
    cl <- parallel::makeCluster(2L, type = "FORK")
    plan(cluster, workers = cl)
    expect_snapshot_error(
      check_future_plan()
    )
  }

})
