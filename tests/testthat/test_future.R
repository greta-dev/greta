test_that("check_future_plan() works", {
  library(future)
  op <- plan()
  plan(multisession)

  # one chain
  expect_error(check_future_plan(), NA)
  # put the future plan back as we found it
  plan(op)
})

test_that("mcmc errors for invalid parallel plans", {
  library(future)
  op <- plan()

  # silence future's warning about multicore support
  old_option <- Sys.getenv("R_FUTURE_SUPPORTSMULTICORE_UNSTABLE")
  Sys.setenv("R_FUTURE_SUPPORTSMULTICORE_UNSTABLE" = "quiet")

  # handle handle forks, so only accept multisession, or multi session clusters
  suppressWarnings(plan(multiprocess))
  expect_snapshot(
    error = TRUE,
    check_future_plan()
  )

  plan(multicore)
  expect_snapshot(
    error = TRUE,
    check_future_plan()
  )

  cl <- parallel::makeCluster(2L, type = "FORK")
  plan(cluster, workers = cl)
  expect_snapshot(
    error = TRUE,
    check_future_plan()
  )

  # put the future plan back as we found it
  plan(op)

  # reset warning setting
  Sys.setenv("R_FUTURE_SUPPORTSMULTICORE_UNSTABLE" = old_option)
})
