test_that("check_tf_version fails when M1 mac detected", {
  withr::local_envvar("GRETA_M1_MESSAGE_TESTING" = "on")
  expect_snapshot_error(
    check_tf_version("error")
  )
})
