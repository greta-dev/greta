test_that("check_tf_version fails when M1 mac detected", {
  skip_if_not(check_tf_version())
  withr::local_envvar("GRETA_M1_MESSAGE_TESTING" = "on")
  expect_snapshot(
    x = check_tf_version("error"),
    cnd_class = FALSE
  )
})
