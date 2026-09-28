test_that("the logfile goes where GRETA_INSTALLATION_LOG says", {
  logfile <- withr::local_tempfile(fileext = ".html")
  withr::local_envvar(GRETA_INSTALLATION_LOG = logfile)

  expect_identical(greta_install_logfile(), logfile)
  suppressMessages(write_greta_install_log())
  expect_true(file.exists(logfile))
})
