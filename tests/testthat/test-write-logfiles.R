test_that("the logfile goes where GRETA_INSTALLATION_LOG says", {
  logfile <- withr::local_tempfile(fileext = ".html")
  withr::local_envvar(GRETA_INSTALLATION_LOG = logfile)
  local_install_stash()

  expect_identical(greta_install_logfile(), logfile)
  suppressMessages(write_greta_install_log())
  expect_true(file.exists(logfile))
})

test_that("the logfile escapes install output and keeps it out of summaries", {
  logfile <- withr::local_tempfile(fileext = ".html")
  local_install_stash(
    conda_install_notes = "Requirement tensorflow<2.16 not met",
    conda_install_error = "ERROR: Could not find an activated virtualenv (required)."
  )
  suppressMessages(write_greta_install_log(logfile))
  html <- paste(readLines(logfile), collapse = "\n")

  expect_match(html, "tensorflow&lt;2.16", fixed = TRUE)
  summaries <- regmatches(html, gregexpr("<summary>.*?</summary>", html))[[1]]
  expect_false(any(grepl("<pre>", summaries, fixed = TRUE)))
})
