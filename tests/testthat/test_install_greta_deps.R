test_that("install_greta_deps errors appropriately", {
  skip_if_not(check_tf_version())
  skip_on_ci()
  # a failed install now writes its logfile, so keep it out of the user's
  # directory, and out of the snapshot
  logfile <- withr::local_tempfile(fileext = ".html")
  withr::local_envvar(GRETA_INSTALLATION_LOG = logfile)
  expect_snapshot(
    error = TRUE,
    install_greta_deps(timeout = 0.001),
    transform = \(lines) gsub(logfile, "<logfile>", lines, fixed = TRUE)
  )
})

# These exercise the installation error messages directly (no Python needed), so
# the guidance users see when install_greta_deps() times out or fails is
# captured even where the real-install test above is skipped.
test_that("install timeout message is captured", {
  expect_snapshot(cat(timeout_install_msg(timeout = 5, py_error = "")))
})

test_that("install timeout message includes an underlying python error", {
  expect_snapshot(
    cat(timeout_install_msg(timeout = 5, py_error = "could not resolve env"))
  )
})

test_that("install failure message is captured", {
  expect_snapshot(
    cat(other_install_fail_msg("could not resolve env", env_exists = FALSE))
  )
})

test_that("install failure suggests reinstalling when the env already exists", {
  expect_match(
    other_install_fail_msg("could not resolve env", env_exists = TRUE),
    "reinstall_greta_deps"
  )
  expect_no_match(
    other_install_fail_msg("could not resolve env", env_exists = FALSE),
    "reinstall_greta_deps"
  )
})

test_that("install failure message shows error lines and advice", {
  withr::local_envvar(
    GRETA_INSTALLATION_LOG = "greta-installation-logfile.html"
  )
  expect_snapshot(
    cat(other_install_fail_msg(
      "ERROR: Could not find an activated virtualenv (required).",
      output_notes = "Collecting tensorflow",
      env_exists = FALSE,
      log_written = TRUE
    ))
  )
})

test_that("install failure messages point to the logfile only if it was written", {
  withr::local_envvar(
    GRETA_INSTALLATION_LOG = "greta-installation-logfile.html"
  )
  failed <- function(log_written) {
    other_install_fail_msg(
      "boom",
      env_exists = FALSE,
      log_written = log_written
    )
  }
  timed_out <- function(log_written) {
    timeout_install_msg(log_written = log_written)
  }
  expect_match(failed(TRUE), "greta-installation-logfile.html", fixed = TRUE)
  expect_no_match(failed(FALSE), "logfile")
  expect_match(timed_out(TRUE), "greta-installation-logfile.html", fixed = TRUE)
  expect_no_match(timed_out(FALSE), "logfile")
})

test_that("an install failure with no error lines shows the end of stderr", {
  stderr_text <- paste("line", 1:30, collapse = "\n")
  msg <- other_install_fail_msg(stderr_text, env_exists = FALSE)
  expect_match(msg, "line 30", fixed = TRUE)
  expect_no_match(msg, "line 10\\b")
})

# test_that("reinstall_greta_deps errors appropriately", {
#   skip_if_not(check_tf_version())
#   expect_snapshot(error = TRUE,
#     reinstall_greta_deps(timeout = 0.001)
#   )
# })
