test_that("install_error_lines() keeps only the lines reporting an error", {
  reported <- c(
    "ERROR: No matching distribution found for tensorflow",
    "Error in (function () : Error installing package(s)",
    "PackagesNotFoundError: The following packages are not available",
    "ModuleNotFoundError: No module named 'tensorflow'",
    "  error: No solution found when resolving dependencies"
  )
  benign <- c(
    "Collecting numpy",
    "Checked 42 files, 0 errors",
    "Installed error-free, with no error"
  )
  output <- paste(c(benign[1], reported, benign[-1]), collapse = "\n")
  expect_identical(install_error_lines(output), reported)
})

test_that("an install step that errors is a failure, though it wrote output", {
  skip_on_cran()
  withr::local_envvar(GRETA_INSTALLATION_LOG = withr::local_tempfile())
  local_install_stash()
  stdout_file <- withr::local_tempfile()
  stderr_file <- withr::local_tempfile()
  process <- callr::r_process_options(
    func = function() {
      cat("Collecting tensorflow\n")
      message("ERROR: Could not find an activated virtualenv (required).")
      message("ERROR: Invalid requirement: 'tensorflow {cuda}'")
      stop("Error installing package(s)")
    },
    stdout = stdout_file,
    stderr = stderr_file
  )

  error <- expect_error(
    suppressMessages(
      new_install_process(
        callr_process = process,
        timeout = 1,
        stdout_file = stdout_file,
        stderr_file = stderr_file,
        stash_as = "conda_install"
      )
    )
  )
  # braces in install output are shown, not read as cli markup
  expect_match(conditionMessage(error), "tensorflow {cuda}", fixed = TRUE)
  expect_match(greta_stash$conda_install_error, "activated virtualenv")
})
