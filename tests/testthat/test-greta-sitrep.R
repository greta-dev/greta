test_that("check_tf_version errors when have_python, _tf, or _tfp is FALSE", {
  local_mocked_bindings(
    py_module_available = function(...) FALSE,
    have_python = function(...) FALSE,
    have_tf = function(...) FALSE,
    have_tfp = function(...) FALSE
  )

  expect_snapshot(
    error = TRUE,
    check_tf_version("error")
  )

  expect_snapshot_warning(
    check_tf_version("warn")
  )

  expect_snapshot(
    check_tf_version("message")
  )
})

test_that("check_tf_version fails when tfp not available", {
  greta_stash$python_has_been_initialised <- FALSE
  local_mocked_bindings(
    have_tfp = function(...) FALSE
  )
  expect_snapshot(
    error = TRUE,
    check_tf_version("error")
  )
})

test_that("greta_sitrep warns when have_python, _tf, or _tfp is FALSE", {
  skip_if_not(check_tf_version())
  skip_on_ci()
  skip_on_cran()
  local_mocked_bindings(
    have_python = function(...) FALSE,
    have_tf = function(...) FALSE,
    have_tfp = function(...) TRUE
  )

  expect_snapshot_warning(
    greta_sitrep()
  )

  local_mocked_bindings(
    have_python = function(...) FALSE,
    have_tf = function(...) FALSE,
    have_tfp = function(...) FALSE
  )

  expect_snapshot_warning(
    greta_sitrep()
  )

  local_mocked_bindings(
    have_python = function(...) TRUE,
    have_tf = function(...) FALSE,
    have_tfp = function(...) TRUE
  )

  expect_snapshot_warning(
    greta_sitrep()
  )

  local_mocked_bindings(
    have_python = function(...) FALSE,
    have_tf = function(...) TRUE,
    have_tfp = function(...) FALSE
  )

  expect_snapshot_warning(
    greta_sitrep()
  )

  local_mocked_bindings(
    have_python = function(...) FALSE,
    have_tf = function(...) FALSE,
    have_tfp = function(...) TRUE
  )

  expect_snapshot_warning(
    greta_sitrep()
  )

  local_mocked_bindings(
    have_python = function(...) TRUE,
    have_tf = function(...) TRUE,
    have_tfp = function(...) FALSE
  )

  expect_snapshot_warning(
    greta_sitrep()
  )

  local_mocked_bindings(
    have_python = function(...) TRUE,
    have_tf = function(...) TRUE,
    have_tfp = function(...) TRUE
  )

  expect_snapshot(
    greta_sitrep()
  )
})

test_that("greta_sitrep warns when different versions of python, tf, tfp", {
  skip_if_not(check_tf_version())
  skip_on_ci()
  skip_on_cran()

  local_mocked_bindings(
    py_version = function(...) "3.6"
  )

  expect_snapshot(
    greta_sitrep()
  )

  local_mocked_bindings(
    version_tf = function(...) "2.0.0"
  )

  expect_snapshot(
    greta_sitrep()
  )

  local_mocked_bindings(
    version_tfp = function(...) "0.9.0"
  )

  expect_snapshot(
    greta_sitrep()
  )
})

test_that("greta_sitrep warns greta conda env not available", {
  skip_if_not(check_tf_version())
  skip_on_ci()
  skip_on_cran()

  local_mocked_bindings(
    have_greta_conda_env = function(...) FALSE
  )

  expect_snapshot(
    greta_sitrep()
  )
})


test_that("greta_sitrep works with quiet, minimal, and detailed options", {
  skip_if_not(check_tf_version())
  skip_on_ci()
  skip_on_cran()

  expect_snapshot(
    greta_sitrep(verbosity = "quiet")
  )
  expect_snapshot(
    greta_sitrep(verbosity = "minimal")
  )
  # test it errors when verbosity is not "quiet", "minimal", and "detailed"
  expect_snapshot(
    error = TRUE,
    greta_sitrep(verbosity = "bananas")
  )

  expect_snapshot(
    greta_sitrep(verbosity = "detailed")
  )
})
