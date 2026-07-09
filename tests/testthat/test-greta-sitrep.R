# the offline-readiness lines depend on machine state (the uv cache and
# UV_OFFLINE), so drop them from sitrep snapshots; report_offline_readiness()
# has its own deterministic tests in test-python-backend.R
redact_offline_readiness <- function(x) {
  x[!grepl("offline-ready|will need internet|installation vignette", x)]
}

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

  local_python_plan()
  expect_snapshot(
    greta_sitrep(),
    transform = redact_offline_readiness
  )
})

test_that("greta_sitrep warns when different versions of python, tf, tfp", {
  skip_if_not(check_tf_version())
  skip_on_ci()
  skip_on_cran()

  local_mocked_bindings(
    py_version = function(...) "3.6"
  )

  local_python_plan()
  expect_snapshot(
    greta_sitrep(),
    transform = redact_offline_readiness
  )

  local_mocked_bindings(
    version_tf = function(...) "2.0.0"
  )

  expect_snapshot(
    greta_sitrep(),
    transform = redact_offline_readiness
  )

  local_mocked_bindings(
    version_tfp = function(...) "0.9.0"
  )

  expect_snapshot(
    greta_sitrep(),
    transform = redact_offline_readiness
  )
})

test_that("greta_sitrep reports a missing conda env as unused when managed", {
  skip_if_not(check_tf_version())
  skip_on_ci()
  skip_on_cran()

  local_mocked_bindings(
    have_greta_conda_env = function(...) FALSE
  )

  local_python_plan()
  expect_snapshot(
    greta_sitrep(),
    transform = redact_offline_readiness
  )
})

test_that("conda env status keeps the availability check off-managed", {
  local_mocked_bindings(
    have_greta_conda_env = function(...) FALSE
  )
  plan_managed <- new_python_plan("managed", "default")
  plan_conda <- new_python_plan("conda", "preference", python = "/envs/py")

  expect_snapshot(report_greta_conda_env_status(plan = plan_managed))
  expect_snapshot(report_greta_conda_env_status(plan = plan_conda))
})


test_that("greta_sitrep works with quiet, minimal, and detailed options", {
  skip_if_not(check_tf_version())
  skip_on_ci()
  skip_on_cran()

  # the resolved Python backend depends on machine state (RETICULATE_PYTHON, a
  # stored greta preference, or a detected conda env), so mask those lines to
  # keep the snapshot stable
  redact_backend <- function(x) {
    x <- gsub("selected via: .*", "selected via: <source>", x)
    x <- gsub('backend: ".*"', 'backend: "<backend>"', x)
    x <- gsub("python: .*", "python: <python>", x)
    redact_offline_readiness(x)
  }

  expect_snapshot(
    greta_sitrep(verbosity = "quiet"),
    transform = redact_backend
  )
  local_python_plan()
  expect_snapshot(
    greta_sitrep(verbosity = "minimal"),
    transform = redact_backend
  )
  # test it errors when verbosity is not "quiet", "minimal", and "detailed"
  expect_snapshot(
    error = TRUE,
    greta_sitrep(verbosity = "bananas")
  )

  expect_no_error(
    greta_sitrep(verbosity = "detailed")
  )
})
