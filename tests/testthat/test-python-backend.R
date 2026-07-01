test_that("greta_python_plan() respects a user-set RETICULATE_PYTHON path", {
  plan <- greta_python_plan(
    reticulate_python = "/usr/bin/python",
    stored_backend = NULL,
    conda_python = NULL
  )
  expect_equal(plan$backend, "user")
  expect_equal(plan$python, "/usr/bin/python")
  expect_equal(plan$source, "RETICULATE_PYTHON")
})

test_that("greta_python_plan() treats RETICULATE_PYTHON='managed' as managed", {
  plan <- greta_python_plan(
    reticulate_python = "managed",
    stored_backend = NULL,
    conda_python = NULL
  )
  expect_equal(plan$backend, "managed")
})

test_that("greta_python_plan() honours a stored preference", {
  plan_managed <- greta_python_plan(
    "",
    stored_backend = "managed",
    conda_python = NULL
  )
  expect_equal(plan_managed$backend, "managed")
  expect_equal(plan_managed$source, "preference")

  plan_path <- greta_python_plan(
    "",
    stored_backend = "/opt/py",
    conda_python = NULL
  )
  expect_equal(plan_path$backend, "user")
  expect_equal(plan_path$python, "/opt/py")
})

test_that("greta_python_plan() auto-detects an existing greta conda env", {
  plan <- greta_python_plan(
    reticulate_python = "",
    stored_backend = NULL,
    conda_python = "/home/me/envs/greta-env-tf2/bin/python"
  )
  expect_equal(plan$backend, "conda")
  expect_equal(plan$source, "auto_detect")
})

test_that("greta_python_plan() defaults to managed", {
  plan <- greta_python_plan("", stored_backend = NULL, conda_python = NULL)
  expect_equal(plan$backend, "managed")
  expect_equal(plan$source, "default")
})

test_that("greta_python_plan() priority: env path > preference > conda", {
  plan <- greta_python_plan(
    reticulate_python = "/usr/bin/python",
    stored_backend = "managed",
    conda_python = "/envs/greta-env-tf2/bin/python"
  )
  expect_equal(plan$backend, "user")
  expect_equal(plan$python, "/usr/bin/python")
})

test_that("python preference round-trips and clears", {
  withr::local_envvar(R_USER_CONFIG_DIR = withr::local_tempdir())
  expect_null(get_greta_python_backend())

  set_greta_python_backend("managed")
  expect_equal(get_greta_python_backend(), "managed")

  clear_greta_python_backend()
  expect_null(get_greta_python_backend())
})

test_that("greta_set_python_uv() / greta_set_python_path() store the preference", {
  withr::local_envvar(R_USER_CONFIG_DIR = withr::local_tempdir())

  suppressMessages(greta_set_python_uv())
  expect_equal(get_greta_python_backend(), "managed")

  fake_python <- withr::local_tempfile()
  file.create(fake_python)
  suppressMessages(greta_set_python_path(fake_python))
  expect_equal(get_greta_python_backend(), fake_python)
})

test_that("greta_reset_python() clears the stored preference", {
  withr::local_envvar(R_USER_CONFIG_DIR = withr::local_tempdir())
  suppressMessages(greta_set_python_uv())
  expect_equal(get_greta_python_backend(), "managed")

  suppressMessages(greta_reset_python())
  expect_null(get_greta_python_backend())
})

test_that("greta_set_python_path() errors on a missing path", {
  expect_error(
    greta_set_python_path("/no/such/python"),
    "No Python executable"
  )
})

test_that("a conda-tagged preference resolves to a conda backend", {
  plan <- plan_from_value("conda:/x/y/bin/python", source = "preference")
  expect_identical(plan$backend, "conda")
  expect_identical(plan$python, "/x/y/bin/python")
})

test_that("greta_python_plan honours a conda-tagged stored preference", {
  withr::local_envvar(R_USER_CONFIG_DIR = withr::local_tempdir())
  set_greta_python_backend("conda:/x/y/bin/python")
  plan <- greta_python_plan(reticulate_python = "", conda_python = NULL)
  expect_identical(plan$backend, "conda")
  expect_identical(plan$source, "preference")
  expect_identical(plan$python, "/x/y/bin/python")
})

test_that("pending backend report simulates a fresh restart", {
  # mimic an active session where greta has already set RETICULATE_PYTHON, while
  # the user themselves set none: the report must reflect the new preference,
  # not greta's session env var
  withr::local_envvar(
    R_USER_CONFIG_DIR = withr::local_tempdir(),
    RETICULATE_PYTHON = "/fake/conda/python"
  )
  old <- greta_stash$reticulate_python_at_load
  greta_stash$reticulate_python_at_load <- ""
  withr::defer(greta_stash$reticulate_python_at_load <- old)

  set_greta_python_backend("managed")
  plan <- suppressMessages(report_pending_python_backend())
  expect_identical(plan$backend, "managed")
  expect_identical(plan$source, "preference")
})

test_that("greta hints are shown once", {
  withr::local_envvar(R_USER_CONFIG_DIR = withr::local_tempdir())
  expect_false(greta_hint_shown("conda_to_managed"))
  mark_greta_hint_shown("conda_to_managed")
  expect_true(greta_hint_shown("conda_to_managed"))
})

test_that("RETICULATE_PYTHON takes precedence over a stored preference (#801)", {
  plan <- greta_python_plan(
    reticulate_python = "managed",
    stored_backend = "/opt/py",
    conda_python = NULL
  )
  expect_equal(plan$backend, "managed")
  expect_equal(plan$source, "RETICULATE_PYTHON")
})

test_that("should_nudge_to_managed() only fires for an interactive auto-detect", {
  withr::local_envvar(R_USER_CONFIG_DIR = withr::local_tempdir())
  auto <- list(backend = "conda", source = "auto_detect")

  expect_true(should_nudge_to_managed(auto, is_interactive = TRUE))
  expect_false(should_nudge_to_managed(auto, is_interactive = FALSE))
  expect_false(should_nudge_to_managed(NULL, is_interactive = TRUE))
  expect_false(should_nudge_to_managed(
    list(backend = "managed", source = "default"),
    is_interactive = TRUE
  ))

  mark_greta_hint_shown("conda_to_managed")
  expect_false(should_nudge_to_managed(auto, is_interactive = TRUE))
})
