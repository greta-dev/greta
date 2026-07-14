test_that("setters warn when RETICULATE_PYTHON overrides the stored preference", {
  withr::local_envvar(R_USER_CONFIG_DIR = withr::local_tempdir())
  # greta_stash is an environment: save, defer-restore, overwrite
  old <- greta_stash$reticulate_python_at_load
  withr::defer(greta_stash$reticulate_python_at_load <- old)
  greta_stash$reticulate_python_at_load <- "/fake/python"

  expect_snapshot(greta_set_python())
})

test_that("setters do not warn when RETICULATE_PYTHON is unset", {
  # same setup but stash value <- ""
  withr::local_envvar(R_USER_CONFIG_DIR = withr::local_tempdir())
  # greta_stash is an environment: save, defer-restore, overwrite
  old <- greta_stash$reticulate_python_at_load
  withr::defer(greta_stash$reticulate_python_at_load <- old)
  greta_stash$reticulate_python_at_load <- ""

  expect_snapshot(greta_set_python())
})

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

test_that("greta_set_python() stores the uv and path preferences", {
  withr::local_envvar(R_USER_CONFIG_DIR = withr::local_tempdir())

  suppressMessages(greta_set_python())
  expect_equal(get_greta_python_backend(), "managed")

  suppressMessages(greta_set_python("uv"))
  expect_equal(get_greta_python_backend(), "managed")

  fake_python <- withr::local_tempfile()
  file.create(fake_python)
  suppressMessages(greta_set_python("path", path = fake_python))
  expect_equal(get_greta_python_backend(), fake_python)
})

test_that("greta_set_python('conda') stores a conda-tagged preference", {
  withr::local_envvar(R_USER_CONFIG_DIR = withr::local_tempdir())
  seen_name <- NULL
  local_mocked_bindings(
    conda_python = function(name) {
      seen_name <<- name
      "/envs/greta-env-tf2/bin/python"
    },
    .package = "reticulate"
  )

  suppressMessages(greta_set_python("conda"))
  expect_equal(seen_name, "greta-env-tf2")
  expect_equal(
    get_greta_python_backend(),
    "conda:/envs/greta-env-tf2/bin/python"
  )

  suppressMessages(greta_set_python("conda", name = "my-env"))
  expect_equal(seen_name, "my-env")
})

test_that("greta_reset_python() clears the stored preference", {
  withr::local_envvar(R_USER_CONFIG_DIR = withr::local_tempdir())
  suppressMessages(greta_set_python())
  expect_equal(get_greta_python_backend(), "managed")

  suppressMessages(greta_reset_python())
  expect_null(get_greta_python_backend())
})

test_that("greta_set_python('path') errors on a missing path", {
  expect_error(
    greta_set_python("path", path = "/no/such/python"),
    "No Python executable"
  )
})

test_that("greta_set_python() validates backend/path/name combinations", {
  expect_error(
    greta_set_python("uv", path = "/x"),
    "can only be used"
  )
  expect_error(
    greta_set_python("conda", path = "/x"),
    "can only be used"
  )
  expect_error(
    greta_set_python("uv", name = "x"),
    "can only be used"
  )
  expect_error(
    greta_set_python("path", name = "x"),
    "can only be used"
  )
  expect_error(
    greta_set_python("path"),
    "must be supplied"
  )
})

test_that("greta_set_python() hints when a path is passed as backend", {
  expect_snapshot(error = TRUE, greta_set_python("/usr/bin/python"))
})

test_that("greta_set_python() rejects an unknown backend", {
  expect_error(greta_set_python("virtualenv"), "must be one of")
})

test_that("resolve_python_path() passes through an existing binary", {
  fake_python <- withr::local_tempfile()
  file.create(fake_python)
  expect_identical(resolve_python_path(fake_python), fake_python)
})

test_that("resolve_python_path() finds bin/python in a unix env dir", {
  local_mocked_bindings(is_windows = function() FALSE)
  env_dir <- withr::local_tempdir()
  dir.create(file.path(env_dir, "bin"))
  python <- file.path(env_dir, "bin", "python")
  file.create(python)
  expect_identical(resolve_python_path(env_dir), python)
})

test_that("resolve_python_path() finds Scripts/python.exe on windows", {
  local_mocked_bindings(is_windows = function() TRUE)
  env_dir <- withr::local_tempdir()
  dir.create(file.path(env_dir, "Scripts"))
  python <- file.path(env_dir, "Scripts", "python.exe")
  file.create(python)
  expect_identical(resolve_python_path(env_dir), python)
})

test_that("resolve_python_path() errors when nothing is found", {
  env_dir <- withr::local_tempdir()
  expect_error(resolve_python_path(env_dir), "No Python executable")
})

test_that("maybe_enable_uv_offline() does nothing when the cache is absent", {
  withr::local_envvar(UV_OFFLINE = NA)
  status <- list(kind = "managed", populated = FALSE)
  expect_false(maybe_enable_uv_offline(cache_status = status))
  expect_identical(Sys.getenv("UV_OFFLINE", unset = NA), NA_character_)
})

test_that("maybe_enable_uv_offline() enables offline when the cache is populated", {
  withr::local_envvar(UV_OFFLINE = NA)
  status <- list(kind = "managed", populated = TRUE)
  expect_true(maybe_enable_uv_offline(cache_status = status))
  expect_identical(Sys.getenv("UV_OFFLINE"), "1")
})

test_that("maybe_enable_uv_offline() respects a pre-existing UV_OFFLINE", {
  withr::local_envvar(UV_OFFLINE = "0")
  status <- list(kind = "managed", populated = TRUE)
  expect_false(maybe_enable_uv_offline(cache_status = status))
  expect_identical(Sys.getenv("UV_OFFLINE"), "0")
})

test_that("uv cache status: managed layout needs python and cache dirs", {
  uv_cache <- file.path(withr::local_tempdir(), "uv")
  status <- greta_uv_cache_status(
    system_uv = NULL,
    reticulate_uv_cache = uv_cache
  )
  expect_identical(status, list(kind = "managed", populated = FALSE))

  dir.create(file.path(uv_cache, "python"), recursive = TRUE)
  dir.create(file.path(uv_cache, "cache"), recursive = TRUE)
  status <- greta_uv_cache_status(
    system_uv = NULL,
    reticulate_uv_cache = uv_cache
  )
  expect_identical(status, list(kind = "managed", populated = TRUE))
})

test_that("uv cache status: UV_CACHE_DIR wins for a system uv", {
  uv_cache <- withr::local_tempdir()
  status <- greta_uv_cache_status(
    system_uv = "/fake/uv",
    uv_cache_dir_env = uv_cache
  )
  # a fresh cache dir (no archive/wheels buckets) is not populated
  expect_identical(status, list(kind = "system", populated = FALSE))

  dir.create(file.path(uv_cache, "wheels-v6"))
  status <- greta_uv_cache_status(
    system_uv = "/fake/uv",
    uv_cache_dir_env = uv_cache
  )
  expect_identical(status, list(kind = "system", populated = TRUE))
})

test_that("uv cache status: system uv cache located via uv itself", {
  uv_cache <- withr::local_tempdir()
  dir.create(file.path(uv_cache, "archive-v0"))
  local_mocked_bindings(system_uv_cache_dir = function(uv) uv_cache)
  status <- greta_uv_cache_status(
    system_uv = "/fake/uv",
    uv_cache_dir_env = ""
  )
  expect_identical(status, list(kind = "system", populated = TRUE))
})

test_that("uv cache status: undetermined system cache is not populated", {
  local_mocked_bindings(system_uv_cache_dir = function(uv) NULL)
  status <- greta_uv_cache_status(
    system_uv = "/fake/uv",
    uv_cache_dir_env = ""
  )
  expect_identical(status, list(kind = "system", populated = FALSE))
})

test_that("detect_system_uv() honours RETICULATE_UV and the option", {
  fake_uv <- withr::local_tempfile()
  file.create(fake_uv)

  withr::with_envvar(c(RETICULATE_UV = "managed"), {
    expect_null(detect_system_uv())
  })
  withr::with_envvar(c(RETICULATE_UV = fake_uv), {
    expect_identical(detect_system_uv(), fake_uv)
  })
  withr::with_envvar(c(RETICULATE_UV = "/no/such/uv"), {
    expect_null(detect_system_uv())
  })
  withr::with_envvar(c(RETICULATE_UV = NA), {
    withr::with_options(list(reticulate.uv_binary = "managed"), {
      expect_null(detect_system_uv())
    })
    withr::with_options(list(reticulate.uv_binary = fake_uv), {
      expect_identical(detect_system_uv(), fake_uv)
    })
  })
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

test_that("detect_greta_conda_python() returns a valid recorded python", {
  withr::local_envvar(R_USER_CONFIG_DIR = withr::local_tempdir())
  fake_python <- withr::local_tempfile()
  file.create(fake_python)

  ensure_greta_config_dir()
  writeLines(fake_python, greta_conda_record_file())

  expect_equal(detect_greta_conda_python(), fake_python)
})

test_that("detect_greta_conda_python() drops a stale record", {
  withr::local_envvar(R_USER_CONFIG_DIR = withr::local_tempdir())

  ensure_greta_config_dir()
  writeLines("/no/such/python", greta_conda_record_file())

  result <- detect_greta_conda_python()
  expect_false(identical(result, "/no/such/python"))
  expect_false(file.exists(greta_conda_record_file()))
})

test_that("detect_greta_conda_python() does not error on an empty record file", {
  withr::local_envvar(R_USER_CONFIG_DIR = withr::local_tempdir())

  ensure_greta_config_dir()
  file.create(greta_conda_record_file())

  expect_no_error(detect_greta_conda_python())
})

test_that("get_greta_python_backend() returns NULL for an empty backend file", {
  withr::local_envvar(R_USER_CONFIG_DIR = withr::local_tempdir())

  ensure_greta_config_dir()
  file.create(greta_python_backend_file())

  expect_no_error(result <- get_greta_python_backend())
  expect_null(result)
})

test_that("greta hints are shown once", {
  withr::local_envvar(R_USER_CONFIG_DIR = withr::local_tempdir())
  expect_false(greta_hint_shown("conda_to_managed"))
  mark_greta_hint_shown("conda_to_managed")
  expect_true(greta_hint_shown("conda_to_managed"))
})

test_that("flag_greta_deps_removed() sets the session flag", {
  old <- greta_stash$deps_removed_this_session
  withr::defer(greta_stash$deps_removed_this_session <- old)
  greta_stash$deps_removed_this_session <- FALSE

  flag_greta_deps_removed()
  expect_true(greta_stash$deps_removed_this_session)
})

test_that("invalidate_greta_python_session() drops the cached backend plan", {
  old_backend <- greta_stash$python_backend
  withr::defer(greta_stash$python_backend <- old_backend)
  greta_stash$python_backend <- list(backend = "conda", source = "auto_detect")

  invalidate_greta_python_session()
  expect_null(greta_stash$python_backend)
})

test_that("invalidate_greta_python_session() unsets RETICULATE_PYTHON greta owns", {
  old_at_load <- greta_stash$reticulate_python_at_load
  withr::defer(greta_stash$reticulate_python_at_load <- old_at_load)

  withr::local_envvar(RETICULATE_PYTHON = "managed")
  greta_stash$reticulate_python_at_load <- ""
  invalidate_greta_python_session()
  expect_identical(Sys.getenv("RETICULATE_PYTHON", unset = NA), NA_character_)

  withr::local_envvar(RETICULATE_PYTHON = "managed")
  greta_stash$reticulate_python_at_load <- "managed"
  invalidate_greta_python_session()
  expect_identical(Sys.getenv("RETICULATE_PYTHON", unset = NA), NA_character_)
})

test_that("invalidate_greta_python_session() leaves a user RETICULATE_PYTHON alone", {
  old_at_load <- greta_stash$reticulate_python_at_load
  withr::defer(greta_stash$reticulate_python_at_load <- old_at_load)

  withr::local_envvar(RETICULATE_PYTHON = "/opt/my/python")
  greta_stash$reticulate_python_at_load <- "/opt/my/python"
  invalidate_greta_python_session()
  expect_identical(Sys.getenv("RETICULATE_PYTHON"), "/opt/my/python")
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
  fake_python <- withr::local_tempfile()
  file.create(fake_python)
  auto <- list(backend = "conda", source = "auto_detect", python = fake_python)

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

test_that("should_nudge_to_managed() returns FALSE when the python is gone", {
  withr::local_envvar(R_USER_CONFIG_DIR = withr::local_tempdir())
  auto <- list(
    backend = "conda",
    source = "auto_detect",
    python = "/no/such/python"
  )
  expect_false(should_nudge_to_managed(auto, is_interactive = TRUE))
})

test_that("greta_set_deps() round-trips through the stored file", {
  withr::local_envvar(R_USER_CONFIG_DIR = withr::local_tempdir())
  expect_null(get_greta_stored_deps())

  deps <- greta_deps_spec(
    tf_version = "2.14.0",
    tfp_version = "0.22.1",
    python_version = "3.10"
  )
  suppressMessages(greta_set_deps(deps))
  stored <- get_greta_stored_deps()
  expect_s3_class(stored, "greta_deps_spec")
  expect_equal(stored$tf_version, "2.14.0")
  expect_equal(stored$tfp_version, "0.22.1")
  expect_equal(stored$python_version, "3.10")
})

test_that("greta_set_deps() reports what it stored", {
  withr::local_envvar(R_USER_CONFIG_DIR = withr::local_tempdir())
  expect_snapshot(greta_set_deps())
})

test_that("greta_set_deps(NULL) errors and points at greta_remove('deps')", {
  expect_snapshot(error = TRUE, greta_set_deps(NULL))
})

test_that("greta_set_deps() rejects a deps not built by greta_deps_spec()", {
  expect_error(
    greta_set_deps(list(tf_version = "2.15.1")),
    "greta_deps_spec"
  )
})

test_that("get_greta_stored_deps() treats corrupt or invalid files as absent", {
  withr::local_envvar(R_USER_CONFIG_DIR = withr::local_tempdir())
  ensure_greta_config_dir()

  writeLines("nonsense", greta_deps_file())
  expect_null(get_greta_stored_deps())

  writeLines("tf_version=2.15.1", greta_deps_file())
  expect_null(get_greta_stored_deps())

  writeLines(
    c("tf_version=2.99.0", "tfp_version=0.23.0", "python_version=3.11"),
    greta_deps_file()
  )
  expect_null(get_greta_stored_deps())
})

test_that("stored deps versions translate to py_require() arguments", {
  args <- greta_py_require_args(
    tf_version = "2.14.0",
    tfp_version = "0.22.1",
    python_version = "3.10"
  )
  expect_equal(
    args$packages,
    c("tensorflow==2.14.*", "tensorflow_probability==0.22.*")
  )
  expect_equal(args$python_version, "3.10")
})

test_that("greta_py_require_args() defaults python_version to greta's range", {
  args <- greta_py_require_args()
  expect_equal(args$python_version, greta_deps_default$python_range)
})

test_that("report_offline_readiness() reports non-managed backends as ready", {
  fake_python <- withr::local_tempfile()
  file.create(fake_python)
  plan <- new_python_plan("user", "preference", python = fake_python)
  expect_snapshot(report_offline_readiness(plan = plan))
})

test_that("report_offline_readiness() warns when a non-managed python is gone", {
  plan <- new_python_plan("conda", "auto_detect", python = "/no/such/python")
  expect_snapshot(report_offline_readiness(plan = plan))
})

test_that("report_offline_readiness() reports managed backend states", {
  plan <- new_python_plan("managed", "default")
  empty <- list(kind = "managed", populated = FALSE)
  full <- list(kind = "managed", populated = TRUE)

  expect_snapshot(report_offline_readiness(plan, empty, uv_offline = ""))
  expect_snapshot(report_offline_readiness(plan, empty, uv_offline = "1"))
  expect_snapshot(report_offline_readiness(plan, full, uv_offline = ""))
  expect_snapshot(report_offline_readiness(plan, full, uv_offline = "1"))
  expect_snapshot(report_offline_readiness(plan, full, uv_offline = "0"))
})
