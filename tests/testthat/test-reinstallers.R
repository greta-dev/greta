test_that("remove_greta_env(ask = FALSE) removes without prompting", {
  withr::local_options(lifecycle_verbosity = "quiet")
  withr::local_envvar(R_USER_CONFIG_DIR = withr::local_tempdir())
  local_mocked_bindings(have_greta_conda_env = function(...) TRUE)
  local_mocked_bindings(
    conda_remove = function(...) NULL,
    .package = "reticulate"
  )
  # if any prompt fires, fail loudly — this is finding 2's regression test
  local_mocked_bindings(
    yesno = function(...) stop("yesno should not be called"),
    .package = "yesno"
  )
  # pre-create the install-time record to test the cleanup
  ensure_greta_config_dir()
  writeLines("/fake/python", greta_conda_record_file())

  expect_message(
    res <- remove_greta_env(ask = FALSE),
    "environment removed"
  )
  expect_true(res)
  expect_false(file.exists(greta_conda_record_file()))
})

test_that("remove_greta_env(ask = FALSE) flags the session and drops the cached backend", {
  withr::local_options(lifecycle_verbosity = "quiet")
  withr::local_envvar(R_USER_CONFIG_DIR = withr::local_tempdir())
  local_mocked_bindings(have_greta_conda_env = function(...) TRUE)
  local_mocked_bindings(
    conda_remove = function(...) NULL,
    .package = "reticulate"
  )
  old_flag <- greta_stash$deps_removed_this_session
  old_backend <- greta_stash$python_backend
  withr::defer({
    greta_stash$deps_removed_this_session <- old_flag
    greta_stash$python_backend <- old_backend
  })
  greta_stash$deps_removed_this_session <- FALSE
  greta_stash$python_backend <- list(backend = "conda", source = "auto_detect")

  expect_message(remove_greta_env(ask = FALSE), "environment removed")
  expect_true(greta_stash$deps_removed_this_session)
  expect_null(greta_stash$python_backend)
})

test_that("remove_miniconda(ask = FALSE) flags the session and drops the cached backend", {
  withr::local_options(lifecycle_verbosity = "quiet")
  fake_miniconda <- withr::local_tempdir()
  local_mocked_bindings(
    miniconda_path = function(...) fake_miniconda,
    .package = "reticulate"
  )
  old_flag <- greta_stash$deps_removed_this_session
  old_backend <- greta_stash$python_backend
  withr::defer({
    greta_stash$deps_removed_this_session <- old_flag
    greta_stash$python_backend <- old_backend
  })
  greta_stash$deps_removed_this_session <- FALSE
  greta_stash$python_backend <- list(backend = "managed", source = "default")

  expect_message(remove_miniconda(ask = FALSE), "successfully removed")
  expect_true(greta_stash$deps_removed_this_session)
  expect_null(greta_stash$python_backend)
})

test_that("remove_greta_env returns FALSE when there is no environment", {
  withr::local_options(lifecycle_verbosity = "quiet")
  local_mocked_bindings(have_greta_conda_env = function(...) FALSE)
  expect_message(res <- remove_greta_env(ask = FALSE), "No 'greta-env-tf2'")
  expect_false(res)
})


test_that("declining the prompt skips removal and returns FALSE", {
  withr::local_options(lifecycle_verbosity = "quiet")
  local_mocked_bindings(have_greta_conda_env = function(...) TRUE)
  local_mocked_bindings(
    conda_remove = function(...) stop("must not remove"),
    .package = "reticulate"
  )
  # simulate the user answering "no"
  local_mocked_bindings(yesno = function(...) FALSE, .package = "yesno")
  expect_false(remove_greta_env(ask = TRUE))
})


test_that("greta_remove_all_deps reports honestly when nothing exists", {
  withr::local_options(lifecycle_verbosity = "quiet")
  withr::local_envvar(
    R_USER_CONFIG_DIR = withr::local_tempdir(),
    # R_user_dir("reticulate", "cache") respects this, isolating the uv-cache
    # check without mocking
    R_USER_CACHE_DIR = "/greta-test/cache"
  )
  local_mocked_bindings(have_greta_conda_env = function(...) FALSE)
  local_mocked_bindings(
    miniconda_path = function(...) "/greta-test/no-such-miniconda",
    .package = "reticulate"
  )
  expect_snapshot(res <- greta_remove_all_deps(ask = FALSE))
  expect_false(res)
})

test_that("greta_remove_all_deps reports removal when something was removed", {
  withr::local_options(lifecycle_verbosity = "quiet")
  withr::local_envvar(
    R_USER_CONFIG_DIR = withr::local_tempdir(),
    R_USER_CACHE_DIR = "/greta-test/cache"
  )
  local_mocked_bindings(have_greta_conda_env = function(...) TRUE)
  local_mocked_bindings(
    conda_remove = function(...) NULL,
    miniconda_path = function(...) "/greta-test/no-such-miniconda",
    .package = "reticulate"
  )
  expect_snapshot(res <- greta_remove_all_deps(ask = FALSE))
  expect_true(res)
})

###

test_that("destroy_greta_deps reports honestly when nothing exists", {
  # force the deprecation warning to fire so we snapshot it
  withr::local_options(lifecycle_verbosity = "warning")
  local_mocked_bindings(have_greta_conda_env = function(...) FALSE)
  local_mocked_bindings(
    miniconda_path = function(...) "/greta-test/no-such-miniconda",
    .package = "reticulate"
  )
  expect_snapshot(
    res <- destroy_greta_deps(ask = FALSE)
  )
  expect_false(res)
})

test_that("destroy_greta_deps reports removal when something was removed", {
  withr::local_options(lifecycle_verbosity = "warning")
  withr::local_envvar(
    R_USER_CONFIG_DIR = withr::local_tempdir(),
    R_USER_CACHE_DIR = "/greta-test/cache"
  )
  local_mocked_bindings(have_greta_conda_env = function(...) TRUE)
  local_mocked_bindings(
    conda_remove = function(...) NULL,
    miniconda_path = function(...) "/greta-test/no-such-miniconda",
    .package = "reticulate"
  )
  expect_snapshot(
    res <- destroy_greta_deps(ask = FALSE)
  )
  expect_true(res)
})

###

test_that("greta_remove() dispatches to the right internal remover", {
  calls <- character()
  local_mocked_bindings(
    remove_greta_all = function(...) {
      calls[["all"]] <<- "all"
      invisible(TRUE)
    },
    remove_greta_env_impl = function(...) {
      calls[["env"]] <<- "env"
      invisible(TRUE)
    },
    remove_miniconda_impl = function(...) {
      calls[["miniconda"]] <<- "miniconda"
      invisible(TRUE)
    },
    remove_reticulate_uv_cache_impl = function(...) {
      calls[["uv_cache"]] <<- "uv_cache"
      invisible(TRUE)
    },
    remove_greta_preference_impl = function(...) {
      calls[["preference"]] <<- "preference"
      invisible(TRUE)
    },
    remove_greta_deps_impl = function(...) {
      calls[["deps"]] <<- "deps"
      invisible(TRUE)
    }
  )

  greta_remove("all")
  greta_remove("env")
  greta_remove("miniconda")
  greta_remove("uv_cache")
  greta_remove("preference")
  greta_remove("deps")

  expect_setequal(
    names(calls),
    c("all", "env", "miniconda", "uv_cache", "preference", "deps")
  )
})

test_that("greta_remove() defaults to removing everything", {
  called <- FALSE
  local_mocked_bindings(
    remove_greta_all = function(...) {
      called <<- TRUE
      invisible(TRUE)
    }
  )
  greta_remove()
  expect_true(called)
})

test_that("greta_remove() rejects an unknown `what`", {
  expect_error(greta_remove("nonsense"), "should be one of")
})

test_that("greta_remove('preference') clears only the Python preference", {
  withr::local_envvar(R_USER_CONFIG_DIR = withr::local_tempdir())
  set_greta_python_backend("managed")
  suppressMessages(greta_set_deps(greta_deps_spec()))
  expect_message(res <- greta_remove("preference"), "Cleared")
  expect_true(res)
  expect_null(get_greta_python_backend())
  expect_s3_class(get_greta_stored_deps(), "greta_deps_spec")
})

test_that("greta_remove('deps') clears only the stored dependency versions", {
  withr::local_envvar(R_USER_CONFIG_DIR = withr::local_tempdir())
  set_greta_python_backend("managed")
  suppressMessages(greta_set_deps(greta_deps_spec()))
  expect_message(res <- greta_remove("deps"), "Cleared")
  expect_true(res)
  expect_null(get_greta_stored_deps())
  expect_equal(get_greta_python_backend(), "managed")
})

test_that("greta_remove('deps') reports when nothing is stored", {
  withr::local_envvar(R_USER_CONFIG_DIR = withr::local_tempdir())
  expect_message(res <- greta_remove("deps"), "No stored")
  expect_false(res)
})

test_that("greta_remove('all') clears stored preferences including deps", {
  withr::local_envvar(R_USER_CONFIG_DIR = withr::local_tempdir())
  local_mocked_bindings(
    remove_greta_env_impl = function(...) FALSE,
    remove_miniconda_impl = function(...) FALSE,
    remove_reticulate_uv_cache_impl = function(...) FALSE
  )
  set_greta_python_backend("managed")
  suppressMessages(greta_set_deps(greta_deps_spec()))

  suppressMessages(greta_remove(ask = FALSE))
  expect_null(get_greta_python_backend())
  expect_null(get_greta_stored_deps())
})

test_that("greta_remove('all') flags the session when the env or miniconda was removed", {
  withr::local_envvar(R_USER_CONFIG_DIR = withr::local_tempdir())
  local_mocked_bindings(
    remove_greta_env_impl = function(...) TRUE,
    remove_miniconda_impl = function(...) FALSE,
    remove_reticulate_uv_cache_impl = function(...) FALSE
  )
  old_flag <- greta_stash$deps_removed_this_session
  withr::defer(greta_stash$deps_removed_this_session <- old_flag)
  greta_stash$deps_removed_this_session <- FALSE

  suppressMessages(greta_remove(ask = FALSE))
  expect_true(greta_stash$deps_removed_this_session)
})

test_that("greta_remove('all') does not flag the session when nothing was removed", {
  withr::local_envvar(R_USER_CONFIG_DIR = withr::local_tempdir())
  local_mocked_bindings(
    remove_greta_env_impl = function(...) FALSE,
    remove_miniconda_impl = function(...) FALSE,
    remove_reticulate_uv_cache_impl = function(...) FALSE
  )
  old_flag <- greta_stash$deps_removed_this_session
  withr::defer(greta_stash$deps_removed_this_session <- old_flag)
  greta_stash$deps_removed_this_session <- FALSE

  suppressMessages(greta_remove(ask = FALSE))
  expect_false(greta_stash$deps_removed_this_session)
})
