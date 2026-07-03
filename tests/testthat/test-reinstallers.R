test_that("remove_greta_env(ask = FALSE) removes without prompting", {
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

test_that("remove_greta_env returns FALSE when there is no environment", {
  local_mocked_bindings(have_greta_conda_env = function(...) FALSE)
  expect_message(res <- remove_greta_env(ask = FALSE), "No 'greta-env-tf2'")
  expect_false(res)
})


test_that("declining the prompt skips removal and returns FALSE", {
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
