test_that("limit_tf_cores_under_check caps TF thread pools under R CMD check", {
  withr::local_envvar("_R_CHECK_LIMIT_CORES_" = "TRUE")
  intra <- NULL
  inter <- NULL
  fake_threading <- list(
    set_intra_op_parallelism_threads = \(n) intra <<- n,
    set_inter_op_parallelism_threads = \(n) inter <<- n
  )

  expect_true(limit_tf_cores_under_check(threading = fake_threading))
  expect_equal(intra, 2L)
  expect_equal(inter, 2L)
})

test_that("limit_tf_cores_under_check does nothing when not under R CMD check", {
  called <- FALSE
  fake_threading <- list(
    set_intra_op_parallelism_threads = \(n) called <<- TRUE,
    set_inter_op_parallelism_threads = \(n) called <<- TRUE
  )

  withr::local_envvar("_R_CHECK_LIMIT_CORES_" = NA)
  expect_false(limit_tf_cores_under_check(threading = fake_threading))
  expect_false(called)

  withr::local_envvar("_R_CHECK_LIMIT_CORES_" = "false")
  expect_false(limit_tf_cores_under_check(threading = fake_threading))
  expect_false(called)
})

test_that("limit_tf_cores_under_check survives TensorFlow errors", {
  withr::local_envvar("_R_CHECK_LIMIT_CORES_" = "TRUE")
  fake_threading <- list(
    set_intra_op_parallelism_threads = \(n) {
      stop("cannot modify after context init")
    },
    set_inter_op_parallelism_threads = \(n) NULL
  )

  expect_false(limit_tf_cores_under_check(threading = fake_threading))
})

test_that("check_tf_version() hints at restarting R after greta_remove() this session", {
  local_mocked_bindings(
    have_python = function(...) FALSE,
    have_tf = function(...) FALSE,
    have_tfp = function(...) FALSE
  )
  old_flag <- greta_stash$deps_removed_this_session
  old_init <- greta_stash$python_has_been_initialised
  withr::defer({
    greta_stash$deps_removed_this_session <- old_flag
    greta_stash$python_has_been_initialised <- old_init
  })
  greta_stash$deps_removed_this_session <- TRUE
  greta_stash$python_has_been_initialised <- TRUE

  expect_snapshot(check_tf_version("message"))
})

test_that("check_tf_version() omits the restart hint when nothing was removed", {
  local_mocked_bindings(
    have_python = function(...) FALSE,
    have_tf = function(...) FALSE,
    have_tfp = function(...) FALSE
  )
  old_flag <- greta_stash$deps_removed_this_session
  old_init <- greta_stash$python_has_been_initialised
  withr::defer({
    greta_stash$deps_removed_this_session <- old_flag
    greta_stash$python_has_been_initialised <- old_init
  })
  greta_stash$deps_removed_this_session <- FALSE
  greta_stash$python_has_been_initialised <- TRUE

  expect_snapshot(check_tf_version("message"))
})
