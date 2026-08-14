test_that("check_result carries the reason and still behaves as a logical", {
  ok <- check_result(TRUE)
  expect_true(isTRUE(ok))
  expect_null(check_reason(ok))

  failed <- check_result(FALSE, "no solution")
  expect_false(isTRUE(failed))
  expect_identical(check_reason(failed), "no solution")
})

test_that("have_python reports why Python could not be loaded", {
  local_mocked_bindings(
    py_available = function(...) {
      stop("uv could not resolve a Python", call. = FALSE)
    },
    .package = "reticulate"
  )
  result <- have_python()
  expect_false(isTRUE(result))
  expect_match(check_reason(result), "uv could not resolve a Python")
})

test_that("check_tf_version names which requirement failed", {
  local_python_plan()
  local_mocked_bindings(
    have_python = function() check_result(TRUE),
    have_tf = function() check_result(TRUE),
    have_tfp = function() check_result(FALSE, "No module named 'tf_keras'")
  )
  expect_snapshot(error = TRUE, check_tf_version("error"))
})

test_that("a module older than greta's floor is rejected, naming both", {
  too_old <- have_python_module(
    label = "TensorFlow",
    get_version = function() "2.15.0",
    minimum = greta_deps_default$tf_min
  )
  expect_false(isTRUE(too_old))
  expect_match(check_reason(too_old), "2.15.0 is installed")
  expect_match(check_reason(too_old), greta_deps_default$tf_min)

  new_enough <- have_python_module(
    label = "TensorFlow",
    get_version = function() greta_deps_default$tf,
    minimum = greta_deps_default$tf_min
  )
  expect_true(isTRUE(new_enough))
})

# The load gate and greta_deps_spec() are separate code paths that must agree
# about which versions greta supports. They disagreed for the whole of the Keras
# 3 changeover -- the gate accepted TensorFlow 2.9 while greta had moved to an
# API that needs 2.18 -- so an old environment loaded and then failed inside
# Keras.
test_that("the load gate and greta_deps_spec() agree on the floor", {
  below_floor <- "2.17.0"

  expect_error(
    greta_deps_spec(tf_version = below_floor),
    "supports TensorFlow"
  )

  gate <- have_python_module(
    label = "TensorFlow",
    get_version = function() below_floor,
    minimum = greta_deps_default$tf_min
  )
  expect_false(isTRUE(gate))
})

test_that("a too-old TensorFlow on conda says what to run", {
  local_python_plan(
    new_python_plan("conda", "auto_detect", python = "greta-env-tf2")
  )
  local_mocked_bindings(
    have_python = function() check_result(TRUE),
    have_tf = function() {
      check_result(
        FALSE,
        "TensorFlow 2.15.0 is installed, but greta needs 2.18.0 or later"
      )
    },
    have_tfp = function() check_result(TRUE)
  )
  expect_snapshot(error = TRUE, check_tf_version("error"))
})

test_that("a too-old TensorFlow on the managed backend says to restart", {
  local_python_plan()
  local_mocked_bindings(
    have_python = function() check_result(TRUE),
    have_tf = function() {
      check_result(
        FALSE,
        "TensorFlow 2.15.0 is installed, but greta needs 2.18.0 or later"
      )
    },
    have_tfp = function() check_result(TRUE)
  )
  expect_snapshot(error = TRUE, check_tf_version("error"))
})

test_that("check_tf_version reports every failed requirement", {
  # the diagnostic is mocked: it spawns a subprocess, and this test is about
  # the message rather than about running one
  local_python_plan()
  local_mocked_bindings(
    have_python = function() check_result(FALSE, "python is a no-show"),
    have_tf = function() check_result(FALSE, "tensorflow is a no-show"),
    have_tfp = function() check_result(FALSE, "tfp is a no-show"),
    diagnose_python_load = function(...) "mocked diagnosis",
    write_greta_install_log = function(...) invisible(NULL)
  )
  expect_snapshot(error = TRUE, check_tf_version("error"))
})

test_that("a Python failure is diagnosed, and other failures are not", {
  # an environment records the call by reference, so the mock does not need to
  # reach out and rebind something in the enclosing frame
  calls <- new.env(parent = emptyenv())
  calls$diagnosed <- FALSE

  local_mocked_bindings(
    have_python = function() check_result(FALSE, "python is a no-show"),
    have_tf = function() check_result(TRUE),
    have_tfp = function() check_result(TRUE),
    diagnose_python_load = function(...) {
      calls$diagnosed <- TRUE
      "mocked diagnosis"
    },
    write_greta_install_log = function(...) invisible(NULL)
  )
  expect_error(check_tf_version("error"))
  expect_true(calls$diagnosed)

  # a module failure carries its own reason, so there is nothing to go and find
  calls$diagnosed <- FALSE
  local_mocked_bindings(
    have_python = function() check_result(TRUE),
    have_tf = function() check_result(FALSE, "tensorflow is a no-show"),
    have_tfp = function() check_result(TRUE),
    diagnose_python_load = function(...) {
      calls$diagnosed <- TRUE
      "mocked diagnosis"
    },
    write_greta_install_log = function(...) invisible(NULL)
  )
  expect_error(check_tf_version("error"))
  expect_false(calls$diagnosed)
})

test_that("check_tf_version does not diagnose when it is not erroring", {
  calls <- new.env(parent = emptyenv())
  calls$diagnosed <- FALSE

  local_mocked_bindings(
    have_python = function() check_result(FALSE, "python is a no-show"),
    have_tf = function() check_result(TRUE),
    have_tfp = function() check_result(TRUE),
    diagnose_python_load = function(...) {
      calls$diagnosed <- TRUE
      "mocked diagnosis"
    },
    write_greta_install_log = function(...) invisible(NULL)
  )
  # skips in the test suite run through alert = "none"; spawning a subprocess
  # per call there would be intolerable
  expect_false(check_tf_version("none"))
  expect_false(calls$diagnosed)
})

test_that("a failed check without a reason still errors cleanly", {
  local_python_plan()
  local_mocked_bindings(
    have_python = function() check_result(TRUE),
    have_tf = function() check_result(FALSE),
    have_tfp = function() check_result(TRUE)
  )
  expect_snapshot(error = TRUE, check_tf_version("error"))
})

test_that("braces in a Python message are not read as cli interpolation", {
  # uv and Python messages contain braces; unescaped they would be evaluated
  local_python_plan()
  local_mocked_bindings(
    have_python = function() check_result(TRUE),
    have_tf = function() check_result(TRUE),
    have_tfp = function() check_result(FALSE, "bad dict {'a': 1} in {config}")
  )
  expect_snapshot(error = TRUE, check_tf_version("error"))
})
