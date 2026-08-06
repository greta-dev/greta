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
  local_mocked_bindings(
    have_python = function() check_result(TRUE),
    have_tf = function() check_result(TRUE),
    have_tfp = function() check_result(FALSE, "No module named 'tf_keras'")
  )
  expect_snapshot(error = TRUE, check_tf_version("error"))
})

test_that("check_tf_version reports every failed requirement", {
  local_mocked_bindings(
    have_python = function() check_result(FALSE, "python is a no-show"),
    have_tf = function() check_result(FALSE, "tensorflow is a no-show"),
    have_tfp = function() check_result(FALSE, "tfp is a no-show")
  )
  expect_snapshot(error = TRUE, check_tf_version("error"))
})

test_that("a failed check without a reason still errors cleanly", {
  local_mocked_bindings(
    have_python = function() check_result(TRUE),
    have_tf = function() check_result(FALSE),
    have_tfp = function() check_result(TRUE)
  )
  expect_snapshot(error = TRUE, check_tf_version("error"))
})

test_that("braces in a Python message are not read as cli interpolation", {
  # uv and Python messages contain braces; unescaped they would be evaluated
  local_mocked_bindings(
    have_python = function() check_result(TRUE),
    have_tf = function() check_result(TRUE),
    have_tfp = function() check_result(FALSE, "bad dict {'a': 1} in {config}")
  )
  expect_snapshot(error = TRUE, check_tf_version("error"))
})
