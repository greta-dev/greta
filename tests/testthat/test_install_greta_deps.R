test_that("install_greta_deps errors appropriately", {
  skip_if_not(check_tf_version())
  skip_on_ci()
  expect_snapshot(error = TRUE, install_greta_deps(timeout = 0.001))
})

# These exercise the installation error messages directly (no Python needed), so
# the guidance users see when install_greta_deps() times out or fails is
# captured even where the real-install test above is skipped.
test_that("install timeout message is captured", {
  expect_snapshot(cat(timeout_install_msg(timeout = 5, py_error = "")))
})

test_that("install timeout message includes an underlying python error", {
  expect_snapshot(
    cat(timeout_install_msg(timeout = 5, py_error = "could not resolve env"))
  )
})

test_that("install failure message is captured", {
  expect_snapshot(cat(other_install_fail_msg("could not resolve env")))
})

# test_that("reinstall_greta_deps errors appropriately", {
#   skip_if_not(check_tf_version())
#   expect_snapshot(error = TRUE,
#     reinstall_greta_deps(timeout = 0.001)
#   )
# })
