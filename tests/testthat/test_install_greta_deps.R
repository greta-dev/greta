test_that("install_greta_deps errors appropriately", {
  skip_if_not(check_tf_version())
  expect_error(
    object = install_greta_deps(timeout = 0.001),
    regexp = "^Stopping as installation of"
  )
})
