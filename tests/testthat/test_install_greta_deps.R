test_that("install_greta_deps errors appropriately", {
  expect_error(
    object = install_greta_deps(timeout = 0.001),
    regexp = "^Stopping as installation of"
  )
})
