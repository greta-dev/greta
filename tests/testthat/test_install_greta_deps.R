test_that("install_greta_deps errors appropriately", {
  expect_snapshot(
    error = TRUE,
    install_greta_deps(timeout = 0.001)
  )
})
