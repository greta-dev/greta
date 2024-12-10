test_that("install_greta_deps errors appropriately", {
  skip_if_not(check_tf_version())
  expect_snapshot(error = TRUE,
    install_greta_deps(timeout = 0.001)
  )
})

# test_that("reinstall_greta_deps errors appropriately", {
#   skip_if_not(check_tf_version())
#   expect_snapshot(error = TRUE,
#     reinstall_greta_deps(timeout = 0.001)
#   )
# })
