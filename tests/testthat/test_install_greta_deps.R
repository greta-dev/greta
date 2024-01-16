test_that("install_greta_deps errors appropriately", {
  skip_if_not(check_tf_version())
  expect_snapshot_error(
    install_greta_deps(timeout = 0.001)
  )
})

# test_that("reinstall_greta_deps errors appropriately", {
#   skip_if_not(check_tf_version())
#   expect_snapshot_error(
#     reinstall_greta_deps(timeout = 0.001)
#   )
# })
