test_that("check_tf_version fails when M1 mac detected", {
  mockery::stub(check_tf_version, 'is_mac_arm64', TRUE, 2)
  expect_snapshot_error(
    check_tf_version("error")
  )
})
