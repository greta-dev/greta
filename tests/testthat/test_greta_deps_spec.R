test_that("greta python range detection works correctly",{
  skip_if_not(check_tf_version())
  # correct ranges
  expect_snapshot(check_greta_python_range("3.11"))
  expect_snapshot(check_greta_python_range("3.9"))
  expect_snapshot(check_greta_python_range("3.3"))
  expect_snapshot(check_greta_python_range("3.8.2"))
  expect_snapshot(check_greta_python_range("3.3.3"))
  # incorrect ranges
  expect_snapshot(
    error = TRUE,
    check_greta_python_range("3.1")
  )
  expect_snapshot(
    error = TRUE,
    check_greta_python_range("3.12")
  )
  expect_snapshot(
    error = TRUE,
    check_greta_python_range("2.7")
  )
  expect_snapshot(
    error = TRUE,
    check_greta_python_range("3.1.1")
  )
  expect_snapshot(
    error = TRUE,
    check_greta_python_range("3.14")
  )
})


test_that("greta_deps_spec fails appropriately", {
  skip_if_not(check_tf_version())
  # skip on windows as there are small differences in version recommendations
  skip_on_os(os = "windows")
  # default
  expect_snapshot(greta_deps_spec())
  # some correct ranges
  expect_snapshot(
    greta_deps_spec(tf_version = "2.14.0",
                    tfp_version = "0.22.1",
                    python_version = "3.9")
  )
  expect_snapshot(
    greta_deps_spec(tf_version = "2.12.0",
                    tfp_version = "0.20.0",
                    python_version = "3.9")
  )
  # TF above range
  expect_snapshot(
    error = TRUE,
    greta_deps_spec(tf_version = "2.16.1",
                    tfp_version = "0.11.0",
                    python_version = "3.8")
  )
  # TF below range
  expect_snapshot(
    error = TRUE,
    greta_deps_spec(tf_version = "1.9.0",
                    tfp_version = "0.11.0",
                    python_version = "3.8")
  )
  # TFP above range
  expect_snapshot(
    error = TRUE,
    greta_deps_spec(tf_version = "2.15.0",
                    tfp_version = "0.24.0",
                    python_version = "3.10")
  )
  # TFP below range
  expect_snapshot(
    error = TRUE,
    greta_deps_spec(tf_version = "2.15.0",
                    tfp_version = "0.6.0",
                    python_version = "3.10")
  )
  # Python above range
  expect_snapshot(
    error = TRUE,
    greta_deps_spec(tf_version = "2.9.1",
                    tfp_version = "0.23.0",
                    python_version = "3.13")
  )
  # Python below range
  expect_snapshot(
    error = TRUE,
    greta_deps_spec(tf_version = "2.9.1",
                    tfp_version = "0.23.0",
                    python_version = "2.6")
  )
  # Only Python is not valid
  # TODO - suggest changing python version in error message
  expect_snapshot(
    error = TRUE,
    greta_deps_spec(tf_version = "2.15.0",
                    tfp_version = "0.23.0",
                    python_version = "3.8")
  )
  # Only TF is not valid
  expect_snapshot(
    error = TRUE,
    greta_deps_spec(tf_version = "2.15.0",
                    tfp_version = "0.22.0",
                    python_version = "3.10")
  )
  expect_snapshot(
    error = TRUE,
    greta_deps_spec(tf_version = "2.14.0",
                    tfp_version = "0.21.0",
                    python_version = "3.8")
  )
  expect_snapshot(
    error = TRUE,
    greta_deps_spec(tf_version = "2.13.0",
                    tfp_version = "0.20.0",
                    python_version = "3.8")
  )
  # Only TFP is not valid
  expect_snapshot(
    error = TRUE,
    greta_deps_spec(tf_version = "2.15.0",
                    tfp_version = "0.17.0",
                    python_version = "3.8")
  )
  expect_snapshot(
    error = TRUE,
    greta_deps_spec(tf_version = "2.17.0",
                    tfp_version = "0.23.0",
                    python_version = "3.8")
  )
  expect_snapshot(
    error = TRUE,
    greta_deps_spec(tf_version = "2.9.0",
                    tfp_version = "0.17.0",
                    python_version = "3.8")
  )
})
