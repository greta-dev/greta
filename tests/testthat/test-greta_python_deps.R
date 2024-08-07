test_that("greta_python_deps fails appropriately", {
  # default
  expect_snapshot(greta_python_deps())
  # TF outside range
  expect_snapshot(
    error = TRUE,
    greta_python_deps(tf_version = "2.17.0",
                    tfp_version = "0.11.0",
                    python_version = "3.8")
  )
  # TF outside range
  expect_snapshot(
    error = TRUE,
    greta_python_deps(tf_version = "1.9.0",
                    tfp_version = "0.11.0",
                    python_version = "3.8")
  )
  # TFP outside range
  expect_snapshot(
    error = TRUE,
    greta_python_deps(tf_version = "2.15.0",
                      tfp_version = "0.24.0",
                      python_version = "3.10")
  )
  # TFP outside range
  expect_snapshot(
    error = TRUE,
    greta_python_deps(tf_version = "2.15.0",
                      tfp_version = "0.6.0",
                      python_version = "3.10")
  )
  # Python outside range
  expect_snapshot(
    error = TRUE,
    greta_python_deps(tf_version = "2.9.1",
                      tfp_version = "0.23.0",
                      python_version = "3.13")
  )
  # Python outside range
  expect_snapshot(
    error = TRUE,
    greta_python_deps(tf_version = "2.9.1",
                      tfp_version = "0.23.0",
                      python_version = "2.6")
  )
  # Only Python is not valid
  # TODO - suggest changing python version in error message
  expect_snapshot(
    error = TRUE,
    greta_python_deps(tf_version = "2.15.0",
                      tfp_version = "0.23.0",
                      python_version = "3.8")
  )
  # Only TF is not valid
  # Only TFP is not valid
  expect_snapshot(
    error = TRUE,
    greta_python_deps(tf_version = "2.15.0",
                      tfp_version = "0.17.0",
                      python_version = "3.8")
    )
  expect_snapshot(
    error = TRUE,
    greta_python_deps(tf_version = "2.17.0",
                      tfp_version = "0.23.0",
                      python_version = "3.8")
    )
  expect_snapshot(
    error = TRUE,
    greta_python_deps(tf_version = "2.9.0",
                      tfp_version = "0.17.0",
                      python_version = "3.8")
    )
})
