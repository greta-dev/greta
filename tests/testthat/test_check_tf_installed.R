test_that("Config/reticulate: has installed python and tf and tfp", {
  skip_if_not(check_tf_version())
  expect_true(have_conda())
  expect_true(have_python())
  expect_true(have_tf())
  expect_true(have_tfp())
})

test_that("TF version is at least version 2.9.0", {
  skip_if_not(check_tf_version())
  tf_version <- tf$`__version__`
  expect_gte(compareVersion(tf_version, "2.9.0"), 0)
})

test_that("TFP version is at least version 1.18.0", {
  skip_if_not(check_tf_version())
  tfp_version <- tfp$`__version__`
  expect_gte(compareVersion(tfp_version, "0.18.0"), 0)
})
