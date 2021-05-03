test_that("Config/reticulate: has installed python and tf and tfp", {
  skip_if_not(check_tf_version())
  expect_true(have_conda())
  expect_true(have_python())
  expect_true(have_tf())
  expect_true(have_tfp())
})

test_that("tensorflow version is 1.14", {
  skip_if_not(check_tf_version())
  expect_equal(tf$`__version__`, "1.14.0")
  expect_equal(tfp$`__version__`, "0.7.0")
})
