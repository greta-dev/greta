test_that("Config/reticulate: has installed python and tf and tfp", {
  expect_true(have_conda())
  expect_true(have_python())
  expect_true(have_tf())
  expect_true(have_tfp())
})

test_that("tensorflow version is 1.14", {
  expect_equal(tensorflow::tf_version(), "1.14")
})
