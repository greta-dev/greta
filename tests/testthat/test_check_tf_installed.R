test_that("Config/reticulate: option works", {
  expect_true(have_conda())
  expect_true(have_python())
  expect_true(have_tf())
  expect_true(have_tfp())
})

test_that("tensorflow and tf probability versions are 1.14 and 0.7.0", {
  expect_equal(tf$`__version__`, "1.14.0")
  expect_equal(tfp$`__version__`, "0.7.0")
})
