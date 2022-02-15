test_that("tensorflow returns appropriate thing with 'dim'", {
  tf_obj <- tensorflow::as_tensor(x = 1)
  expect_equal(
    dim(tf_obj),
    integer(0)
  )
})
