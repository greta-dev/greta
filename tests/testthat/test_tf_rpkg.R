test_that("Tensor behaves as we expect",{
  x <- tensorflow::as_tensor(42, "int32")
  expect_snapshot(x)
  expect_snapshot(dim(x))
  expect_snapshot(
    tf$reshape(x, tensorflow::as_tensor(shape(-1, 1, 1L, 1, 1L)))
  )
})

test_that("shape behaves as we expect",{
  expect_snapshot(
    tensorflow::shape(1,1,1)
  )
})


# other parts to test:
# batch_size <- tf$shape(x)[[0]]
# shape_list <- c(list(batch_size), as.integer(to_shape(dims_out)))
# shape_out <- tf$stack(shape_list)
#
# tf$reshape(ref[, idx, ], tensorflow::as_tensor(shape(-1, length(idx), 1)))
#
# update_list <- lapply(run_id, function(i) {
#   tf$reshape(updates[, i - 1, ], tensorflow::as_tensor(shape(-1, 1, 1)))
# })
#
# has_batch(x)
#
#
# test_that("multiplication works", {
#   expect_equal(2 * 2, 4)
# })
