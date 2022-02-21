
test_that("tensorflow returns appropriate thing with 'dim'", {
  skip_if_not(check_tf_version())
  xt_int_32 <- as_tensor(x = 42, "int32")
  xt_int_64 <- as_tensor(x = 42, "int64")
  xt_float_32 <- as_tensor(x = 42, "float32")
  xt_float_32_dec <- as_tensor(x = 42.2, "float32")

  expect_equal(dim(xt_int_32), integer(0))
  expect_equal(dim(xt_int_64), integer(0))
  expect_equal(dim(xt_float_32), integer(0))
  expect_equal(dim(xt_float_32_dec), integer(0))
  expect_equal(dim(shape(1,2,3)), NULL)
  expect_equal(dim(as_tensor(c(1:3))), 3)
})

test_that("Tensor behaves as we expect",{
  skip_if_not(check_tf_version())
  x <- tensorflow::as_tensor(42, "int32")
  expect_snapshot_output(x)
  expect_snapshot_output(dim(x))
  expect_snapshot_output(
    tf$reshape(x, tensorflow::as_tensor(shape(-1, 1, 1L, 1, 1L)))
  )
})

test_that("shape returns right thing", {
  skip_if_not(check_tf_version())
  expect_snapshot_output(shape())
  expect_snapshot_output(shape(NULL))
  expect_snapshot_output(shape(NA))
  expect_snapshot_output(shape(dims = NULL))
  expect_snapshot_output(shape(3, 4))
  expect_snapshot_output(shape(NA, 4))
  expect_snapshot_output(shape(dims = c(NA, 4)))
  expect_snapshot_output(shape(1,1,1))
  expect_equal(dim(shape()), NULL)
  expect_equal(dim(shape(NULL)), NULL)
  expect_equal(dim(shape(NA)), NULL)
  expect_equal(dim(shape(dims = NULL)), NULL)
  expect_equal(dim(shape(3, 4)), NULL)
  expect_equal(dim(shape(NA, 4)), NULL)
  expect_equal(dim(shape(dims = c(NA, 4))), NULL)
})


test_that("placeholder and friends behave the same way", {
  skip_if_not(check_tf_version())

  x <- tf$compat$v1$placeholder(tf$float64, list(2, NULL))
  y <- x * 2
  sess <- tf$compat$v1$Session()
  dict <- dict(x = array(1, c(2, 10)))

  expect_snapshot_output(dim(x))
  expect_snapshot_output(length(x))
  expect_snapshot_output(dim(y))
  expect_snapshot_output(length(y))
  expect_snapshot_output(length(dict))
  expect_snapshot_output(dim(dict))
  expect_snapshot_output(length(sess$run(y, feed_dict = dict)))
  expect_snapshot_output(dim(sess$run(y, feed_dict = dict)))

  dict <- dict(x = array(1, c(2, 4)))

  expect_snapshot_output(dim(dict))
  expect_snapshot_output(length(dict))
  expect_snapshot_output(length(sess$run(y, feed_dict = dict)))
  expect_snapshot_output(dim(sess$run(y, feed_dict = dict)))

})

test_that("TensorShape conversions remain stable", {
  skip_if_not(check_tf_version())
  x <- shape(NA, 3)
  expect_snapshot_output(as.list(x))
  expect_snapshot_output(as.integer(x))
  expect_snapshot_output(as_tensor(x))
  expect_snapshot_output(x[[1]])
  expect_snapshot_output(x[[2]])
})

test_that("shape returns appropriate TensorShape object", {
  skip_if_not(check_tf_version())
  expect_snapshot_output(shape())
  expect_snapshot_output(shape(NULL))
  expect_snapshot_output(shape(NA))
  expect_snapshot_output(shape(dims = NULL))
  expect_snapshot_output(shape(3, 4))
  expect_snapshot_output(shape(NA, 4))
  expect_snapshot_output(shape(dims = c(NA, 4)))
  expect_snapshot_output(c(shape(1), 3))
  expect_snapshot_output(length(shape(1)))
  expect_snapshot_output(length(shape(1, 3)))
  expect_snapshot_output(as.integer(shape(1, 3)))
  expect_snapshot_output(as.numeric(shape(1, 3)))
  expect_snapshot_output(as.double(shape(1, 3)))
  expect_snapshot_output(shape(1, 3) == shape(1,3))
  expect_snapshot_output(shape(1, 3) == shape(1,2))
  expect_snapshot_output(shape(1, 3) != shape(1,3))
  expect_snapshot_output(shape(1, 3) != shape(1,2))
})

test_that("tf$reshape behaves as expected", {
  skip_if_not(check_tf_version())
  expect_snapshot_output(
    tf$reshape(tf$zeros(shape(8)), as_tensor(shape(NA, 4)))
    )
})

test_that("[, [[, and assignment returns right object", {
  skip_if_not(check_tf_version())
  x_extract <- shape(1,2,3)
  expect_snapshot_output(x_extract[1])
  expect_snapshot_output(x_extract[[1]])
  expect_snapshot_output(x_extract[2:3])
  expect_snapshot_output(x_extract[-1])
  expect_snapshot_output(x_extract[1] <- 11)
  expect_snapshot_output(x_extract[1] <- shape(11))
  expect_snapshot_output(x_extract[1] <- list(11))
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
