set.seed(2020 - 02 - 11)

test_that("tensorflow returns appropriate thing with 'dim'", {
  skip_if_not(check_tf_version())
  xt_int_32 <- tensorflow::as_tensor(x = 42, "int32")
  xt_int_64 <- tensorflow::as_tensor(x = 42, "int64")
  xt_float_32 <- tensorflow::as_tensor(x = 42, "float32")
  xt_float_32_dec <- tensorflow::as_tensor(x = 42.2, "float32")

  expect_identical(dim(xt_int_32), integer(0))
  expect_identical(dim(xt_int_64), integer(0))
  expect_identical(dim(xt_float_32), integer(0))
  expect_identical(dim(xt_float_32_dec), integer(0))
  expect_null(dim(shape(1,2,3)))
  expect_identical(dim(tensorflow::as_tensor(c(1:3))), 3L)
})

test_that("Tensor behaves as we expect",{
  skip_if_not(check_tf_version())
  x <- tensorflow::as_tensor(42, "int32")
  expect_snapshot(length(x))
  expect_snapshot(dim(x))
  expect_snapshot(
    tf$reshape(x, tensorflow::as_tensor(shape(-1, 1, 1L, 1, 1L)))
  )
})

test_that("shape returns right thing", {
  skip_if_not(check_tf_version())
  expect_snapshot(shape())
  expect_snapshot(shape(NULL))
  expect_snapshot(shape(NA))
  expect_snapshot(shape(dims = NULL))
  expect_snapshot(shape(3, 4))
  expect_snapshot(shape(NA, 4))
  expect_snapshot(shape(dims = c(NA, 4)))
  expect_snapshot(shape(1,1,1))
  expect_null(dim(shape()))
  expect_null(dim(shape(NULL)))
  expect_null(dim(shape(NA)))
  expect_null(dim(shape(dims = NULL)))
  expect_null(dim(shape(3, 4)))
  expect_null(dim(shape(NA, 4)))
  expect_null(dim(shape(dims = c(NA, 4))))
})

# TF1/2 check todo
# do we care about something equivalent to this?
# test_that("placeholder and friends behave the same way", {
#   skip_if_not(check_tf_version())
#
#   x <- tf$compat$v1$placeholder(tf$float64, list(2, NULL))
#   y <- x * 2
#   sess <- tf$compat$v1$Session()
#   dict <- dict(x = array(1, c(2, 10)))
#
#   expect_snapshot_output(dim(x))
#   expect_snapshot_output(length(x))
#   expect_snapshot_output(dim(y))
#   expect_snapshot_output(length(y))
#   expect_snapshot_output(length(dict))
#   expect_snapshot_output(dim(dict))
#   expect_snapshot_output(length(sess$run(y, feed_dict = dict)))
#   expect_snapshot_output(dim(sess$run(y, feed_dict = dict)))
#
#   dict <- dict(x = array(1, c(2, 4)))
#
#   expect_snapshot_output(dim(dict))
#   expect_snapshot_output(length(dict))
#   expect_snapshot_output(length(sess$run(y, feed_dict = dict)))
#   expect_snapshot_output(dim(sess$run(y, feed_dict = dict)))
#
# })

test_that("TensorShape conversions remain stable", {
  skip_if_not(check_tf_version())
  x <- shape(NA, 3)
  expect_snapshot(as.list(x))
  expect_snapshot(as.integer(x))
  expect_snapshot(tensorflow::as_tensor(x))
  expect_snapshot(x[[1]])
  expect_snapshot(x[[2]])
})

test_that("shape returns appropriate TensorShape object", {
  skip_if_not(check_tf_version())
  expect_snapshot(shape())
  expect_snapshot(shape(NULL))
  expect_snapshot(shape(NA))
  expect_snapshot(shape(dims = NULL))
  expect_snapshot(shape(3, 4))
  expect_snapshot(shape(NA, 4))
  expect_snapshot(shape(dims = c(NA, 4)))
  expect_snapshot(c(shape(1), 3))
  expect_snapshot(length(shape(1)))
  expect_snapshot(length(shape(1, 3)))
  expect_snapshot(as.integer(shape(1, 3)))
  expect_snapshot(as.numeric(shape(1, 3)))
  expect_snapshot(as.double(shape(1, 3)))
  expect_snapshot(shape(1, 3) == shape(1,3))
  expect_snapshot(shape(1, 3) == shape(1,2))
  expect_snapshot(shape(1, 3) != shape(1,3))
  expect_snapshot(shape(1, 3) != shape(1,2))
})

test_that("[, [[, and assignment returns right object", {
  skip_if_not(check_tf_version())
  x_extract <- shape(1,2,3)
  expect_snapshot(x_extract[1])
  expect_snapshot(x_extract[[1]])
  expect_snapshot(x_extract[2:3])
  expect_snapshot(x_extract[-1])
  expect_snapshot({
    x_extract[1] <- 11
    x_extract[1]
    })
  expect_snapshot({
    x_extract[1] <- shape(11)
    x_extract[1]
    })
  expect_snapshot({
    x_extract[1] <- list(11)
    x_extract[1]
    })
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
