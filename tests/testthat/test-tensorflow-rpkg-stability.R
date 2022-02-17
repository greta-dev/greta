xt_int_32 <- as_tensor(x = 42, "int32")
xt_int_64 <- as_tensor(x = 42, "int64")
xt_float_32 <- as_tensor(x = 42, "float32")
xt_float_32_dec <- as_tensor(x = 42.2, "float32")

x <- shape(NA, 3)

test_that("tensorflow returns appropriate thing with 'dim'", {
  expect_equal(dim(xt_int_32), integer(0))
  expect_equal(dim(xt_int_64), integer(0))
  expect_equal(dim(xt_float_32), integer(0))
  expect_equal(dim(xt_float_32_dec), integer(0))
  expect_equal(dim(shape(1,2,3)), NULL)
  expect_equal(dim(as_tensor(c(1:3))), 3)
})

test_that("shape returns right thing", {
  expect_snapshot_output(shape())
  expect_snapshot_output(shape(NULL))
  expect_snapshot_output(shape(NA))
  expect_snapshot_output(shape(dims = NULL))
  expect_snapshot_output(shape(3, 4))
  expect_snapshot_output(shape(NA, 4))
  expect_snapshot_output(shape(dims = c(NA, 4)))
  expect_equal(dim(shape()), NULL)
  expect_equal(dim(shape(NULL)), NULL)
  expect_equal(dim(shape(NA)), NULL)
  expect_equal(dim(shape(dims = NULL)), NULL)
  expect_equal(dim(shape(3, 4)), NULL)
  expect_equal(dim(shape(NA, 4)), NULL)
  expect_equal(dim(shape(dims = c(NA, 4))), NULL)
})


test_that("placeholder and friends behave the same way", {

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


# test_that("tensorflow returns right thing with 'length'", {
#
# })


test_that("TensorShape conversions remain stable", {
  expect_snapshot_output(as.list(x))
  expect_snapshot_output(as.integer(x))
  expect_snapshot_output(as_tensor(x))
  expect_snapshot_output(x[[1]])
  expect_snapshot_output(x[[2]])
})


test_that("shape returns appropriate TensorShape object", {
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
  expect_snapshot_output(
    tf$reshape(tf$zeros(shape(8)), as_tensor(shape(NA, 4)))
    )
})

test_that("[, [[, and assignment returns right object", {
  x_extract <- shape(1,2,3)
  expect_snapshot_output(x_extract[1])
  expect_snapshot_output(x_extract[[1]])
  expect_snapshot_output(x_extract[2:3])
  expect_snapshot_output(x_extract[-1])
  expect_snapshot_output(x_extract[1] <- 11)
  expect_snapshot_output(x_extract[1] <- shape(11))
  expect_snapshot_output(x_extract[1] <- list(11))
})

