test_that("print and summary work", {
  skip_if_not(check_tf_version())

  ga_data <- as_data(matrix(1:9, nrow = 3))
  ga_stochastic <- normal(0, 1)
  ga_operation <- ga_data * ga_stochastic

  # data arrays
  # print method
  expect_snapshot(
    ga_data
  )

  # summary method
  expect_snapshot(
    summary(ga_data)
  )
  # stochastic arrays
  # print method
  expect_snapshot(
    ga_stochastic
  )

  # summary method
  expect_snapshot(
    summary(ga_stochastic)
  )

  # operation arrays
  # print method
  expect_snapshot(
    ga_operation
  )

  # summary method
  expect_snapshot(
    summary(ga_operation)
  )

  # assigned arrays (only partly unknown)
  z <- zeros(3, 3)
  z[, 1] <- ones(3)
  z[, 2] <- normal(0, 1, 3)

  expect_snapshot(
    z
  )

  # assigned unknown arrays (only partly unknown)
  n <- normal(0, 1, dim = c(3, 3))^2
  n[, 1] <- ones(3)

  expect_snapshot(
    n
  )

})

test_that("as.matrix works", {
  skip_if_not(check_tf_version())

  # data
  d <- greta_array(0:1, dim = c(3, 3))
  d_mat <- as.matrix(d)
  expect_true(inherits(d_mat, "matrix"))

  # variable
  v <- normal(0, 1, dim = 2)
  v_mat <- as.matrix(v)
  expect_true(inherits(v_mat, "matrix"))

  # operation
  o <- v[1]
  o_mat <- as.matrix(o)
  expect_true(inherits(o_mat, "matrix"))
})

test_that("print method works for longer greta arrays",{
  skip_if_not(check_tf_version())

  ga_data_long <- as_data(matrix(1:20, ncol = 1))
  ga_stochastic_long <- normal(0, 1, dim = 20)
  ga_operation_long <- ga_data_long * ga_stochastic_long

  expect_snapshot(
    ga_data_long
    )
  expect_snapshot(
    ga_stochastic_long
    )
  expect_snapshot(
    ga_operation_long
    )
  expect_snapshot(
    print(ga_data_long, n = 19)
    )
  expect_snapshot(
    print(ga_data_long, n = 20)
    )
  expect_snapshot(
    print(ga_data_long, n = 21)
    )
  expect_snapshot(
    print(ga_stochastic_long, n = 19)
    )
  expect_snapshot(
    print(ga_stochastic_long, n = 20)
    )
  expect_snapshot(
    print(ga_stochastic_long, n = 21)
    )
  expect_snapshot(
    print(ga_operation_long, n = 19)
    )
  expect_snapshot(
    print(ga_operation_long, n = 20)
    )
  expect_snapshot(
    print(ga_operation_long, n = 21)
    )

})
