test_that("arithmetic operators work as expected", {
  skip_if_not(check_tf_version())

  a <- randn(25, 4)
  b <- randn(25, 4)

  check_op(`-`, a)
  check_op(`-`, a, b)
  check_op(`+`, a, b)
  check_op(`*`, a, b)
  check_op(`/`, a, b)
  check_op(`^`, abs(a), b)
  check_op(`%%`, a, b)
  check_op(`%/%`, a, b)
  check_op(`%*%`, a, t(b))
})

test_that("arithmetic operators work as expected with arrays and scalars", {
  skip_if_not(check_tf_version())

  a <- randn(2, 1, 1)
  b <- 4

  check_op(`-`, a, b)
  check_op(`+`, a, b)
  check_op(`*`, a, b)
  check_op(`/`, a, b)
  check_op(`^`, abs(a), b)
  check_op(`%%`, a, b)
  check_op(`%/%`, a, b)
})

test_that("logical operators work as expected", {
  skip_if_not(check_tf_version())

  a <- randn(25, 4) > 0
  b <- randn(25, 4) > 0
  a[] <- as.integer(a[])
  b[] <- as.integer(b[])

  check_op(`!`, a, only = "data")
  check_op(`&`, a, b, only = "data")
  check_op(`|`, a, b, only = "data")
})

test_that("relational operators work as expected", {
  skip_if_not(check_tf_version())

  a <- randn(25, 4)
  b <- randn(25, 4)

  check_op(`<`, a, b)
  check_op(`>`, a, b)
  check_op(`<=`, a, b)
  check_op(`>=`, a, b)
  check_op(`==`, a, b)
  check_op(`!=`, a, b)
})

test_that("random strings of operators work as expected", {
  skip_if_not(check_tf_version())

  for (i in 1:10) {
    a <- randn(25, 4)
    b <- randn(25, 4)

    # generate a 5-deep random function of operations
    fun <- gen_opfun(5,
      ops = c(
        "+", "-", "*",
        "/", "&",
        "|", "<", ">"
      )
    )

    r_out <- fun(a, b)
    greta_out <- grab(fun(as_data(a), as_data(b)))

    # nas should be in the same places
    na_idx <- which(is.na(r_out))
    expect_identical(
      which(is.na(greta_out)),
      na_idx
    )

    # the values can get quite extreme, so scale differences, omit NAs before
    # and after doing scaling, tolerate a 1% variation
    difference <- abs(r_out - greta_out) / abs(greta_out)
    difference <- na.omit(difference[-na_idx])
    expect_true(all(difference < 1e-2))
  }
})

test_that("%*% errors informatively", {
  skip_if_not(check_tf_version())


  a <- ones(3, 4)
  b <- ones(1, 4)
  c <- ones(2, 2, 2)

  expect_snapshot(error = TRUE,
    a %*% b
  )

  expect_snapshot(error = TRUE,
    a %*% c
  )
})

test_that("%*% works when one is a non-greta array", {
  skip_if_not(check_tf_version())
  x <- matrix(1, 2, 3)
  y <- rep(1, 3)

  expect_snapshot(x %*% y)
  expect_snapshot(x %*% as_data(y))
  expect_snapshot(as_data(x) %*% y)
  expect_snapshot(as_data(x) %*% as_data(y))

  dim1 <- dim(x %*% as_data(y))
  dim2 <- dim(as_data(x) %*% y)
  dim3 <- dim(as_data(x) %*% as_data(y))

  expect_true(all(dim1 == dim2 & dim1 == dim3 & dim2 == dim3))

  res_1 <- x %*% as_data(y)
  res_2 <- as_data(x) %*% y
  res_3 <- as_data(x) %*% as_data(y)

  expect_snapshot(calculate(res_1, nsim = 1))
  expect_snapshot(calculate(res_2, nsim = 1))
  expect_snapshot(calculate(res_3, nsim = 1))
})
