context('operators')

test_that('arithmetic operators work as expected', {

  skip_if_not(check_tf_version())
  source('helpers.R')

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

test_that('logical operators work as expected', {

  skip_if_not(check_tf_version())
  source('helpers.R')

  a <- randn(25, 4) > 0
  b <- randn(25, 4) > 0
  a[] <- as.integer(a[])
  b[] <- as.integer(b[])

  check_op(`!`, a)
  check_op(`&`, a, b)
  check_op(`|`, a, b)

})

test_that('relational operators work as expected', {

  skip_if_not(check_tf_version())
  source('helpers.R')

  a <- randn(25, 4)
  b <- randn(25, 4)

  check_op(`<`, a, b)
  check_op(`>`, a, b)
  check_op(`<=`, a, b)
  check_op(`>=`, a, b)
  check_op(`==`, a, b)
  check_op(`!=`, a, b)

})

test_that('random strings of operators work as expected', {

  skip_if_not(check_tf_version())
  source('helpers.R')

  for (i in 1:10) {

    a <- randn(25, 4)
    b <- randn(25, 4)

    # generate a 5-deep random function of operations
    fun <- gen_opfun(5,
                     ops = c('+', '-', '*',
                             '/', '%/%', '&',
                             '|', '<', '>'))

    r_out <- fun(a, b)
    greta_out <- grab(fun(as_data(a), as_data(b)))

    # nas should be in the same places
    na_idx <- which(is.na(r_out))
    expect_identical(which(is.na(greta_out)),
                     na_idx)

    # the values can get quite extreme, so scale differences, omit NAs before
    # and after doing scaling, tolerate a 1% variation
    difference <- abs(r_out - greta_out) / abs(greta_out)
    difference <- na.omit(difference[-na_idx])
    expect_true(all(difference < 1e-2))

  }

})

test_that('%*% errors informatively', {

  a <- ones(3, 4)
  b <- ones(1, 4)

  expect_error(a %*% b,
               'incompatible dimensions: 3x4 vs 1x4')

})
