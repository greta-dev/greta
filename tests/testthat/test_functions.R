context('functions')

test_that('simple functions work as expected', {

  skip_if_not(check_tf_version())
  source('helpers.R')

  x <- randn(25, 4)
  n <- 10
  k <- 5

  # logarithms and exponentials
  check_op(log, exp(x))
  check_op(exp, x)
  check_op(log1p, exp(x))
  check_op(expm1, x)

  # miscellaneous mathematics
  check_op(abs, x)
  check_op(mean, x)
  check_op(sqrt, exp(x))
  check_op(sign, x)

  # rounding of numbers
  check_op(ceiling, x)
  check_op(floor, x)
  check_op(round, x)

  # trigonometry
  check_op(cos, x)
  check_op(sin, x)
  check_op(tan, x * 0.5)
  check_op(acos, 2 * plogis(x) - 1)
  check_op(asin, 2 * plogis(x) - 1)
  check_op(atan, x)

  # special mathematical functions
  check_op(lgamma, x)
  check_op(digamma,x)

})

test_that('matrix functions work as expected', {

  skip_if_not(check_tf_version())
  source('helpers.R')

  a <- rWishart(1, 6, diag(5))[, , 1]
  b <- randn(5, 25)

  check_op(t, b)
  check_op(chol, a)
  check_op(diag, a)
  check_op(`diag<-`, a, 1:5)
  check_op(solve, a)
  check_op(solve, a, b)

})

test_that('reducing functions work as expected', {

  skip_if_not(check_tf_version())
  source('helpers.R')

  a <- randn(1, 3)
  b <- randn(5, 25)

  check_op(sum, a, b)
  check_op(prod, a, b)
  check_op(min, a, b)
  check_op(max, a, b)

})

test_that('sweep works as expected', {

  skip_if_not(check_tf_version())
  source('helpers.R')

  stats_list <- list(randn(5), randn(25))
  x <- randn(5, 25)

  for (dim in c(1, 2)) {
    for (fun in c('-', '+', '/', '*')) {

      stats <- stats_list[[dim]]

      r_out <- sweep(x, dim, stats, FUN = fun)

      greta_array <- sweep(as_data(x), dim, as_data(stats), FUN = fun)
      greta_out <- grab(greta_array)

      difference <- as.vector(abs(r_out - greta_out))
      expect_true(all(difference < 1e-4))

    }

  }

})


test_that('solve and sweep error as expected', {

  source('helpers.R')

  a <- as_data(randn(5, 25))
  b <- as_data(randn(5, 25, 2))
  c <- as_data(randn(5, 5))
  stats <- as_data(randn(5))

  # solve

  # a must be 2D
  expect_error(solve(b, a),
               "'a' and 'b' must both be 2D, but 'a' has dimensions:")

  # b must also be 2D
  expect_error(solve(c, b),
               "'a' and 'b' must both be 2D, but 'b' has dimensions:")

  # only square matrices allowed for first element
  expect_error(solve(a, a),
               "^'a' must be square, but has")
  expect_error(solve(a),
               "^'a' must be square, but has")

  # dimension of second array must match
  expect_error(solve(c, t(a)),
               "^'b' must have the same number of rows as 'a'")

  # sweep
  # x must be 2D
  expect_error(sweep(b, 1, stats),
               '^x must be a 2D array, but has')

  # dim must be either 1 or 2
  expect_error(sweep(a, 3, stats),
               'MARGIN can only be 1 or 2')

  # stats must have the correct number of elements
  expect_error(sweep(a, 1, c(stats, stats)),
               '^the number of elements of STATS does not match')

  # stats must be a column vector
  expect_error(sweep(a, 1, t(stats)),
               '^STATS must be a column vector array, but has dimensions')

  expect_error(sweep(a, 2, stats),
               '^the number of elements of STATS does not match')

})
