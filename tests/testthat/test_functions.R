context('functions')

test_that('simple functions work as expected', {

  source('helpers.R')

  x <- randn(25, 4)

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
