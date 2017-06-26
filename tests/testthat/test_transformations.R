context('transformations')

test_that('transformations work as expected', {

  skip_if_not(check_tf_version())
  source('helpers.R')

  a <- randn(25, 4)

  r_icloglog <- function (x)
    1 - exp(-1 * exp(x))

  r_log1pe <- function (x)
    log1p(exp(x))

  check_op(pnorm, a, greta_op = iprobit)
  check_op(plogis, a, greta_op = ilogit)
  check_op(r_icloglog, a, greta_op = icloglog)
  check_op(pcauchy, a, greta_op = icauchit)
  check_op(r_log1pe, a, greta_op = log1pe)

})

