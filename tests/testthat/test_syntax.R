context('syntax')

test_that('`distribution<-` works in models', {

  source('helpers.R')

  flush()

  y <- as_data(randn(5))
  expect_equal(y$node$type, 'data')
  y_op <- y * 1
  expect_equal(y_op$node$type, 'operation')

  # distribution parameter
  # data
  mu <- normal(0, 1)
  distribution(y) = normal(mu, 2)
  sample_distribution(mu)

  # operation
  mu <- normal(0, 1)
  distribution(y_op) = normal(mu, 1)  #!
  sample_distribution(mu)

  # free parameter
  # data
  mu <- free()
  distribution(y) = normal(mu, 2)
  sample_distribution(mu)

  # op
  mu <- free()
  distribution(y_op) = normal(mu, 1)  #!
  sample_distribution(mu)

  # test truncation

})

test_that('distribution() works', {

  source('helpers.R')

  flush()

  a = normal(0, 1)
  b = free()
  c = as_data(randn(5))
  d = c * 1

  # when run on a distribution, should just return the same greta array
  expect_identical(distribution(a), a)  #!

  # when run on somehting without a distribution, should return NULL
  expect_null(distribution(b))
  expect_null(distribution(c))
  expect_null(distribution(d))

  # once assigned, should return the original distribution
  distribution(b) = a  #!
  expect_equal(distribution(b), a)  #!

  distribution(c) = a  #!
  expect_equal(distribution(c), a)

  distribution(d) = a  #!
  expect_equal(distribution(d), a)  #!

})

test_that('`distribution<-` errors informatively', {

  source('helpers.R')

  flush()

  y <- randn(3, 3, 2)
  x <- randn(1)

  # not a stochastic greta array on the right
  expect_error({distribution(y) = x},
               'right hand side of distribution must be a distribution greta array')

  expect_error({distribution(y) = as_data(x)},
               'right hand side of distribution must be a distribution greta array')

  # no density on the right
  expect_error({distribution(y) = free()},
               'right hand side of distribution must be a distribution greta array')

  # non-scalar and wrong dimensions
  expect_error({distribution(y) = normal(0, 1, dim = c(3, 3, 1))},
               '^left- and right-hand side of distribution have different dimensions.')

  # double assignment of distribution to node
  y_ <- as_data(y)
  distribution(y_) = normal(0, 1)
  expect_error({distribution(y_) = normal(0, 1)},
               'greta array already has a distribution')  #!

  # # it works when the new distribution is pre-assigned
  # mu = normal(0, 1)
  # distribution(y_) = mu
  # distribution(y_) = mu

})

test_that('distribution() errors informatively', {

  source('helpers.R')

  flush()

  y <- randn(3)

  expect_error(distribution(y),
               'not a greta array')

  distribution(free())

})

