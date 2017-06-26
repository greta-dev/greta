context('syntax')

test_that('`distribution<-` works in models', {

  skip_if_not(check_tf_version())
  source('helpers.R')

  # with a distribution parameter
  y <- as_data(randn(5))
  expect_equal(node_type(y$node), 'data')

  # data
  mu <- normal(0, 1)
  distribution(y) = normal(mu, 2)
  sample_distribution(mu)

})

test_that('distribution() works', {

  source('helpers.R')

  a = normal(0, 1)
  x = as_data(randn(5))

  # when run on a distribution, should just return the same greta array
  expect_identical(distribution(a), a)

  # when run on something without a distribution, should return NULL
  expect_null(distribution(x))

  # once assigned, should return the original distribution
  a2 = normal(0, 1)
  distribution(x) = a2
  expect_equal(distribution(x), x)

})

test_that('`distribution<-` errors informatively', {

  source('helpers.R')

  y <- randn(3, 3, 2)
  x <- randn(1)

  # not a greta array with a distribution on the right
  expect_error({distribution(y) = x},
               'right hand side must be a greta array')

  expect_error({distribution(y) = as_data(x)},
               'right hand side must have a distribution')

  # no density on the right
  expect_error({distribution(y) = variable()},
               'right hand side must have a distribution')

  # non-scalar and wrong dimensions
  expect_error({distribution(y) = normal(0, 1, dim = c(3, 3, 1))},
               '^left and right hand sides have different dimensions.')

  # double assignment of distribution to node
  y_ <- as_data(y)
  distribution(y_) = normal(0, 1)
  expect_error({distribution(y_) = normal(0, 1)},
               'left hand side already has a distribution assigned')

  # assignment with a greta array that already has a fixed value
  y1 <- as_data(y)
  y2 <- as_data(y)
  d <- normal(0, 1)
  distribution(y1) = d
  expect_error({distribution(y2) = d},
               'right hand side has already been assigned fixed values')

  # assignment to a variable
  z = variable()
  expect_error({distribution(z) = normal(0, 1)},
               'distributions can only be assigned to data greta arrays')

  # assignment to an op
  z2 = z ^ 2
  expect_error({distribution(z2) = normal(0, 1)},
               'distributions can only be assigned to data greta arrays')

  # assignment to another distribution
  u = uniform(0, 1)
  expect_error({distribution(z2) = normal(0, 1)},
               'distributions can only be assigned to data greta arrays')

})

test_that('distribution() errors informatively', {

  source('helpers.R')

  y <- randn(3)

  expect_error(distribution(y),
               'not a greta array')


})

