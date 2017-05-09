context('miscellaneous methods')

test_that('check_tf_version works', {

  # record the true version and forge an old version
  true_version <- tf$`__version__`
  tf$`__version__` <- "0.9.0"

  # expected text
  expected_message <-
    paste0("\n\n  greta requires TensorFlow version 1.0.0 or higher, but you ",
           "have version 0.9.0\n  You can write models, but not sample from ",
           "them.\n  See https://www.tensorflow.org/install for installation ",
           "instructions.\n\n")

  expect_error(greta:::check_tf_version('error'),
               expected_message)
  expect_warning(greta:::check_tf_version('warn'),
                 expected_message)
  expect_message(greta:::check_tf_version('message'),
                 expected_message)

  # reset the true version
  tf$`__version__` <- true_version

})

test_that('logo runs', {

  # use an available font
  greta:::banner(font = 'sans')
  expect_warning(greta:::banner(width = 2, font = 'sans'),
                 '^with a margin of 0.2 the minimum width to ensure the banner is height-filled is')
  greta:::blank_banner()

})

test_that('.onLoad runs', {

  greta:::.onLoad()

})

test_that('tensorflow coercion work', {

  float <- greta:::tf_as_float(1)
  integer <- greta:::tf_as_integer(1)
  logical <- greta:::tf_as_logical(1)

  expect_equal(float$dtype$name, 'float32')
  expect_equal(integer$dtype$name, 'int64')
  expect_equal(logical$dtype$name, 'bool')

})

test_that('all_greta_arrays works', {

  env <- new.env()

  env$a = normal(0, 1)
  env$b = as_data(rnorm(10))
  env$c = env$a * env$b

  array_list <- greta:::all_greta_arrays(env)
  array_list_nodata <- greta:::all_greta_arrays(env, include_data = FALSE)

  expect_identical(names(array_list), c('a', 'b', 'c'))
  expect_identical(names(array_list_nodata), c('a', 'c'))

})

test_that('distribution errors informatively', {

  source('helpers.R')

  flush()

    y <- randn(3, 3, 2)
  x <- randn(1)

  # not a stochastic greta array on the right
  expect_error({distribution(y) = x},
               'right hand side of distribution must be a stochastic greta array')

  expect_error({distribution(y) = as_data(x)},
               'right hand side of distribution must be a stochastic greta array')

  # no density on the right
  expect_error({distribution(y) = free()},
               'free parameters do not have distributions, so cannot be used to define a distribution')

  # non-scalar and wrong dimensions
  expect_error({distribution(y) = normal(0, 1, dim = c(3, 3, 1))},
               '^left- and right-hand side of distribution have different dimensions.')

})

test_that('greta_model objects print', {

  m <- define_model(normal(0, 1))
  message <- capture.output(print(m))
  expect_equal(message, 'greta model')

})

test_that('greta_model objects print', {

  m <- define_model(normal(0, 1))
  message <- capture.output(print(m))
  expect_equal(message, 'greta model')

})

test_that('define_model and mcmc error informatively', {

  source('helpers.R')

  flush()

  x <- as_data(randn(10))

  # no model with non-probability density greta arrays
  expect_error(define_model(free()),
               'none of the greta arrays in the model are associated with a probability density, so a model cannot be defined')

  expect_error(define_model(x),
               'none of the greta arrays in the model are associated with a probability density, so a model cannot be defined')

  expect_error(define_model(),
               'could not find any non-data greta arrays')

  # can't define a model for an unfixed discrete variable
  expect_error(define_model(bernoulli(0.5)),
               "model contains a discrete random variable that doesn't have a fixed value, so cannot be sampled from")

  # can't draw samples of a data greta array
  z = normal(0, 1)
  m <- define_model(x, z)
  expect_error(mcmc(m),
               'x is a data greta array, data greta arrays cannot be sampled')

})

test_that('check_dims errors informatively', {

  source('helpers.R')

  flush()

  a <- ones(3, 3)
  b <- ones(1)
  c <- ones(2, 2)
  d <- ones(2, 2, 2)
  dim1 <- c(3, 3)

  # with one scalar, it should always should work
  expect_equal(greta:::check_dims(a, b),
               dim(a))

  # but not when target_dim is set
  expect_error(greta:::check_dims(a, b, target_dim = dim1),
               'array dimensions should be 3x3, but input dimensions were 3x3, 1x1')

  # with both scalar, it should always should work
  expect_equal(greta:::check_dims(b, b),
               dim(b))

  # with two differently shaped arrays it shouldn't
  expect_error(greta:::check_dims(a, c),
               'incompatible dimensions: 3x3, 2x2')

  # with two scalars and a target dimenssion, just return the target dimension
  expect_equal(greta:::check_dims(b, b, target_dim = dim1),
               dim1)

})

test_that('rejected mcmc proposals', {

  source('helpers.R')

  flush()

  # numerical rejection
  x <- rnorm(10000, 1e6, 1)
  z = normal(-1e6, 1e-6)
  distribution(x) = normal(z, 1e6)
  m <- define_model(z)
  expect_message(mcmc(m, n_samples = 1, warmup = 0),
                 'proposal rejected due to numerical instability')

  flush()

  # bad proposal
  x <- rnorm(100, 0, 0.01)
  z = normal(0, 10)
  distribution(x) = normal(z, 0.01)
  m <- define_model(z)
  mcmc(m, n_samples = 1, warmup = 0)

})
