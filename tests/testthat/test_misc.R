context('miscellaneous methods')

test_that('check_tf_version works', {

  skip_if_not(check_tf_version())

  # record the true version and forge an old version
  true_version <- tf$`__version__`
  tf$`__version__` <- "0.9.0"

  # expected text
  expected_message <- "you have version 0.9.0"

  expect_error(check_tf_version('error'),
               expected_message)
  expect_warning(check_tf_version('warn'),
                 expected_message)
  expect_message(check_tf_version('message'),
                 expected_message)

  # reset the true version
  tf$`__version__` <- true_version

  # forge a missing installation
  expected_message <- "isn't installed"

  with_mock(
    `reticulate::py_module_available` = function(x) {FALSE},
    expect_error(check_tf_version('error'), expected_message),
    expect_warning(check_tf_version('warn'), expected_message),
    expect_message(check_tf_version('message'), expected_message)
  )

})

test_that('.onLoad runs', {

  greta:::.onLoad()

})

test_that('tensorflow coercion works', {

  skip_if_not(check_tf_version())

  float <- greta:::tf_as_float(1)
  integer <- greta:::tf_as_integer(1)
  logical <- greta:::tf_as_logical(1)

  float_type <- options()$greta_tf_float$name
  expect_equal(float$dtype$name, float_type)
  expect_equal(integer$dtype$name, 'int32')
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

test_that('greta_model objects print', {

  skip_if_not(check_tf_version())

  m <- model(normal(0, 1))
  message <- capture.output(print(m))
  expect_equal(message, 'greta model')

})

test_that('define and mcmc error informatively', {

  skip_if_not(check_tf_version())
  source('helpers.R')

  x <- as_data(randn(10))

  # no model with non-probability density greta arrays
  expect_error(model(variable()),
               'none of the greta arrays in the model are associated with a probability density, so a model cannot be defined')

  expect_error(model(x),
               'none of the greta arrays in the model are associated with a probability density, so a model cannot be defined')

  expect_error(model(),
               'could not find any non-data greta arrays')

  # can't define a model for an unfixed discrete variable
  expect_error(model(bernoulli(0.5)),
               "model contains a discrete random variable that doesn't have a fixed value, so cannot be sampled from")

  # no parameters here, so define or dag should error
  distribution(x) = normal(0, 1)
  expect_error(model(x),
               'none of the greta arrays in the model are unknown, so a model cannot be defined')

  # a bad number of cores
  a = normal(0, 1)
  expect_warning(model(a, n_cores = 1000000L),
               'cores were requested, but only')

  # can't draw samples of a data greta array
  z = normal(x, 1)
  m <- model(x, z)
  expect_error(mcmc(m),
               'x is a data greta array, data greta arrays cannot be sampled')

})

test_that('check_dims errors informatively', {

  source('helpers.R')

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

  # with two scalars and a target dimension, just return the target dimension
  expect_equal(greta:::check_dims(b, b, target_dim = dim1),
               dim1)

})

test_that('disjoint graphs are checked', {

  skip_if_not(check_tf_version())
  source('helpers.R')

  # if the target nodes aren't related, they sould be checked separately

  a = uniform(0, 1)
  b = normal(a, 2)

  # c is unrelated and has no density
  c = variable()

  expect_error(m <- model(a, b, c),
               'the model contains 2 disjoint graphs, one or more of these sub-graphs does not contain any greta arrays that are associated with a probability density, so a model cannot be defined')

  # d is unrelated and known
  d = as_data(randn(3))
  distribution(d) = normal(0, 1)
  expect_error(m <- model(a, b, d),
               'the model contains 2 disjoint graphs, one or more of these sub-graphs does not contain any greta arrays that are unknown, so a model cannot be defined')


})

test_that("plotting models doesn't error", {

  skip_if_not(check_tf_version())
  source('helpers.R')

  a = uniform(0, 1)

  m <- model(a)

  plot(m)

})

test_that("structures work correctly", {

  skip_if_not(check_tf_version())
  source('helpers.R')

  a <- ones(2, 2)
  b <- zeros(2)
  c <- greta_array(3, dim = c(2, 2, 2))

  expect_identical(grab(a), array(1, dim = c(2, 2)))
  expect_identical(grab(b), array(0, dim = c(2, 1)))
  expect_identical(grab(c), array(3, dim = c(2, 2, 2)))

})

test_that("cleanly() handles TF errors nicely", {

  source('helpers.R')

  inversion_stop <- function ()
    stop ("this non-invertible thing is not invertible")

  cholesky_stop <- function ()
    stop ("Cholesky decomposition was not successful")

  other_stop <- function ()
    stop ("Fetchez la vache!")

  expect_equal(cleanly(inversion_stop()), NA)
  expect_equal(cleanly(cholesky_stop()), NA)
  expect_error(cleanly(other_stop()),
               "greta hit a tensorflow error:")

})
