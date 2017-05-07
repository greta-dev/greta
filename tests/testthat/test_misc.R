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

test_that('notimplemented errors', {

  expect_error(greta:::notimplemented(),
               'method not yet implemented')

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

test_that('likelihood errors informatively', {

  source('helpers.R')

  y <- randn(3, 3, 2)
  x <- randn(1)

  # not a stochastic greta array on the right
  expect_error({likelihood(y) = x},
               'right hand side of likelihood must be a stochastic greta array')

  expect_error({likelihood(y) = as_data(x)},
               'right hand side of likelihood must be a stochastic greta array')

  # no density on the right
  expect_error({likelihood(y) = free()},
               'free parameters do not have distributions, so cannot be used to define a likelihood')

  # non-scalar and wrong dimensions
  expect_error({likelihood(y) = normal(0, 1, dim = c(3, 3, 1))},
               '^left- and right-hand side of likelihood have different dimensions.')

})
