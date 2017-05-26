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

test_that('.onLoad runs', {

  greta:::.onLoad()

})

test_that('tensorflow coercion works', {

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

  m <- model(normal(0, 1))
  message <- capture.output(print(m))
  expect_equal(message, 'greta model')

})

test_that('define and mcmc error informatively', {

  source('helpers.R')

  x <- as_data(randn(10))

  # no model with non-probability density greta arrays
  expect_error(model(free()),
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

test_that('rejected mcmc proposals', {

  source('helpers.R')

  # set up for numerical rejection of initial location
  x <- rnorm(10000, 1e6, 1)
  z = normal(-1e6, 1e-6)
  distribution(x) = normal(z, 1e6)
  m <- model(z)

  with_mock(
    `greta:::create_progress_bar` = mock_create_progress_bar,
    m <- model(z),
    out <- capture_output(mcmc(m, n_samples = 1, warmup = 0)),
    expect_match(out, '100% bad')
  )

  # bad proposal
  x <- rnorm(100, 0, 0.01)
  z = normal(0, 10)
  distribution(x) = normal(z, 0.01)
  m <- model(z)
  mcmc(m, n_samples = 1, warmup = 0, verbose = FALSE)

})

test_that('disjoint graphs are checked', {

  source('helpers.R')

  # if the target nodes aren't related, they sould be checked separately

  a = uniform(0, 1)
  b = normal(a, 2)

  # c is unrelated and has no density
  c = free()

  expect_error(m <- model(a, b, c),
               'the model contains 2 disjoint graphs, one or more of these sub-graphs does not contain any greta arrays that are associated with a probability density, so a model cannot be defined')

  # d is unrelated and known
  d = as_data(randn(3))
  distribution(d) = normal(0, 1)
  expect_error(m <- model(a, b, d),
               'the model contains 2 disjoint graphs, one or more of these sub-graphs does not contain any greta arrays that are unknown, so a model cannot be defined')


})

test_that("plotting models doesn't error", {

  source('helpers.R')

  a = uniform(0, 1)

  m <- model(a)

  plot(m)

})


test_that("structures work correctly", {

  source('helpers.R')

  a <- ones(2, 2)
  b <- zeros(2)
  c <- greta_array(3, dim = c(2, 2, 2))

  expect_identical(grab(a), array(1, dim = c(2, 2)))
  expect_identical(grab(b), array(0, dim = c(2, 1)))
  expect_identical(grab(c), array(3, dim = c(2, 2, 2)))

})

test_that('mcmc works with verbosity and warmup', {

  x <- rnorm(10)
  z = normal(0, 1)
  distribution(x) = normal(z, 1)
  m <- model(z)
  mcmc(m, n_samples = 5, warmup = 5, verbose = TRUE)

})

test_that('progress bar gives a range of messages', {

  source('helpers.R')

  # 1/101 should be <1%
  with_mock(
    `greta:::create_progress_bar` = mock_create_progress_bar,
    `greta:::mcmc` = mock_mcmc,
    out <- capture_output(mcmc(101)),
    expect_match(out, '<1% bad')
  )

  # 1/50 should be 50%
  with_mock(
    `greta:::create_progress_bar` = mock_create_progress_bar,
    `greta:::mcmc` = mock_mcmc,
    out <- capture_output(mcmc(50)),
    expect_match(out, '2% bad')
  )

  # 1/1 should be 100%
  with_mock(
    `greta:::create_progress_bar` = mock_create_progress_bar,
    `greta:::mcmc` = mock_mcmc,
    out <- capture_output(mcmc(1)),
    expect_match(out, '100% bad')
  )

})


test_that('stashed_samples works', {

  source('helpers.R')

  # set up model
  a <- normal(0, 1)
  m <- model(a)

  draws <- mcmc(m, warmup = 10, n_samples = 10, verbose = FALSE)

  # with a completed sample, this should be NULL
  ans <- stashed_samples()
  expect_null(ans)

  # mock up a stash
  stash <- greta:::greta_stash
  assign('trace_stash', as.matrix(rnorm(17)), envir = stash)

  # should convert to an mcmc.list
  ans <- stashed_samples()
  expect_s3_class(ans, 'mcmc.list')

})
