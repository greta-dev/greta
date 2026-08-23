test_that("calculate uses the local RNG seed", {
  skip_if_not(check_tf_version())

  # fix variable
  a <- normal(0, 1)
  y <- normal(a, 1)

  # the global RNG seed should change if the seed is *not* specified
  before <- rng_seed()
  sims <- calculate(y, nsim = 1)
  after <- rng_seed()
  expect_false(identical(before, after))

  # the global RNG seed should not change if the seed *is* specified
  before <- rng_seed()
  sims <- calculate(y, nsim = 1, seed = 12345)
  after <- rng_seed()
  expect_identical(before, after)

  # the samples should differ if the seed is *not* specified
  one <- calculate(y, nsim = 1)
  two <- calculate(y, nsim = 1)
  expect_false(identical(one, two))

  # the samples should differ if the seeds are specified differently
  one <- calculate(y, nsim = 1, seed = 12345)
  two <- calculate(y, nsim = 1, seed = 54321)
  expect_false(identical(one, two))

  # the samples should be the same if the seed is the same
  one <- calculate(y, nsim = 1, seed = 12345)
  two <- calculate(y, nsim = 1, seed = 12345)
  expect_identical(one, two)
})

test_that("when calculate simulates multiple values, they are calculated using the same RNG seeds", {
  skip_if_not(check_tf_version())

  x <- normal(0, 1)
  x_2 <- x * 1

  vals <- calculate(x, x_2, nsim = 10)
  expect_identical(vals$x, vals$x_2)
})

test_that("calculate produces the right number of samples", {
  skip_if_not(check_tf_version())

  # fix variable
  a <- normal(0, 1)
  y <- normal(a, 1, dim = c(1, 3))

  # the global RNG seed should not change if the seed *is* specified
  before <- rng_seed()
  sims <- calculate(y, nsim = 1, seed = 12345)
  after <- rng_seed()
  expect_identical(before, after)

  # the samples should differ if the seed is *not* specified
  one <- calculate(y, nsim = 1)
  two <- calculate(y, nsim = 1)
  expect_false(identical(one, two))

  # the samples should differ if the seeds are specified differently
  one <- calculate(y, nsim = 1, seed = 12345)
  two <- calculate(y, nsim = 1, seed = 54321)
  expect_false(identical(one, two))

  # the samples should be the same if the seed is the same
  one <- calculate(y, nsim = 1, seed = 12345)
  two <- calculate(y, nsim = 1, seed = 12345)
  expect_identical(one, two)
})


test_that("calculate samples are the same when the argument seed is the same", {
  skip_if_not(check_tf_version())
  a <- normal(0, 1)
  y <- normal(a, 1)
  m <- model(y)

  c_one <- calculate(y, nsim = 1, seed = 12345)
  c_two <- calculate(y, nsim = 1, seed = 12345)

  expect_identical(as.numeric(c_one), as.numeric(c_two))
})

test_that("calculate samples are the same when the R seed is the same", {
  skip_if_not(check_tf_version())
  a <- normal(0, 1)
  y <- normal(a, 1)
  m <- model(y)

  set.seed(12345)
  c_one <- calculate(y, nsim = 1)

  set.seed(12345)
  c_two <- calculate(y, nsim = 1)

  expect_identical(as.numeric(c_one), as.numeric(c_two))
})

test_that("mcmc samples are the same when the R seed is the same, also with tf set seed", {
  skip_if_not(check_tf_version())
  a <- normal(0, 1)
  y <- normal(a, 1)
  m <- model(y)

  set.seed(12345)
  one <- mcmc(m, warmup = 10, n_samples = 1, chains = 1)
  set.seed(12345)
  two <- mcmc(m, warmup = 10, n_samples = 1, chains = 1)

  expect_identical(as.numeric(one), as.numeric(two))

  tensorflow::set_random_seed(12345)
  one_tf <- mcmc(m, warmup = 10, n_samples = 1, chains = 1)
  tensorflow::set_random_seed(12345)
  two_tf <- mcmc(m, warmup = 10, n_samples = 1, chains = 1)

  expect_identical(as.numeric(one_tf), as.numeric(two_tf))

  # and the two routes agree, as ?mcmc promises: set_random_seed() calls
  # set.seed() internally, so both paths hand mcmc() the same seed. Before
  # greta seeded TensorFlow at all, these differed, and this test asserted
  # that they must - see greta-dev/greta#285
  expect_identical(as.numeric(one), as.numeric(one_tf))

  expect_identical(as.numeric(two), as.numeric(two_tf))
})

test_that("mcmc seeds TensorFlow from set.seed(), not from session state", {
  skip_if_not(check_tf_version())
  a <- normal(0, 1)
  y <- normal(a, 1)
  m <- model(y)

  set.seed(12345)
  one <- mcmc(m, warmup = 10, n_samples = 1, chains = 1, verbose = FALSE)

  # perturb TensorFlow's global seed between the two runs. mcmc() draws its own
  # seed from R's RNG, so if it passed that to TensorFlow this would be
  # overwritten and the draws would still match. The test above this one only
  # passes because an earlier calculate() call in this file has already set the
  # TensorFlow seed as a side effect - it would fail run on its own.
  tensorflow::tf$random$set_seed(999L)

  set.seed(12345)
  two <- mcmc(m, warmup = 10, n_samples = 1, chains = 1, verbose = FALSE)

  expect_identical(as.numeric(one), as.numeric(two))
})

test_that("simulate uses the local RNG seed", {
  skip_if_not(check_tf_version())

  # fix variable
  a <- normal(0, 1)
  y <- normal(a, 1)
  m <- model(y)

  # the global RNG seed should change if the seed is *not* specified
  before <- rng_seed()
  sims <- simulate(m)
  after <- rng_seed()
  expect_false(identical(before, after))

  # the global RNG seed should not change if the seed *is* specified
  before <- rng_seed()
  sims <- simulate(m, seed = 12345)
  after <- rng_seed()
  expect_identical(before, after)

  # the samples should differ if the seed is *not* specified
  one <- simulate(m)
  two <- simulate(m)
  expect_false(identical(one, two))

  # the samples should differ if the seeds are specified differently
  one <- simulate(m, seed = 12345)
  two <- simulate(m, seed = 54321)
  expect_false(identical(one, two))

  # the samples should be the same if the seed is the same
  one <- simulate(m, seed = 12345)
  two <- simulate(m, seed = 12345)
  expect_identical(one, two)
})
