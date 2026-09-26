test_that("building greta arrays leaves R's random number stream alone", {
  skip_if_not(check_tf_version())

  set.seed(1)
  before <- rng_seed()
  x <- normal(0, 1)
  y <- x * 2
  m <- model(x)
  expect_identical(rng_seed(), before)
})

test_that("greta arrays built after the same seed stay distinct", {
  skip_if_not(check_tf_version())

  # node names used to be drawn from R's random number stream, so re-seeding
  # before each array repeated them, and a and b became one node sharing one
  # set of draws - greta-dev/greta#366. Identical priors, so a scheme that
  # named nodes by hashing their contents would merge them too
  set.seed(1)
  a <- normal(0, 1)
  set.seed(1)
  b <- normal(0, 1)

  draws <- calculate(a, b, nsim = 10)
  expect_false(identical(draws$a, draws$b))
})
