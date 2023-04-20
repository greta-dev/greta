
test_that("calculate samples are the same when the argument seed is the same", {
  a <- normal(0, 1)
  y <- normal(a, 1)
  m <- model(y)

  c_one <- calculate(y, nsim = 1, seed = 12345)
  c_two <- calculate(y, nsim = 1, seed = 12345)

  expect_equal(
    as.numeric(c_one),
    as.numeric(c_two)
  )

})

test_that("calculate samples are the same when the R seed is the same", {
  a <- normal(0, 1)
  y <- normal(a, 1)
  m <- model(y)

  set.seed(12345)
  c_one <- calculate(y, nsim = 1)

  set.seed(12345)
  c_two <- calculate(y, nsim = 1)

  expect_equal(
    as.numeric(c_one),
    as.numeric(c_two)
    )
})

test_that("mcmc samples are the same when the R seed is the same", {
  a <- normal(0, 1)
  y <- normal(a, 1)
  m <- model(y)

  set.seed(12345)
  one <- mcmc(m, warmup = 10, n_samples = 1, chains = 1)
  set.seed(12345)
  two <- mcmc(m, warmup = 10, n_samples = 1, chains = 1)

  expect_equal(
    as.numeric(one),
    as.numeric(two)
    )
})


