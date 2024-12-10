test_that("simulate produces the right number of samples", {
  skip_if_not(check_tf_version())

  # fix variable
  a <- normal(0, 1)
  y <- normal(a, 1, dim = c(1, 3))
  m <- model(y, a)

  # should be vectors
  sims <- simulate(m)
  expect_equal(dim(sims$a), c(1, dim(a)))
  expect_equal(dim(sims$y), c(1, dim(y)))

  sims <- simulate(m, 17)
  expect_equal(dim(sims$a), c(17, dim(a)))
  expect_equal(dim(sims$y), c(17, dim(y)))
})

test_that("simulate errors if distribution-free variables are not fixed", {
  skip_if_not(check_tf_version())


  # fix variable
  a <- variable()
  y <- normal(a, 1)
  m <- model(y)
  expect_snapshot(error = TRUE,
    sims <- simulate(m)
  )
})

test_that("simulate errors if a distribution cannot be sampled from", {
  skip_if_not(check_tf_version())


  # fix variable
  y_ <- rhyper(10, 5, 3, 2)
  y <- as_data(y_)
  m <- lognormal(0, 1)
  distribution(y) <- hypergeometric(m, 3, 2)
  m <- model(y)
  expect_snapshot(error = TRUE,
    sims <- simulate(m)
  )
})

test_that("simulate errors nicely if nsim is invalid", {
  skip_if_not(check_tf_version())


  x <- normal(0, 1)
  m <- model(x)

  expect_snapshot(error = TRUE,
    simulate(m, nsim = 0)
  )

  expect_snapshot(error = TRUE,
    simulate(m, nsim = -1)
  )

  expect_snapshot(error = TRUE,
    simulate(m, nsim = "five")
  )
})
