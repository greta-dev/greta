test_that("`distribution<-` works in models", {
  skip_if_not(check_tf_version())

  # with a distribution parameter
  y <- as_data(randn(5))
  expect_equal(node_type(get_node(y)), "data")

  # data
  mu <- normal(0, 1)
  distribution(y) <- normal(mu, 2)
  sample_distribution(mu)
})

test_that("distribution() works", {
  skip_if_not(check_tf_version())

  a <- normal(0, 1)
  x <- as_data(randn(5))

  # when run on a distribution, should just return the same greta array
  expect_identical(distribution(a), a)

  # when run on something without a distribution, should return NULL
  expect_null(distribution(x))

  # once assigned, should return the original distribution
  a2 <- normal(0, 1)
  distribution(x) <- a2
  expect_equal(distribution(x), x)
})

test_that("`distribution<-` errors informatively", {
  skip_if_not(check_tf_version())

  y <- randn(3, 3, 2)
  x <- randn(1)

  # not a greta array with a distribution on the right
  expect_snapshot(error = TRUE,
    distribution(y) <- x
  )

  expect_snapshot(error = TRUE,
    distribution(y) <- as_data(x)
  )

  # no density on the right
  expect_snapshot(error = TRUE,
    distribution(y) <- variable()
  )

  # non-scalar and wrong dimensions
  expect_snapshot(error = TRUE,
    distribution(y) <- normal(0, 1, dim = c(3, 3, 1))
  )

  # double assignment of distribution to node
  y_ <- as_data(y)
  distribution(y_) <- normal(0, 1)
  expect_snapshot(error = TRUE,
    distribution(y_) <- normal(0, 1)
  )

  # assignment with a greta array that already has a fixed value
  y1 <- as_data(y)
  y2 <- as_data(y)
  d <- normal(0, 1)
  distribution(y1) <- d
  expect_snapshot(error = TRUE,
    distribution(y2) <- y1
  )

  # assignment to a variable
  z <- variable()
  expect_snapshot(error = TRUE,
    distribution(z) <- normal(0, 1)
  )

  # assignment to an op
  z2 <- z^2
  expect_snapshot(error = TRUE,
    distribution(z2) <- normal(0, 1)
  )

  # assignment to another distribution
  u <- uniform(0, 1)
  expect_snapshot(error = TRUE,
    distribution(z2) <- normal(0, 1)
  )

})

test_that("distribution() errors informatively", {
  skip_if_not(check_tf_version())

  y <- randn(3)

  expect_snapshot(
    error = TRUE,
    distribution(y)
  )
})
