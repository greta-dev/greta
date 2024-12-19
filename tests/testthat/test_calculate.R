test_that("deterministic calculate works with correct lists", {
  skip_if_not(check_tf_version())

  # unknown variable
  x <- as_data(c(1, 2))
  a <- normal(0, 1)
  y <- a * x
  vals <- calculate(y, values = list(a = 3))
  expect_equal(vals$y, matrix(c(3, 6)))

  # unknown variable and new data
  x <- as_data(c(1, 2))
  a <- normal(0, 1)
  y <- a * x
  vals <- calculate(y, values = list(a = 6, x = c(2, 1)))
  expect_equal(vals$y, matrix(c(12, 6)))

  # fixed value depending on multiple variables
  x <- as_data(c(1, 2))
  a1 <- normal(0, 1)
  a2 <- normal(0, 1, truncation = c(0, Inf))
  a <- a1 * a2
  y <- a * x
  vals <- calculate(y, values = list(a = 6, x = c(2, 1)))
  expect_equal(vals$y, matrix(c(12, 6)))
})

test_that("stochastic calculate works with correct lists", {
  skip_if_not(check_tf_version())

  # nolint start
  # with y ~ N(100, 1 ^ 2), it should be very unlikely that y <= 90
  # ( pnorm(90, 100, 1) = 7e-24 )
  # nolint end

  nsim <- 97

  # fix variable
  a <- normal(0, 1)
  y <- normal(a, 1)
  sims <- calculate(y, nsim = nsim, values = list(a = 100))
  expect_true(all(sims$y > 90))
  expect_equal(dim(sims$y), c(nsim, dim(y)))

  # fix variable with more dims on y
  a <- normal(0, 1)
  y <- normal(a, 1, dim = c(3, 3, 3))
  sims <- calculate(y, nsim = nsim, values = list(a = 100))
  expect_true(all(sims$y > 90))
  expect_equal(dim(sims$y), c(nsim, dim(y)))

  # fix variable and new data
  x <- as_data(1)
  a <- normal(0, 1)
  y <- normal(a * x, 1)
  sims <- calculate(y, nsim = nsim, values = list(a = 50, x = 2))
  expect_true(all(sims$y > 90))
  expect_equal(dim(sims$y), c(nsim, dim(y)))

  # data with distribution
  x <- as_data(1)
  y <- as_data(randn(10))
  a <- normal(0, 1)
  distribution(y) <- normal(a * x, 1)
  sims <- calculate(y, nsim = nsim, values = list(a = 50, x = 2))
  expect_true(all(sims$y > 90))
  expect_equal(dim(sims$y), c(nsim, dim(y)))

  # multivariate data with distribution
  n <- 10
  k <- 3
  x <- ones(1, k)
  y <- as_data(randn(n, k))
  a <- normal(0, 1)
  distribution(y) <- multivariate_normal(a * x, diag(k), n_realisations = n)
  sims <- calculate(y, nsim = nsim, values = list(a = 50, x = rep(2, k)))
  expect_true(all(sims$y > 90))
  expect_equal(dim(sims$y), c(nsim, dim(y)))

  # weird multivariate data with distribution
  n <- 10
  k <- 3
  x <- ones(1, k)
  y <- matrix(0, n, k)
  idx <- sample.int(k, n, replace = TRUE)
  y[cbind(seq_len(n), idx)] <- 1
  y <- as_data(y)
  a <- normal(0, 1, dim = c(1, k))
  distribution(y) <- categorical(ilogit(a * x), n_realisations = n)
  sims <- calculate(y,
    nsim = nsim,
    values = list(
      a = c(50, 5, 0.5),
      x = rep(2, k)
    )
  )

  expect_true(all(apply(sims$y, 1:2, sum) == 1))
  expect_equal(dim(sims$y), c(nsim, dim(y)))
})

test_that("deterministic calculate works with greta_mcmc_list objects", {
  skip_if_not(check_tf_version())

  samples <- 10
  x <- as_data(c(1, 2))
  a <- normal(0, 1)
  y <- a * x
  m <- model(y)
  draws <- mcmc(m, warmup = 0, n_samples = samples, verbose = FALSE)

  # with an existing greta array
  y_values <- calculate(y, values = draws)
  # correct class
  expect_s3_class(y_values, "greta_mcmc_list")
  # correct dimensions
  expect_equal(dim(y_values[[1]]), c(10, 2))
  # all valid values
  expect_true(all(is.finite(as.vector(y_values[[1]]))))

  # with a new greta array, based on a different element in the model
  new_values <- calculate(a^2, values = draws)
  # correct class
  expect_s3_class(new_values, "greta_mcmc_list")
  # correct dimensions
  expect_equal(dim(new_values[[1]]), c(10, 1))
  # all valid values
  expect_true(all(is.finite(as.vector(new_values[[1]]))))
})

test_that("calculate with greta_mcmc_list doesn't mix up variables", {
  skip_if_not(check_tf_version())

  a <- normal(-100, 0.001)
  b <- normal(100, 0.001)
  c <- normal(0, 0.001)
  result <- b * c + a

  model <- model(a, b)
  draws <- mcmc(model, warmup = 100, n_samples = 100, verbose = FALSE)

  result_draws <- calculate(result, values = draws)
  vals <- as.vector(as.matrix(result_draws))

  # the values should be around -100 if the variables aren't mixed up, or a long
  # way off if they are
  expect_gt(min(vals), -105)
  expect_lt(max(vals), -95)
})

test_that("calculate with greta_mcmc_list doesn't lose track of new nodes", {
  skip_if_not(check_tf_version())

  z <- normal(0, 1)
  m <- model(z)
  draws <- mcmc(m, warmup = 100, n_samples = 100, verbose = FALSE)

  x <- z^2
  expect_ok(x_draws <- calculate(x, values = draws))
  expect_equal(as.matrix(x_draws)[, 1], as.matrix(draws)[, 1]^2)

  y <- z * 2
  expect_ok(y_draws <- calculate(y, values = draws))
  expect_equal(as.matrix(y_draws)[, 1], as.matrix(draws)[, 1] * 2)
})

test_that("stochastic calculate works with greta_mcmc_list objects", {
  skip_if_not(check_tf_version())

  samples <- 10
  chains <- 2

  n <- 100
  y <- as_data(rnorm(n))
  x <- as_data(1)
  a <- normal(0, 1)
  distribution(y) <- normal(a, x)
  m <- model(a)
  draws <- mcmc(
    m,
    warmup = 0,
    n_samples = samples,
    chains = chains,
    verbose = FALSE
  )

  # this should error without nsim being specified (y is stochastic)
  expect_snapshot(error = TRUE,
    calc_a <- calculate(a, y, values = draws)
  )

  # this should be OK
  sims <- calculate(y, values = draws, nsim = 10)
  expect_equal(dim(sims$y), c(10, dim(y)))

  # for a list of targets, the result should be a list
  nsim <- 10
  sims <- calculate(a, y, values = draws, nsim = nsim)

  # correct class, dimensions, and valid values
  expect_true(is.list(sims))
  expect_equal(names(sims), c("a", "y"))
  expect_equal(dim(sims$a), c(nsim, 1, 1))
  expect_equal(dim(sims$y), c(nsim, n, 1))
  expect_true(all(is.finite(sims$a)) & all(is.finite(sims$y)))

  # a single array with these nsim observations
  sims <- calculate(y, values = draws, nsim = nsim)

  expect_true(is.numeric(sims$y))
  expect_equal(dim(sims$y), c(nsim, n, 1))
  expect_true(all(is.finite(sims$y)))

  # warn about resampling if nsim is greater than elements in draws
  expect_snapshot_warning(
    new_y <- calculate(y, values = draws, nsim = samples * chains + 1)
  )

})

test_that("calculate errors if the mcmc samples unrelated to target", {
  skip_if_not(check_tf_version())

  samples <- 10
  chains <- 2

  n <- 100
  y <- as_data(rnorm(n))
  x <- as_data(1)
  a <- normal(0, 1)
  distribution(y) <- normal(a, x)
  m <- model(a)
  draws <- mcmc(
    m,
    warmup = 0,
    n_samples = samples,
    chains = chains,
    verbose = FALSE
  )

  c <- normal(0, 1)

  expect_snapshot(error = TRUE,
    calc_c <- calculate(c, values = draws)
  )
})

test_that("stochastic calculate works with mcmc samples & new stochastics", {
  skip_if_not(check_tf_version())

  samples <- 10
  chains <- 2

  n <- 100
  y <- as_data(rnorm(n))
  x <- as_data(1)
  a <- normal(0, 1)
  distribution(y) <- normal(a, x)
  m <- model(a)
  draws <- mcmc(
    m,
    warmup = 0,
    n_samples = samples,
    chains = chains,
    verbose = FALSE
  )

  # new stochastic greta array
  b <- lognormal(a, 1)

  # this should error without nsim being specified (b is stochastic and not
  # given by draws)
  expect_snapshot(error = TRUE,
    calc_b <- calculate(b, values = draws)
  )

  sims <- calculate(b, values = draws, nsim = 10)
  expect_equal(dim(sims$b), c(10, dim(b)))
  expect_true(all(sims$b > 0))
})

test_that("calculate errors nicely if non-greta arrays are passed", {
  skip_if_not(check_tf_version())

  x <- c(1, 2)
  a <- normal(0, 1)
  y <- a * x

  # it should error nicely
  expect_snapshot(error = TRUE,
    calc_y <- calculate(y, x, values = list(x = c(2, 1)))
  )

  # and a hint for this common error
  expect_snapshot(error = TRUE,
    calc_y <- calculate(y, list(x = c(2, 1)))
  )

})

test_that("calculate errors nicely if values for stochastics not passed", {
  skip_if_not(check_tf_version())

  x <- as_data(c(1, 2))
  a <- normal(0, 1)
  y <- a * x

  # it should error nicely
  expect_snapshot(error = TRUE,
    calc_y <- calculate(y, values = list(x = c(2, 1)))
  )

  # but is should work fine if nsim is set
  expect_ok(calculate(y, values = list(x = c(2, 1)), nsim = 1))
})

test_that("calculate errors nicely if values have incorrect dimensions", {
  skip_if_not(check_tf_version())

  x <- as_data(c(1, 2))
  a <- normal(0, 1)
  y <- a * x

  # it should error nicely
  expect_snapshot(error = TRUE,
    calc_y <- calculate(y, values = list(a = c(1, 1)))
  )
})

test_that("calculate works with variable batch sizes", {
  skip_if_not(check_tf_version())

  samples <- 100
  x <- as_data(c(1, 2))
  a <- normal(0, 1)
  y <- a * x
  m <- model(y)
  draws <- mcmc(m, warmup = 0, n_samples = samples, verbose = FALSE)

  # variable valid batch sizes
  val_1 <- calculate(y, values = draws, trace_batch_size = 1)
  val_10 <- calculate(y, values = draws, trace_batch_size = 10)
  val_100 <- calculate(y, values = draws, trace_batch_size = 100)
  val_inf <- calculate(y, values = draws, trace_batch_size = Inf)

  # check the first one
  expect_s3_class(val_1, "greta_mcmc_list")
  expect_equal(dim(val_1[[1]]), c(100, 2))
  expect_true(all(is.finite(as.vector(val_1[[1]]))))

  # check the others are the same
  expect_identical(val_10, val_1)
  expect_identical(val_100, val_1)
  expect_identical(val_inf, val_1)
})

test_that("calculate errors nicely with invalid batch sizes", {
  skip_if_not(check_tf_version())

  samples <- 100
  x <- as_data(c(1, 2))
  a <- normal(0, 1)
  y <- a * x
  m <- model(y)
  draws <- mcmc(m, warmup = 0, n_samples = samples, verbose = FALSE)

  # variable valid batch sizes
  expect_snapshot(error = TRUE,
    calc_y <- calculate(y, values = draws, trace_batch_size = 0)
  )
  expect_snapshot(error = TRUE,
    calc_y <- calculate(y, values = draws, trace_batch_size = NULL)
  )
  expect_snapshot(error = TRUE,
    calc_y <- calculate(y, values = draws, trace_batch_size = NA)
  )
})

test_that("calculate returns a named list", {
  skip_if_not(check_tf_version())

  a <- as_data(randn(3))
  b <- a^2
  c <- sqrt(b)

  # if target is a single greta array, the output should be a single numeric
  result <- calculate(b, nsim = 10)
  expect_true(is.list(result))
  expect_true(is.numeric(result$b))

  # if target is a list, the output should be a list of numerics
  result <- calculate(b, c, nsim = 10)
  expect_true(is.list(result))

  # check contents
  are_numeric <- vapply(result, is.numeric, FUN.VALUE = logical(1))
  expect_true(all(are_numeric))

  # check names
  expect_equal(names(result), c("b", "c"))
})

test_that("calculate produces the right number of samples", {
  skip_if_not(check_tf_version())

  # fix variable
  a <- normal(0, 1)
  y <- normal(a, 1, dim = c(1, 3))

  # should be vectors
  sims <- calculate(a, nsim = 1)
  expect_equal(dim(sims$a), c(1, dim(a)))

  sims <- calculate(a, nsim = 17)
  expect_equal(dim(sims$a), c(17, dim(a)))

  sims <- calculate(y, nsim = 1)
  expect_equal(dim(sims$y), c(1, dim(y)))

  sims <- calculate(y, nsim = 19)
  expect_equal(dim(sims$y), c(19, dim(y)))

})

test_that("calculate works if distribution-free variables are fixed", {
  skip_if_not(check_tf_version())


  # fix variable
  a <- variable()
  y <- normal(a, 1)
  sims <- calculate(a, y, nsim = 1, values = list(a = 100))

  expect_true(all(sims$y > 90))
})

test_that("calculate errors if distribution-free variables are not fixed", {
  skip_if_not(check_tf_version())

  # fix variable
  a <- variable()
  y <- normal(a, 1)
  expect_snapshot(error = TRUE,
    calc_a <- calculate(a, y, nsim = 1)
  )
})

test_that("calculate errors if a distribution cannot be sampled from", {
  skip_if_not(check_tf_version())

  # fix variable
  y <- hypergeometric(5, 3, 2)
  expect_snapshot(error = TRUE,
    sims <- calculate(y, nsim = 1)
  )
})

test_that("calculate errors nicely if nsim is invalid", {
  skip_if_not(check_tf_version())

  x <- normal(0, 1)

  expect_snapshot(error = TRUE,
    calc_x <- calculate(x, nsim = 0)
  )

  expect_snapshot(error = TRUE,
    calc_x <- calculate(x, nsim = -1)
  )

  expect_snapshot(error = TRUE,
    calc_x <- calculate(x, nsim = "five")
  )
})
