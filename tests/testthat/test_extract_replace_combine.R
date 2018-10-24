context("extract/replace/combine")

test_that("extract works like R", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  a <- randn(10)
  b <- randn(10, 1)
  c <- randn(10, 5)
  d <- randn(2, 2, 2)

  # Can extract with a vector, regardless of dimension
  check_expr(a[1:6], "a")
  check_expr(b[1:6], "b")
  check_expr(c[1:6], "c")
  check_expr(d[1:6], "d")

  # can extract first column, regardless of dimension
  check_expr(b[1:6, ], "b")
  check_expr(b[1:6, 1], "b")
  check_expr(c[1:6, ], "c")
  check_expr(c[1:6, 1], "c")
  check_expr(d[1:2, , ], "d")
  check_expr(d[1:2, 1, , drop = FALSE], "d")

  # can extract with negative dimensions
  check_expr(a[3:1], "a")
  check_expr(d[2:1, , 1:2], "d")

  # can extract with logicals
  check_expr(a[c(TRUE, FALSE, TRUE)], "a")

  # can extract with a mix of numerics and logicals
  check_expr(d[2:1, , c(TRUE, FALSE), drop = FALSE], "d")

  # can extract with missing entries in various places
  check_expr(d[, , 2:1], "d")
  check_expr(d[, 2:1, ], "d")
  check_expr(d[2:1, , ], "d")

  # can extract single elements without dropping dimensions
  check_expr(d[, , 1, drop = FALSE], "d")
  check_expr(d[, 1, , drop = FALSE], "d")
  check_expr(d[1, , , drop = FALSE], "d")

  # can do empty extracts
  check_expr(a[], "a")
  check_expr(b[], "b")
  check_expr(c[], "c")
  check_expr(d[], "d")

  # can do negative extracts
  check_expr(a[-1], "a")
  check_expr(b[-1:-3, ], "b")
  check_expr(c[-1:-4, ], "c")
  check_expr(d[-1:-2, -1, , drop = FALSE], "d")

})

test_that("replace works like R", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  # check using expressions, and comparing the whole object to which replacement
  # was applied

  # Can replace with a vector, regardless of dimension
  x <- randn(10)
  check_expr({
    x[1:6] <- seq_len(6)
    x
  })

  x <- randn(10, 1)
  check_expr({
    x[1:6] <- seq_len(6)
    x
  })

  x <- randn(10, 5)
  check_expr({
    x[1:6] <- seq_len(6)
    x
  })

  x <- randn(2, 2, 2)
  check_expr({
    x[1:6] <- seq_len(6)
    x
  })

  # can replace first column, regardless of dimension
  x <- randn(10, 1)
  check_expr({
    x[1:6, 1] <- seq_len(6 * 1)
    x
  })

  x <- randn(10, 5)
  check_expr({
    x[1:6, 1] <- seq_len(6 * 1)
    x
  })

  x <- randn(2, 2, 2)
  check_expr({
    x[1:2, 1, ] <- seq_len(2 * 1 * 2)
    x
  })

  # can replace a chunk, regardless of dimension
  x <- randn(10, 1)
  check_expr({
    x[1:6, ] <- seq_len(6)
    x
  })

  x <- randn(10, 5)
  check_expr({
    x[1:6, ] <- seq_len(6 * 5)
    x
  })

  x <- randn(10, 2, 2)
  check_expr({
    x[1:2, , ] <- seq_len(2 * 2 * 2)
    x
  })

  # can replace with negative dimensions
  x <- randn(10)
  check_expr({
    x[3:1] <- seq_len(3)
    x
  })

  x <- randn(10, 2, 2)
  check_expr({
    x[2:1, , 1:2] <- seq_len(2 * 2 * 2)
    x
  })

  # can replace with logicals (the logical vector is repeated 3 1/3 times)
  x <- randn(10)
  check_expr({
    x[c(TRUE, FALSE, TRUE)] <- seq_len(7)
    x
  })

  # can extract with a mix of numerics and logicals
  x <- randn(10, 2, 2)
  check_expr({
    x[2:1, , c(TRUE, FALSE)] <- seq_len(4)
    x
  })

  # can assign with missing entries in various places
  x <- randn(10, 2, 2)
  check_expr({
    x[, , 1:2] <- seq_len(10 * 2 * 2)
    x
  })

  x <- randn(10, 2, 2)
  check_expr({
    x[, 1:2, ] <- seq_len(10 * 2 * 2)
    x
  })

  x <- randn(10, 2, 2)
  check_expr({
    x[1:2, , ] <- seq_len(2 * 2 * 2)
    x
  })

  # can assign single elements without dropping dimensions
  x <- randn(10, 2, 2)
  check_expr({
    x[1, , ] <- seq_len(1 * 2 * 2)
    x
  })

  x <- randn(10, 2, 2)
  check_expr({
    x[, 1, ] <- seq_len(10 * 1 * 2)
    x
  })

  x <- randn(10, 2, 2)
  check_expr({
    x[, , 1] <- seq_len(10 * 2 * 1)
    x
  })

  # can do full replacements
  x <- randn(10)
  check_expr({
    x[] <- seq_len(10)
    x
  })

  x <- randn(10, 1)
  check_expr({
    x[] <- seq_len(10)
    x
  })

  x <- randn(10, 5)
  check_expr({
    x[] <- seq_len(10 * 5)
    x
  })

  x <- randn(2, 2, 2)
  check_expr({
    x[] <- seq_len(2 * 2 * 2)
    x
  })

  # can do negative replacements
  x <- randn(10)
  check_expr({
    x[-1] <- seq_len(9)
    x
  })

  x <- randn(10, 1)
  check_expr({
    x[-1:-3, ] <- seq_len(7 * 1)
    x
  })

  x <- randn(10, 5)
  check_expr({
    x[-1:-4, ] <- seq_len(6 * 5)
    x
  })

  x <- randn(2, 2, 2)
  check_expr({
    x[-1, -1, ] <- seq_len(1 * 1 * 2)
    x
  })

  # can replace multiple entries with one;
  x <- randn(10)
  check_expr({
    x[1:3] <- 1
    x
  })

  x <- randn(10, 1)
  check_expr({
    x[1:3, 1] <- 1
    x
  })

  x <- randn(10, 5)
  check_expr({
    x[1:3, ] <- 1
    x
  })

  x <- randn(2, 2, 2)
  check_expr({
    x[1:2, , ] <- 1
    x
  })

  # can replace multiple entries with a vector (with factorizing length)
  x <- randn(10)
  check_expr({
    x[1:4] <- seq_len(2)
    x
  })

  x <- randn(10, 1)
  check_expr({
    x[1:4, 1] <- seq_len(2)
    x
  })

  x <- randn(10, 5)
  check_expr({
    x[1:9, ] <- seq_len(3)
    x
  })

  x <- randn(2, 2, 2)
  check_expr({
    x[1:2, , 1] <- seq_len(2)
    x
  })

  # replace with a greta array (e.g. with subset of self)
  # check_expr seems to balls this one up, so do it longhand
  x <- randn(10, 1)
  ga_x <- as_data(x)
  x[1:6, ] <- x[6:1, ]
  ga_x[1:6, ] <- ga_x[6:1, ]
  greta_out <- as.vector(grab(ga_x))
  compare_op(x, greta_out)

})

test_that("rep works like R", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  a <- randn(10)
  b <- randn(10, 1)
  c <- randn(10, 5)
  d <- randn(10, 2, 2)

  rep_times <- function(x)
    rep(x, times = 3)

  check_op(rep_times, a)
  check_op(rep_times, b)
  check_op(rep_times, c)
  check_op(rep_times, d)

  rep_length <- function(x)
    rep(x, length.out = 3)

  check_op(rep_length, a)
  check_op(rep_length, b)
  check_op(rep_length, c)
  check_op(rep_length, d)

  rep_times_each <- function(x)
    rep(x, times = 3, each = 3)

  check_op(rep_times_each, a)
  check_op(rep_times_each, b)
  check_op(rep_times_each, c)
  check_op(rep_times_each, d)

  rep_length_each <- function(x)
    rep(x, length = 30, each = 3)

  check_op(rep_length_each, a)
  check_op(rep_length_each, b)
  check_op(rep_length_each, c)
  check_op(rep_length_each, d)

})

test_that("rbind, cbind and c work like R", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  a <- randn(5, 1)
  b <- randn(1, 5)
  d <- randn(5, 5)

  check_op(rbind, a, a)
  check_op(rbind, b, d)
  check_op(rbind, d, b)

  check_op(cbind, b, b)
  check_op(cbind, a, d)
  check_op(cbind, d, d)

  # flatten and concatenate arrays
  check_op(c, a, d)
  check_op(c, b, a)
  check_op(c, d, b)

  # unary c flattens arrays
  check_op(c, a)
  check_op(c, b)
  check_op(c, d)

})

test_that("abind works like R", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  a <- randn(5, 1, 3)
  b <- randn(1, 1, 3)
  c <- randn(5, 1, 1)

  check_op(abind, a, b, other_args = list(along = 1))
  check_op(abind, a, c)
  check_op(abind, a, a, other_args = list(along = 0))
  check_op(abind, a, a, other_args = list(along = 4))

})

test_that("abind errors informatively", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  a <- ones(5, 1, 3)
  b <- ones(1, 1, 3)
  c <- ones(5, 1, 1)

  expect_error(abind(a, b),
               "dimension 1 had varying sizes")
  expect_error(abind(a, c, along = 5),
               "along must be between 0 and 4")

})

test_that("rbind and cbind can prepend R arrays to greta arrays", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  a <- randn(5, 1)
  b <- ones(5, 1)

  z <- rbind(a, b)
  expect_s3_class(z, "greta_array")
  expect_identical(dim(z), c(10L, 1L))

  z <- rbind(b, a)
  expect_s3_class(z, "greta_array")
  expect_identical(dim(z), c(10L, 1L))

  z <- cbind(a, b)
  expect_s3_class(z, "greta_array")
  expect_identical(dim(z), c(5L, 2L))

  z <- cbind(b, a)
  expect_s3_class(z, "greta_array")
  expect_identical(dim(z), c(5L, 2L))

})

test_that("assign errors on variable greta arrays", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  z <- normal(0, 1, dim = 5)
  expect_error(z[1] <- 3,
               "cannot replace values in a variable greta array")

})

test_that("rbind and cbind give informative error messages", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  a <- as_data(randn(5, 1))
  b <- as_data(randn(1, 5))

  expect_error(rbind(a, b),
               "all greta arrays must be have the same number of columns")

  expect_error(cbind(a, b),
               "all greta arrays must be have the same number of rows")

})

test_that("replacement gives informative error messages", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  x <- ones(2, 2, 2)
  expect_error(x[1:2, , 1] <- seq_len(3),
               "not a multiple of replacement length")

  expect_error(x[1, 1, 3] <- 1,
               "subscript out of bounds")

  x <- ones(2)
  expect_error(x[3] <- 1,
               "subscript out of bounds")

})

test_that("extraction gives informative error messages", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  x <- ones(2, 2, 2)
  expect_error(x[1, 1, 3],
               "subscript out of bounds")

  x <- ones(2)
  expect_error(x[3],
               "subscript out of bounds")

})

test_that("stochastic and operation greta arrays can be extracted", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  a <- normal(0, 1, dim = c(3, 4))
  a_sub <- a[1:2, 2:3]
  expect_identical(dim(a_sub), c(2L, 2L))

  b <- a * 2
  b_sub <- b[1:2, ]
  expect_identical(dim(b_sub), c(2L, 4L))

})

test_that("extract, replace, combine work in models", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  # extract
  a <- normal(0, 1, dim = c(3, 4))
  a_sub <- a[1:2, 2:3]
  m_a <- model(a_sub)
  expect_ok(draws_a <- mcmc(m_a, warmup = 3, n_samples = 3, verbose = FALSE))

  # replace
  b <- ones(4, 3)
  x <- normal(0, 1, dim = 4)
  b[, 2] <- x
  m_b <- model(b)
  expect_ok(draws_b <- mcmc(m_b, warmup = 3, n_samples = 3, verbose = FALSE))

  # combine
  d <- c(normal(0, 1, dim = 2),
         lognormal(0, 1, dim = 3))
  m_d <- model(d)
  expect_ok(draws_d <- mcmc(m_d, warmup = 3, n_samples = 3, verbose = FALSE))

})

test_that("head and tail work", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  a <- randn(10, 1)
  b <- randn(10, 4)
  c <- randn(10, 3, 3)

  check_op(head, a)
  check_op(tail, a)

  check_op(head, b)
  check_op(tail, b)

  check_op(head, c)
  check_op(tail, c)

})

test_that("length and dim work", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  ga_data <- as_data(matrix(1:9, nrow = 3))
  ga_stochastic <- normal(0, 1, dim = c(3, 3))
  ga_operation <- ga_data * ga_stochastic

  # length
  expect_identical(length(ga_data), 9L)
  expect_identical(length(ga_stochastic), 9L)
  expect_identical(length(ga_operation), 9L)

  # dim
  expect_identical(dim(ga_data), c(3L, 3L))
  expect_identical(dim(ga_stochastic), c(3L, 3L))
  expect_identical(dim(ga_operation), c(3L, 3L))

})

test_that("dim<- works", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  x <- randn(3, 4, 2)

  new_dim <- c(2, 2, 6)
  check_op(`dim<-`, x, other_args = list(value = new_dim))

  new_dim <- c(12, 2)
  check_op(`dim<-`, x, other_args = list(value = new_dim))

  new_dim <- NULL
  check_op(`dim<-`, x, other_args = list(value = new_dim))

})

test_that("greta_array() reshapes greta arrays like array", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  x_ <- randu(3, 4, 2)
  x <- as_data(x_)
  new_dim <- c(12, 2)

  # for data greta arrays
  y_ <- as_data(array(x_, dim = new_dim))
  y <- greta_array(x, dim = new_dim)
  compare_op(calculate(y), calculate(y_))

  # for operation greta arrays
  x <- abs(x)
  y_ <- as_data(array(x_, dim = new_dim))
  y <- greta_array(x, dim = new_dim)
  compare_op(calculate(y), calculate(y_))

  # for variable greta arrays
  x <- variable(dim = dim(x))
  y_ <- as_data(array(x_, dim = new_dim))
  y <- greta_array(x, dim = new_dim)
  compare_op(calculate(y, list(x = x_)), calculate(y_))

})

test_that("dim<- errors as expected", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  x <- zeros(3, 4)

  expect_error(dim(x) <- pi[0],
               "length-0 dimension vector is invalid")

  expect_error(dim(x) <- c(1, NA),
               "the dims contain missing values")

  expect_error(dim(x) <- c(1, -1),
               "the dims contain negative values")

  expect_error(dim(x) <- 13,
               paste("dims \\[product 13\\] do not match the length of",
                     "object \\[12\\]"))

})

test_that("dim<- works in a model", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  y <- rnorm(5)

  x1 <- greta_array(1:12, c(3, 4))
  dim(x1) <- NULL

  x2 <- greta_array(1:12, c(3, 4))
  dim(x2) <- 12

  x3 <- greta_array(1:12, c(3, 4))
  dim(x3) <- c(6, 2)

  z <- x1[6, ] * x2[7, ] * x3[5, 2]

  distribution(y) <- normal(z, lognormal(0, 1))

  expect_ok(m <- model(z))
  expect_ok(mcmc(m, warmup = 0, n_samples = 2))

})

test_that("c handles NULLs and lists", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  x <- normal(0, 1)
  y <- as_data(3:1)

  # NULLs should be dropped when concatenating
  z <- c(x, NULL, y)
  expect_s3_class(z, "greta_array")
  expect_identical(dim(z), c(4L, 1L))

  # NULLs at the end should dissappear
  z <- c(x, NULL)
  expect_s3_class(z, "greta_array")
  expect_identical(dim(z), c(1L, 1L))

  # greta arrays combined with things coercible to greta arrays should return a
  # greta array
  z <- c(x, 0, FALSE)
  expect_s3_class(z, "greta_array")
  expect_identical(dim(z), c(3L, 1L))

  # greta arrays combined with other things should return a list
  z <- c(x, mean)
  expect_true(is.list(z))
  expect_s3_class(z[[1]], "greta_array")
  expect_identical(dim(z[[1]]), c(1L, 1L))

  # even with a NULL in there
  z <- c(x, NULL, mean)
  expect_true(is.list(z))
  expect_s3_class(z[[1]], "greta_array")
  expect_identical(dim(z[[1]]), c(1L, 1L))

})
