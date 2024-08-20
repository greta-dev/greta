set.seed(2020 - 02 - 11)

test_that("simple functions work as expected", {
  skip_if_not(check_tf_version())

  x <- randn(25, 4)
  n <- 10
  k <- 5

  # logarithms and exponentials
  check_op(log, exp(x))
  check_op(exp, x)
  check_op(log1p, exp(x))
  check_op(expm1, x)

  # miscellaneous mathematics
  check_op(abs, x)
  check_op(mean, x)
  check_op(sqrt, exp(x))
  check_op(sign, x)

  # rounding of numbers
  check_op(ceiling, x)
  check_op(floor, x)
  check_op(round, x)

  # trigonometry
  check_op(cos, x)
  check_op(sin, x)
  check_op(tan, x * 0.5)
  check_op(acos, 2 * plogis(x) - 1)
  check_op(asin, 2 * plogis(x) - 1)
  check_op(atan, x)

  # special mathematical functions
  check_op(lgamma, x)
  check_op(digamma, x, tolerance = 1e-2)
})


test_that("primitive functions work as expected", {
  skip_if_not(check_tf_version())
  # source("helpers.R")

  real <- randn(25, 4)
  pos <- exp(real)
  posp1 <- pos + 1
  m1p1 <- 2 * pnorm(real) - 1

  check_op(log10, pos)
  check_op(log2, pos)
  check_op(cosh, real)
  check_op(sinh, real)
  check_op(tanh, real)
  check_op(acosh, posp1)
  check_op(asinh, posp1)
  check_op(atanh, m1p1)
  check_op(cospi, real)
  check_op(sinpi, real)
  # potentially change check_op to use absolute or relative error
  check_op(tanpi, real, relative_error = TRUE)
  check_op(trigamma, real, relative_error = TRUE)
})

test_that("cummax and cummin functions error informatively", {
  skip_if_not(check_tf_version())

  cumulative_funs <- list(cummax, cummin)
  x <- as_data(randn(10))

  for (fun in cumulative_funs) {
    expect_snapshot(error = TRUE,
      fun(x)
    )
  }
})

test_that("complex number functions error informatively", {
  skip_if_not(check_tf_version())

  complex_funs <- list(Im, Re, Arg, Conj, Mod)
  x <- as_data(randn(25, 4))

  for (fun in complex_funs) {
    expect_snapshot(error = TRUE,
      fun(x)
    )
  }
})

test_that("matrix functions work as expected", {
  skip_if_not(check_tf_version())

  a <- rWishart(1, 6, diag(5))[, , 1]
  b <- randn(5, 25)
  c <- chol(a)
  d <- c(1, 1)
  e <- randn(10, 25)
  f <- randn(3, 4, 2)

  check_op(t, b)
  check_op(chol, a)
  check_op(chol2inv, c)
  check_op(chol2symm, c)
  check_op(cov2cor, a)
  check_op(diag, a)
  check_op(`diag<-`, a, 1:5, only = "data")
  check_op(solve, a)
  check_op(solve, a, b)
  check_op(forwardsolve, c, b)
  check_op(backsolve, c, b)
  check_op(kronecker, a, c)
  check_op(kronecker, a, d)
  check_op(kronecker, a, a, other_args = list(FUN = "*"))
  check_op(kronecker, a, a, other_args = list(FUN = "+"))
  check_op(kronecker, a, a, other_args = list(FUN = "-"))
  check_op(kronecker, a, a, other_args = list(FUN = "/"))
  check_op(rdist, b)
  check_op(rdist, b, e)
  check_op(rdist, f, f)
})

test_that("kronecker works with greta and base array arguments", {
  skip_if_not(check_tf_version())

  a <- rWishart(1, 6, diag(5))[, , 1]
  b <- chol(a)

  a_greta <- as_data(a)
  b_greta <- as_data(b)

  base_out <- kronecker(a, b)
  greta_out1 <- kronecker(a_greta, b)
  greta_out2 <- kronecker(a, b_greta)

  expect_true(is.greta_array(greta_out1))
  expect_true(is.greta_array(greta_out1))

  compare_op(base_out, grab(greta_out1))
  compare_op(base_out, grab(greta_out2))
})

test_that("aperm works as expected", {
  skip_if_not(check_tf_version())

  a <- randn(5, 4, 3, 2, 1)

  # default is to reverse dims
  check_op(aperm, a)

  # random permutations
  perms <- replicate(5, sample.int(5), simplify = FALSE)
  for (perm in perms) {
    check_op(aperm, a, other_args = list(perm = perm))
  }
})

test_that("reducing functions work as expected", {
  skip_if_not(check_tf_version())

  a <- randn(1, 3)
  b <- randn(5, 25)
  c <- randn(2, 3, 4)

  check_op(sum, a, b)
  check_op(prod, a, b)
  check_op(min, a, b)
  check_op(max, a, b)

  check_op(colSums, b)
  check_op(rowSums, b)
  check_op(colMeans, b)
  check_op(rowMeans, b)

  # default 3D reduction
  check_op(colSums, c)
  check_op(rowSums, c)
  check_op(colMeans, c)
  check_op(rowMeans, c)

  # weird 3D reduction
  x <- randn(2, 3, 4)
  check_expr(colSums(x, dims = 2))
  check_expr(rowSums(x, dims = 2))
  check_expr(colMeans(x, dims = 2))
  check_expr(rowMeans(x, dims = 2))
})

test_that("cumulative functions work as expected", {
  skip_if_not(check_tf_version())

  a <- randn(5)

  check_op(cumsum, a)
  check_op(cumprod, a)
})

test_that("apply works as expected", {
  skip_if_not(check_tf_version())

  # check apply.greta_array works like R's apply for X
  check_apply <- function(X, MARGIN, FUN) { # nolint
    check_op(apply, a,
      other_args = list(
        MARGIN = MARGIN,
        FUN = FUN
      )
    )
  }

  a <- randu(5, 4, 3, 2, 1)

  single_margins <- as.list(1:5)
  multi_margins <- list(c(1, 4), c(2, 5), c(3, 4, 5))
  margins <- c(single_margins, multi_margins)

  for (margin in margins) {
    check_apply(a, margin, "sum")
    check_apply(a, margin, "max")
    check_apply(a, margin, "mean")
    check_apply(a, margin, "min")
    check_apply(a, margin, "prod")
    check_apply(a, margin, "cumsum")
    check_apply(a, margin, "cumprod")
  }
})

test_that("tapply works as expected", {
  skip_if_not(check_tf_version())

  x <- randn(15, 1)

  check_expr(tapply(x, rep(1:5, each = 3), "sum"))
  check_expr(tapply(x, rep(1:5, each = 3), "max"))
  check_expr(tapply(x, rep(1:5, each = 3), "mean"))
  check_expr(tapply(x, rep(1:5, each = 3), "min"))
  check_expr(tapply(x, rep(1:5, each = 3), "prod"))
})

test_that("cumulative functions error as expected", {
  skip_if_not(check_tf_version())

  a <- as_data(randn(1, 5))
  b <- as_data(randn(5, 1, 1))


  expect_snapshot(error = TRUE,
    cumsum(a)
    )

  expect_snapshot(error = TRUE,
    cumsum(b)
  )

  expect_snapshot(error = TRUE,
    cumprod(a)
  )

  expect_snapshot(error = TRUE,
    cumprod(b)
  )

})

test_that("sweep works as expected", {
  skip_if_not(check_tf_version())

  stats_list <- list(randn(5), randn(25))
  x <- randn(5, 25)

  for (dim in c(1, 2)) {
    for (fun in c("-", "+", "/", "*")) {
      stats <- stats_list[[dim]]

      r_out <- sweep(x, dim, stats, FUN = fun)

      greta_array <- sweep(as_data(x), dim, as_data(stats), FUN = fun)
      greta_out <- grab(greta_array)

      compare_op(r_out, greta_out)
    }
  }
})

test_that("sweep works for numeric x and greta array STATS", {
  skip_if_not(check_tf_version())

  stats <- randn(5)
  ga_stats <- as_data(stats)
  x <- randn(5, 25)

  res <- sweep(x, 1, stats, "*")
  expect_ok(ga_res <- sweep(x, 1, ga_stats, "*"))
  diff <- abs(res - grab(ga_res))
  expect_true(all(diff < 1e-6))
})

test_that("solve and sweep and kronecker error as expected", {
  skip_if_not(check_tf_version())

  a <- as_data(randn(5, 25))
  b <- as_data(randn(5, 25, 2))
  c <- as_data(randn(5, 5))
  stats <- as_data(randn(5))

  # solve

  # a must be 2D
  expect_snapshot(
    error = TRUE,
    solve(b, a)
  )

  # b must also be 2D
  expect_snapshot(
    error = TRUE,
    solve(c, b)
  )

  # only square matrices allowed for first element
  expect_snapshot(
    error = TRUE,
    solve(a, a)
  )

  expect_snapshot(
    error = TRUE,
    solve(a)
  )

  # dimension of second array must match
  expect_snapshot(
    error = TRUE,
    solve(c, t(a))
  )

  # sweep
  # x must be 2D
  expect_snapshot(
    error = TRUE,
    sweep(b, 1, stats)
  )

  # dim must be either 1 or 2
  expect_snapshot(
    error = TRUE,
    sweep(a, 3, stats)
  )

  # stats must have the correct number of elements
  expect_snapshot(
    error = TRUE,
    sweep(a, 1, c(stats, stats))
  )

  # stats must be a column vector
  expect_snapshot(
    error = TRUE,
    sweep(a, 1, t(stats))
  )

  expect_snapshot(
    error = TRUE,
    sweep(a, 2, stats)
  )

  # kronecker
  # X must be 2D
  expect_snapshot(
    error = TRUE,
    kronecker(a, b)
  )

  # Y must be 2D
  expect_snapshot(
    error = TRUE,
    kronecker(b, c)
  )

})

test_that("colSums etc. error as expected", {
  skip_if_not(check_tf_version())

  x <- as_data(randn(3, 4, 5))

  expect_snapshot(error = TRUE,
    colSums(x, dims = 3)
  )

  expect_snapshot(error = TRUE,
    rowSums(x, dims = 3)
  )

  expect_snapshot(error = TRUE,
    colMeans(x, dims = 3)
  )

  expect_snapshot(error = TRUE,
    rowMeans(x, dims = 3)
  )

})

test_that("forwardsolve and backsolve error as expected", {
  skip_if_not(check_tf_version())

  a <- wishart(6, diag(5))
  b <- as_data(randn(5, 25))
  c <- chol(a)

  expect_snapshot(error = TRUE,
    forwardsolve(a, b, k = 1)
  )

  expect_snapshot(error = TRUE,
    backsolve(a, b, k = 1)
  )

  expect_snapshot(error = TRUE,
    forwardsolve(a, b, transpose = TRUE)
  )

  expect_snapshot(error = TRUE,
    backsolve(a, b, transpose = TRUE)
  )

})

test_that("tapply errors as expected", {
  skip_if_not(check_tf_version())

  group <- sample.int(5, 10, replace = TRUE)
  a <- ones(10, 1)
  b <- ones(10, 2)

  # X must be a column vector
  expect_snapshot(error = TRUE,
    tapply(b, group, "sum")
  )

  # INDEX can't be a greta array
  expect_snapshot(error = TRUE,
    tapply(a, as_data(group), "sum")
  )
})

test_that("eigen works as expected", {
  skip_if_not(check_tf_version())

  k <- 4
  x <- rWishart(1, k + 1, diag(k))[, , 1]
  x_ga <- as_data(x)

  r_out <- eigen(x)
  greta_out <- eigen(as_data(x))

  r_out_vals <- eigen(x, only.values = TRUE)
  greta_out_vals <- eigen(as_data(x), only.values = TRUE)

  # values
  compare_op(r_out$values, grab(greta_out$values))

  # only values
  compare_op(r_out_vals$values, grab(greta_out_vals$values))

  # vectors
  # these can be inverted, need to loop through columns checking whether they
  # are right if the other way up
  column_difference <- function(r_column, greta_column) {
    pos <- abs(r_column - greta_column)
    neg <- abs(r_column - (-1 * greta_column))
    if (sum(pos) < sum(neg)) {
      pos
    } else {
      neg
    }
  }

  greta_vectors <- grab(greta_out$vectors)
  difference <- vapply(seq_len(k),
    function(i) {
      column_difference(
        r_out$vectors[, i],
        greta_vectors[, i]
      )
    },
    FUN.VALUE = rep(1, k)
  )

  expect_true(all(as.vector(difference) < 1e-4))
})

test_that("ignored options are errored/warned about", {
  skip_if_not(check_tf_version())

  x <- ones(3, 3)
  expect_snapshot(error = TRUE,
    round(x, 2)
  )

  expect_snapshot_warning(
    chol(x, pivot = TRUE)
  )

  expect_snapshot_warning(
    chol2inv(x, LINPACK = TRUE)
  )

  expect_snapshot_warning(
    chol2inv(x, size = 1)
  )

  expect_snapshot_warning(
    rdist(x, compact = TRUE)
  )
})


test_that("incorrect dimensions are errored about", {
  skip_if_not(check_tf_version())

  x <- ones(3, 3, 3)
  y <- ones(3, 4)

  expect_snapshot(error = TRUE,
    t(x)
  )

  expect_snapshot(error = TRUE,
    aperm(x, 2:1)
  )

  expect_snapshot(error = TRUE,
    chol(x)
  )

  expect_snapshot(error = TRUE,
    chol(y)
  )

  expect_snapshot(error = TRUE,
    chol2symm(x)
  )

  expect_snapshot(error = TRUE,
    chol2symm(y)
  )

  expect_snapshot(error = TRUE,
    eigen(x)
  )

  expect_snapshot(error = TRUE,
    eigen(y)
  )

  expect_snapshot(error = TRUE,
    rdist(x, y)
  )
})

test_that("chol2symm inverts chol", {
  skip_if_not(check_tf_version())

  x <- rWishart(1, 10, diag(9))[, , 1]
  u <- chol(x)

  # check the R version
  expect_equal(x, chol2symm(u))

  # check the greta version
  x2 <- calculate(chol2symm(as_data(u)))[[1]]
  expect_equal(x2, x)
})
