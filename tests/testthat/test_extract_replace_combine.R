context('extract/replace/combine')

test_that('extract works like R', {

  source('helpers.R')

  a <- randn(10)
  b <- randn(10, 1)
  c <- randn(10, 5)
  d <- randn(2, 2, 2)

  # Can extract with a vector, regardless of dimension
  check_expr(a[1:6], 'a')
  check_expr(b[1:6], 'b')
  check_expr(c[1:6], 'c')
  check_expr(d[1:6], 'd')

  # can extract first column, regardless of dimension
  check_expr(b[1:6, ], 'b')
  check_expr(b[1:6, 1], 'b')

  check_expr(c[1:6, ], 'c')
  check_expr(c[1:6, 1], 'c')
  check_expr(d[1:2, , ], 'd')
  check_expr(d[1:2, 1, , drop = FALSE], 'd')

  # can extract with negative dimensions
  check_expr(a[3:1], 'a')
  check_expr(d[2:1, , 1:2], 'd')

  # can extract with logicals
  check_expr(a[c(TRUE, FALSE, TRUE)], 'a')

  # can extract with a mix of numerics and logicals
  check_expr(d[2:1, , c(TRUE, FALSE), drop = FALSE], 'd')

  # can extract with missing entries in various places
  check_expr(d[, , 2:1], 'd')
  check_expr(d[, 2:1, ], 'd')
  check_expr(d[2:1, , ], 'd')

  # can extract single elements without dropping dimensions
  check_expr(d[, , 1, drop = FALSE], 'd')
  check_expr(d[, 1, , drop = FALSE], 'd')
  check_expr(d[1, , , drop = FALSE], 'd')

  # can do empty extracts
  check_expr(a[], 'a')
  check_expr(b[], 'b')
  check_expr(c[], 'c')
  check_expr(d[], 'd')

})

test_that('replace works like R', {

  source('helpers.R')

  # check using expressions, and comparing the whole object to which replacement
  # was applied

  # Can replace with a vector, regardless of dimension
  x <- randn(10)
  check_expr({x[1:6] <- seq_len(6); x})
  x <- randn(10, 1)
  check_expr({x[1:6] <- seq_len(6); x})
  x <- randn(10, 5)
  check_expr({x[1:6] <- seq_len(6); x})
  x <- randn(2, 2, 2)
  check_expr({x[1:6] <- seq_len(6); x})

  # can replace first column, regardless of dimension
  x <- randn(10, 1)
  check_expr({x[1:6, 1] <- seq_len(6 * 1); x})
  x <- randn(10, 5)
  check_expr({x[1:6, 1] <- seq_len(6 * 1); x})
  x <- randn(2, 2, 2)
  check_expr({x[1:2, 1, ] <- seq_len(2 * 1 * 2); x})

  # can replace a chunk, regardless of dimension
  x <- randn(10, 1)
  check_expr({x[1:6, ] <- seq_len(6); x})
  x <- randn(10, 5)
  check_expr({x[1:6, ] <- seq_len(6 * 5); x})
  x <- randn(10, 2, 2)
  check_expr({x[1:2, , ] <- seq_len(2 * 2 * 2); x})

  # can replace with negative dimensions
  x <- randn(10)
  check_expr({x[3:1] <- seq_len(3); x})
  x <- randn(10, 2, 2)
  check_expr({x[2:1, , 1:2] <- seq_len(2 * 2 * 2); x})

  # can replace with logicals (the logical vector is repeated 3 1/3 times)
  x <- randn(10)
  check_expr({x[c(TRUE, FALSE, TRUE)] <- seq_len(7); x})

  # can extract with a mix of numerics and logicals
  x <- randn(10, 2, 2)
  check_expr({x[2:1, , c(TRUE, FALSE)] <- seq_len(4); x})

  # can assign with missing entries in various places
  x <- randn(10, 2, 2)
  check_expr({x[, , 1:2] <- seq_len(10 * 2 * 2); x})
  x <- randn(10, 2, 2)
  check_expr({x[, 1:2, ] <- seq_len(10 * 2 * 2); x})
  x <- randn(10, 2, 2)
  check_expr({x[1:2, , ] <- seq_len(2 * 2 * 2); x})

  # can assign single elements without dropping dimensions
  x <- randn(10, 2, 2)
  check_expr({x[1, , ] <- seq_len(1 * 2 * 2); x})
  x <- randn(10, 2, 2)
  check_expr({x[, 1, ] <- seq_len(10 * 1 * 2); x})
  x <- randn(10, 2, 2)
  check_expr({x[, , 1] <- seq_len(10 * 2 * 1); x})

  # can do full replacements
  x <- randn(10)
  check_expr({x[] <- seq_len(10); x})
  x <- randn(10, 1)
  check_expr({x[] <- seq_len(10); x})
  x <- randn(10, 5)
  check_expr({x[] <- seq_len(10 * 5); x})
  x <- randn(2, 2, 2)
  check_expr({x[] <- seq_len(2 * 2 * 2); x})

  # can replace multiple entries with one;
  x <- randn(10)
  check_expr({x[1:3] <- 1; x})
  x <- randn(10, 1)
  check_expr({x[1:3, 1] <- 1; x})
  x <- randn(10, 5)
  check_expr({x[1:3, ] <- 1; x})
  x <- randn(2, 2, 2)
  check_expr({x[1:2, , ] <- 1; x})

  # can replace multiple entries with a vector (with factorizing length)
  x <- randn(10)
  check_expr({x[1:4] <- seq_len(2); x})
  x <- randn(10, 1)
  check_expr({x[1:4, 1] <- seq_len(2); x})
  x <- randn(10, 5)
  check_expr({x[1:9, ] <- seq_len(3); x})
  x <- randn(2, 2, 2)
  check_expr({x[1:2, , 1] <- seq_len(2); x})

  # replace with a greta array (e.g. with subset of self)
  # check_expr seems to balls this one up, so do it longhand:
  x <- randn(10, 1)
  ga_x <- as_data(x)
  x[1:6, ] <- x[6:1, ]
  ga_x[1:6, ] <- ga_x[6:1, ]
  greta_out <- as.vector(grab(ga_x))
  difference <- abs(x - greta_out)
  expect_true(all(difference < 1e4))

})
