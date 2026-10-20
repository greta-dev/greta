test_that("arithmetic operators work as expected", {
  skip_if_not(check_tf_version())

  a <- randn(25, 4)
  b <- randn(25, 4)

  check_op(`-`, a)
  check_op(`-`, a, b)
  check_op(`+`, a, b)
  check_op(`*`, a, b)
  check_op(`/`, a, b)
  check_op(`^`, abs(a), b)
  check_op(`%%`, a, b)
  check_op(`%/%`, a, b)
  check_op(`%*%`, a, t(b))
})

test_that("arithmetic operators work as expected with arrays and scalars", {
  skip_if_not(check_tf_version())

  a <- randn(2, 1, 1)
  b <- 4

  check_op(`-`, a, b)
  check_op(`+`, a, b)
  check_op(`*`, a, b)
  check_op(`/`, a, b)
  check_op(`^`, abs(a), b)
  check_op(`%%`, a, b)
  check_op(`%/%`, a, b)
})

test_that("logical operators work as expected", {
  skip_if_not(check_tf_version())

  a <- randn(25, 4) > 0
  b <- randn(25, 4) > 0
  a[] <- as.integer(a[])
  b[] <- as.integer(b[])

  check_op(`!`, a, only = "data")
  check_op(`&`, a, b, only = "data")
  check_op(`|`, a, b, only = "data")
})

test_that("relational operators work as expected", {
  skip_if_not(check_tf_version())

  a <- randn(25, 4)
  b <- randn(25, 4)

  check_op(`<`, a, b)
  check_op(`>`, a, b)
  check_op(`<=`, a, b)
  check_op(`>=`, a, b)
  check_op(`==`, a, b)
  check_op(`!=`, a, b)
})

test_that("random strings of operators work as expected", {
  skip_if_not(check_tf_version())

  for (i in 1:10) {
    a <- randn(25, 4)
    b <- randn(25, 4)

    # generate a 5-deep random function of operations
    fun <- gen_opfun(
      5,
      ops = c(
        "+",
        "-",
        "*",
        "/",
        "&",
        "|",
        "<",
        ">"
      )
    )

    r_out <- fun(a, b)
    greta_out <- grab(fun(as_data(a), as_data(b)))

    # nas should be in the same places
    na_idx <- which(is.na(r_out))
    expect_identical(
      which(is.na(greta_out)),
      na_idx
    )

    # the values can get quite extreme, so scale differences, omit NAs before
    # and after doing scaling, tolerate a 1% variation
    difference <- abs(r_out - greta_out) / abs(greta_out)
    difference <- na.omit(difference[-na_idx])
    expect_true(all(difference < 1e-2))
  }
})

test_that("%*% errors informatively", {
  skip_if_not(check_tf_version())

  a <- ones(3, 4)
  b <- ones(1, 4)
  c <- ones(2, 2, 2)

  expect_snapshot(error = TRUE, a %*% b)

  expect_snapshot(error = TRUE, a %*% c)
})

test_that("%*% works when one is a non-greta array", {
  skip_if_not(check_tf_version())
  x <- matrix(1, 2, 3)
  y <- rep(1, 3)

  expect_snapshot(x %*% y)
  expect_snapshot(x %*% as_data(y))
  expect_snapshot(as_data(x) %*% y)
  expect_snapshot(as_data(x) %*% as_data(y))

  dim1 <- dim(x %*% as_data(y))
  dim2 <- dim(as_data(x) %*% y)
  dim3 <- dim(as_data(x) %*% as_data(y))

  expect_true(all(dim1 == dim2 & dim1 == dim3 & dim2 == dim3))

  res_1 <- x %*% as_data(y)
  res_2 <- as_data(x) %*% y
  res_3 <- as_data(x) %*% as_data(y)

  expect_snapshot(calculate(res_1, nsim = 1))
  expect_snapshot(calculate(res_2, nsim = 1))
  expect_snapshot(calculate(res_3, nsim = 1))
})

test_that("%*% with data multiplies the data's single copy, not a copy per chain", {
  skip_if_not(check_tf_version())

  for (data_function in list(as_data, as_data_mutable)) {
    X <- data_function(matrix(rnorm(21), 7, 3))
    w <- normal(0, 1, dim = c(3, 1))
    y <- rnorm(7)
    distribution(y) <- normal(X %*% w, 1)
    m <- model(w)

    graph <- m$dag$tf_log_prob_function$get_concrete_function()$graph
    ops <- graph$get_operations()
    op_types <- vapply(ops, \(op) op$type, character(1))
    input_shapes <- function(op) {
      lapply(op$inputs, \(input) unlist(input$shape$as_list()))
    }
    has_shape <- function(op, shape) {
      any(vapply(input_shapes(op), identical, logical(1), as.integer(shape)))
    }

    einsum_reads_data <- any(vapply(
      ops[op_types == "Einsum"],
      has_shape,
      logical(1),
      shape = c(7, 3)
    ))
    data_tiles_unused <- all(vapply(
      ops[op_types == "Tile" & vapply(ops, has_shape, logical(1), c(1, 7, 3))],
      \(op) length(op$outputs[[1]]$consumers()) == 0,
      logical(1)
    ))

    expect_true(einsum_reads_data)
    expect_true(data_tiles_unused)
  }
})

test_that("%*% with data and nsim = 1 works in single precision", {
  skip_if_not(check_tf_version())
  withr::local_options(greta_tf_float = "float32")

  X_value <- matrix(rnorm(21), 7, 3)
  X2 <- matrix(rnorm(21), 7, 3)
  X <- as_data(X_value)
  w <- normal(0, 1, dim = c(3, 1))
  out <- NULL
  expect_no_error(out <- calculate(X %*% w, w, values = list(X = X2), nsim = 1))
  skip_if(is.null(out))
  expect_equal(out[[1]][1, , ], drop(X2 %*% out$w[1, , ]), tolerance = 1e-4)
})
