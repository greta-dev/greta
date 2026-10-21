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

# test code runs in greta's namespace, where base R's %*% finds greta's methods
# whether or not they are registered. A function enclosed by baseenv() sees only
# base R, as code compiled before greta was attached does
test_that("base R's %*% works on greta arrays on either side", {
  skip_if_not(check_tf_version())

  base_matrix_multiply <- function(x, y) x %*% y
  environment(base_matrix_multiply) <- baseenv()

  a <- randn(5, 4)
  b <- randn(4, 3)

  greta_out1 <- base_matrix_multiply(as_data(a), b)
  greta_out2 <- base_matrix_multiply(a, as_data(b))

  expect_s3_class(greta_out1, "greta_array")
  expect_s3_class(greta_out2, "greta_array")

  r_out <- a %*% b
  compare_op(r_out, grab(greta_out1))
  compare_op(r_out, grab(greta_out2))
  check_op(base_matrix_multiply, a, b)
})

test_that("greta's %*% is base R's own", {
  expect_identical(greta::`%*%`, base::`%*%`)
})

test_that("data_operand_side() names the operand that is data", {
  skip_if_not(check_tf_version())

  data <- as_data(matrix(1, 2, 2))
  variable <- normal(0, 1, dim = c(2, 2))

  expect_identical(data_operand_side(data, variable), "left")
  expect_identical(data_operand_side(variable, data), "right")
  expect_null(data_operand_side(data, data))
  expect_null(data_operand_side(variable, variable))
})

test_that("%*% with data multiplies the data's single copy, not a copy per chain", {
  skip_if_not(check_tf_version())

  constant_graph <- matmul_log_prob_graph(as_data)
  mutable_graph <- matmul_log_prob_graph(as_data_mutable)
  constant_ops <- graph_ops_by_type(constant_graph)
  mutable_ops <- graph_ops_by_type(mutable_graph)

  expect_true(einsum_reads_shape(constant_ops, c(7, 3)))
  expect_true(einsum_reads_shape(mutable_ops, c(7, 3)))
  expect_true(tiles_of_shape_unused(constant_ops, c(1, 7, 3)))
  expect_true(tiles_of_shape_unused(mutable_ops, c(1, 7, 3)))
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

  product_draw <- out[[1]][1, , ]
  w_draw <- out$w[1, , ]
  expected_product <- drop(X2 %*% w_draw)
  expect_equal(product_draw, expected_product, tolerance = 1e-4)
})

test_that("%*% with data gives the same log density, gradient and hessian as the operation path", {
  skip_if_not(check_tf_version())

  X <- matrix(rnorm(21), 7, 3)
  left <- matmul_operation_path(X, side = "left")
  right <- matmul_operation_path(X, side = "right")

  expect_matmul_matches_operation_path(
    left,
    wrap = as_data,
    n_chains = c(1, 4),
    check_one_row = TRUE,
    check_hessian = TRUE
  )
  expect_matmul_matches_operation_path(
    left,
    wrap = as_data_mutable,
    n_chains = c(1, 4),
    check_one_row = TRUE,
    check_hessian = TRUE
  )
  expect_matmul_matches_operation_path(
    right,
    wrap = as_data,
    n_chains = c(1, 4),
    check_one_row = TRUE,
    check_hessian = TRUE
  )
  expect_matmul_matches_operation_path(
    right,
    wrap = as_data_mutable,
    n_chains = c(1, 4),
    check_one_row = TRUE,
    check_hessian = TRUE
  )
})

test_that("%*% with data gives the same log density and gradient in single precision", {
  skip_if_not(check_tf_version())
  withr::local_options(greta_tf_float = "float32")

  X <- matrix(rnorm(21), 7, 3)
  left <- matmul_operation_path(X, side = "left")
  right <- matmul_operation_path(X, side = "right")

  expect_matmul_matches_operation_path(
    left,
    wrap = as_data,
    n_chains = 4,
    tolerance = 1e-4
  )
  expect_matmul_matches_operation_path(
    right,
    wrap = as_data,
    n_chains = 4,
    tolerance = 1e-4
  )
})

test_that("%*% with data of other shapes matches the operation path", {
  skip_if_not(check_tf_version())

  one_row_matrix <- matrix(rnorm(3), 1, 3)
  one_column_matrix <- matrix(rnorm(7), 7, 1)
  integer_matrix <- matrix(1:21, 7)
  X <- matrix(rnorm(21), 7, 3)

  one_row_left <- matmul_operation_path(one_row_matrix, side = "left")
  one_row_right <- matmul_operation_path(one_row_matrix, side = "right")
  one_column_left <- matmul_operation_path(one_column_matrix, side = "left")
  one_column_right <- matmul_operation_path(one_column_matrix, side = "right")
  integer_left <- matmul_operation_path(integer_matrix, side = "left")
  integer_right <- matmul_operation_path(integer_matrix, side = "right")
  two_columns_left <- matmul_operation_path(X, side = "left", k = 2)
  two_columns_right <- matmul_operation_path(X, side = "right", k = 2)

  expect_matmul_matches_operation_path(one_row_left, as_data, n_chains = 4)
  expect_matmul_matches_operation_path(one_row_right, as_data, n_chains = 4)
  expect_matmul_matches_operation_path(one_column_left, as_data, n_chains = 4)
  expect_matmul_matches_operation_path(one_column_right, as_data, n_chains = 4)
  expect_matmul_matches_operation_path(integer_left, as_data, n_chains = 4)
  expect_matmul_matches_operation_path(integer_right, as_data, n_chains = 4)
  expect_matmul_matches_operation_path(two_columns_left, as_data, n_chains = 4)
  expect_matmul_matches_operation_path(two_columns_right, as_data, n_chains = 4)
})

test_that("expect_same_log_prob() fails when the log densities differ", {
  skip_if_not(check_tf_version())

  X <- matrix(rnorm(21), 7, 3)
  y <- matrix(rnorm(7), 7)
  fit <- matmul_model(X, y)$model
  other_data <- matmul_model(X + 1, y)$model
  free_state <- matrix(rnorm(12), 4, 3)

  expect_success(expect_same_log_prob(fit, fit, free_state))
  expect_failure(
    expect_same_log_prob(fit, other_data, free_state),
    "same log density and gradient"
  )
})

test_that("calculate() of %*% with data matches the R product", {
  skip_if_not(check_tf_version())

  X_value <- matrix(rnorm(21), 7, 3)
  X2 <- matrix(rnorm(21), 7, 3)
  w0 <- matrix(rnorm(3), 3, 1)
  X <- as_data(X_value)
  w <- normal(0, 1, dim = c(3, 1))

  expected_product <- drop(X_value %*% w0)

  one <- calculate(X %*% w, values = list(w = w0))
  one_product <- drop(one[[1]])
  expect_equal(one_product, expected_product, tolerance = 1e-10)

  three <- calculate(X %*% w, values = list(w = w0), nsim = 3)
  three_products <- draws_as_rows(three[[1]])
  expected_products <- matrix(expected_product, 3, 7, byrow = TRUE)
  expect_equal(three_products, expected_products, tolerance = 1e-10)

  swapped <- calculate(X %*% w, values = list(X = X2, w = w0))
  swapped_product <- drop(swapped[[1]])
  expected_swapped_product <- drop(X2 %*% w0)
  expect_equal(swapped_product, expected_swapped_product, tolerance = 1e-10)
})

test_that("calculate() of %*% with a single-column data swaps the data", {
  skip_if_not(check_tf_version())

  X2 <- matrix(rnorm(7), 7, 1)
  w0 <- matrix(rnorm(1), 1, 1)
  X <- as_data(matrix(rnorm(7), 7, 1))
  w <- normal(0, 1, dim = c(1, 1))

  swapped <- calculate(X %*% w, values = list(X = X2, w = w0))
  swapped_product <- drop(swapped[[1]])
  expected_product <- drop(X2 %*% w0)
  expect_equal(swapped_product, expected_product, tolerance = 1e-10)
})

test_that("calculate() of %*% with data takes each draw of an mcmc() run", {
  skip_if_not(check_tf_version())

  X_value <- matrix(rnorm(21), 7, 3)
  fixture <- matmul_model(X_value)
  draws <- mcmc(
    fixture$model,
    n_samples = 10,
    warmup = 10,
    chains = 2,
    verbose = FALSE
  )
  expect_identical(dim(as.matrix(draws)), c(20L, 3L))

  products <- calculate(fixture$X %*% fixture$w, values = draws)
  expect_chain_products(products, draws, X_value, chain = 1)
  expect_chain_products(products, draws, X_value, chain = 2)
})

drawn_data <- function() {
  X_value <- matrix(rnorm(21), 7, 3)
  X <- as_data(X_value)
  distribution(X) <- normal(0, 1)
  list(X_value = X_value, X = X, w = normal(0, 1, dim = c(3, 1)))
}

test_that("calculate() of %*% with drawn data that isn't a target draws the data", {
  skip_if_not(check_tf_version())

  drawn <- drawn_data()
  w0 <- matrix(rnorm(3), 3, 1)

  expect_drawn_data_products(drawn, w0, nsim = 3)
  expect_drawn_data_products(drawn, w0, nsim = 1)
})

test_that("calculate() of %*% with drawn data and drawn parameters uses both draws", {
  skip_if_not(check_tf_version())

  drawn <- drawn_data()
  X <- drawn$X
  w <- drawn$w
  y <- rnorm(7)
  distribution(y) <- normal(X %*% w, 1)
  m <- model(w)
  draws <- mcmc(m, n_samples = 10, warmup = 10, chains = 1, verbose = FALSE)

  p <- calculate(X %*% w, w, values = draws, nsim = 3)
  expect_identical(names(p), c("X %*% w", "w"))
  expect_identical(dim(p[[1]]), c(3L, 7L, 1L))
  expect_identical(dim(p$w), c(3L, 3L, 1L))

  products <- draws_as_rows(p[[1]])
  fixed_data_products <- draws_as_rows(p$w) %*% t(drawn$X_value)
  expect_draws_differ_from(products, fixed_data_products)
})

test_that("simulate() works on %*% with data", {
  skip_if_not(check_tf_version())

  X <- as_data(matrix(rnorm(21), 7, 3))
  w <- normal(0, 1, dim = c(3, 1))
  y <- as_data(rnorm(7))
  distribution(y) <- normal(X %*% w, 1)
  m <- model(w)

  sims <- simulate(m, nsim = 5)
  expect_identical(dim(sims$y), c(5L, 7L, 1L))
})

test_that("data_values<-() on %*% with mutable data matches a rebuilt model without retracing", {
  skip_if_not(check_tf_version())

  X <- matrix(rnorm(21), 7, 3)
  X2 <- matrix(rnorm(21), 7, 3)
  y <- rnorm(7)
  fs <- matrix(rnorm(12), 4, 3)

  fixture <- matmul_model(X, y, wrap = as_data_mutable)
  log_prob_and_grad(\(s) adjusted_log_prob(fixture$model, s), fs)
  traces_before <- trace_count(fixture$model$dag$tf_log_prob_function)

  data_values(fixture$model, fixture$X) <- X2

  rebuilt <- matmul_model(X2, y)
  expect_same_log_prob(fixture$model, rebuilt$model, fs)
  expect_identical(
    trace_count(fixture$model$dag$tf_log_prob_function),
    traces_before
  )
})

test_that("as_tf_function() with %*% on a dummy input handles batch sizes 1 and 3", {
  skip_if_not(check_tf_version())

  A <- matrix(rnorm(21), 7, 3)
  f <- as_tf_function(\(a) t(a) %*% a, a = as_data(A))

  expect_batch_crossprods(f, n_batch = 1)
  expect_batch_crossprods(f, n_batch = 3)
})

test_that("one data node in two %*% products matches the operation path", {
  skip_if_not(check_tf_version())

  X_value <- matrix(rnorm(21), 7, 3)
  y <- rnorm(7)
  fs <- matrix(rnorm(24), 4, 6)

  two_products <- function(wrap) {
    X <- wrap(X_value)
    w1 <- normal(0, 1, dim = c(3, 1))
    w2 <- normal(0, 1, dim = c(3, 1))
    distribution(y) <- normal(X %*% w1 + X %*% w2, 1)
    model(w1, w2)
  }

  expect_same_log_prob(
    two_products(as_data),
    two_products(\(d) as_data(d) * 1),
    fs
  )
})

test_that("%*% with data that is also a likelihood target matches the operation path", {
  skip_if_not(check_tf_version())

  X_value <- matrix(rnorm(21), 7, 3)
  y <- rnorm(7)
  fs <- matrix(rnorm(12), 4, 3)

  targeted <- function(wrap) {
    X <- as_data(X_value)
    distribution(X) <- normal(0, 1)
    w <- normal(0, 1, dim = c(3, 1))
    distribution(y) <- normal(wrap(X) %*% w, 1)
    model(w)
  }

  expect_same_log_prob(targeted(identity), targeted(\(d) d * 1), fs)
})
