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

matmul_model <- function(
  X,
  y,
  side = c("left", "right"),
  mutable = FALSE,
  via_op = FALSE
) {
  side <- match.arg(side)
  as_data_fun <- if (mutable) as_data_mutable else as_data
  w <- normal(0, 1, dim = c(ncol(X), 1))

  if (side == "left") {
    data <- if (via_op) as_data(X) * 1 else as_data_fun(X)
    product <- data %*% w
  } else {
    data <- if (via_op) as_data(t(X)) * 1 else as_data_fun(t(X))
    product <- t(w) %*% data
  }

  distribution(y) <- normal(product, 1)
  list(model = model(w), X = data, w = w)
}

log_prob_and_grad <- function(log_prob, free_state) {
  free_state <- tf$constant(free_state, dtype = tf_float())
  with(tf$GradientTape() %as% tape, {
    tape$watch(free_state)
    value <- log_prob(free_state)
  })
  list(
    value = as.array(value),
    grad = as.array(tape$gradient(value, free_state))
  )
}

matmul_y <- function(n, side) {
  y <- rnorm(n)
  if (side == "right") {
    y <- matrix(y, 1, n)
  }
  y
}

test_that("%*% with data gives the same log density, gradient and hessian as the operation path", {
  skip_if_not(check_tf_version())

  X <- matrix(rnorm(21), 7, 3)
  for (side in c("left", "right")) {
    y <- matmul_y(7, side)
    for (mutable in c(FALSE, TRUE)) {
      fit <- matmul_model(X, y, side, mutable = mutable)
      ref <- matmul_model(X, y, side, via_op = TRUE)

      for (n_chains in c(1, 4)) {
        fs <- matrix(rnorm(3 * n_chains), n_chains, 3)
        fit_fun <- \(s) fit$model$dag$tf_log_prob_function(s)$adjusted
        ref_fun <- \(s) ref$model$dag$tf_log_prob_function(s)$adjusted
        expect_equal(
          log_prob_and_grad(fit_fun, fs),
          log_prob_and_grad(ref_fun, fs),
          tolerance = 1e-10
        )
      }

      fs <- matrix(rnorm(3), 1, 3)
      expect_equal(
        log_prob_and_grad(\(s) fit$model$dag$one_row_log_prob(s)$adjusted, fs),
        log_prob_and_grad(\(s) ref$model$dag$one_row_log_prob(s)$adjusted, fs),
        tolerance = 1e-10
      )
      expect_equal(
        fit$model$dag$hessians(free_state = tf$Variable(fs)),
        ref$model$dag$hessians(free_state = tf$Variable(fs)),
        tolerance = 1e-10
      )
    }
  }
})

test_that("%*% with data gives the same log density and gradient in single precision", {
  skip_if_not(check_tf_version())
  withr::local_options(greta_tf_float = "float32")

  X <- matrix(rnorm(21), 7, 3)
  for (side in c("left", "right")) {
    y <- matmul_y(7, side)
    fit <- matmul_model(X, y, side)
    ref <- matmul_model(X, y, side, via_op = TRUE)
    fs <- matrix(rnorm(12), 4, 3)
    expect_equal(
      log_prob_and_grad(
        \(s) fit$model$dag$tf_log_prob_function(s)$adjusted,
        fs
      ),
      log_prob_and_grad(
        \(s) ref$model$dag$tf_log_prob_function(s)$adjusted,
        fs
      ),
      tolerance = 1e-4
    )
  }
})

test_that("calculate() of %*% with data matches the R product", {
  skip_if_not(check_tf_version())

  X_value <- matrix(rnorm(21), 7, 3)
  X2 <- matrix(rnorm(21), 7, 3)
  w0 <- matrix(rnorm(3), 3, 1)
  X <- as_data(X_value)
  w <- normal(0, 1, dim = c(3, 1))

  one <- calculate(X %*% w, values = list(w = w0))
  expect_equal(drop(one[[1]]), drop(X_value %*% w0), tolerance = 1e-10)

  three <- calculate(X %*% w, values = list(w = w0), nsim = 3)
  for (i in 1:3) {
    expect_equal(three[[1]][i, , ], drop(X_value %*% w0), tolerance = 1e-10)
  }

  swapped <- calculate(X %*% w, values = list(X = X2, w = w0))
  expect_equal(drop(swapped[[1]]), drop(X2 %*% w0), tolerance = 1e-10)
})

test_that("calculate() of %*% with a single-column data swaps the data", {
  skip_if_not(check_tf_version())

  X2 <- matrix(rnorm(7), 7, 1)
  w0 <- matrix(rnorm(1), 1, 1)
  X <- as_data(matrix(rnorm(7), 7, 1))
  w <- normal(0, 1, dim = c(1, 1))

  swapped <- calculate(X %*% w, values = list(X = X2, w = w0))
  expect_equal(drop(swapped[[1]]), drop(X2 %*% w0), tolerance = 1e-10)
})

test_that("calculate() of %*% with data takes each draw of an mcmc() run", {
  skip_if_not(check_tf_version())

  X_value <- matrix(rnorm(21), 7, 3)
  fixture <- matmul_model(X_value, rnorm(7))
  draws <- mcmc(
    fixture$model,
    n_samples = 10,
    warmup = 10,
    chains = 2,
    verbose = FALSE
  )

  products <- calculate(fixture$X %*% fixture$w, values = draws)
  for (chain in 1:2) {
    chain_products <- as.matrix(products[[chain]])
    chain_draws <- as.matrix(draws[[chain]])
    expect_identical(dim(chain_products), c(10L, 7L))
    for (i in 1:10) {
      expect_equal(
        unname(chain_products[i, ]),
        drop(X_value %*% chain_draws[i, ]),
        tolerance = 1e-10
      )
    }
  }
})

test_that("calculate() of %*% with drawn data that isn't a target draws the data", {
  skip_if_not(check_tf_version())

  X_value <- matrix(rnorm(21), 7, 3)
  w0 <- matrix(rnorm(3), 3, 1)
  X <- as_data(X_value)
  distribution(X) <- normal(0, 1)
  w <- normal(0, 1, dim = c(3, 1))

  for (nsim in c(3, 1)) {
    p <- calculate(X %*% w, values = list(w = w0), nsim = nsim)
    for (i in seq_len(nsim)) {
      expect_gt(max(abs(p[[1]][i, , ] - drop(X_value %*% w0))), 1e-6)
    }
  }
})

test_that("calculate() of %*% with drawn data and drawn parameters uses both draws", {
  skip_if_not(check_tf_version())

  X_value <- matrix(rnorm(21), 7, 3)
  X <- as_data(X_value)
  distribution(X) <- normal(0, 1)
  w <- normal(0, 1, dim = c(3, 1))
  y <- rnorm(7)
  distribution(y) <- normal(X %*% w, 1)
  m <- model(w)
  draws <- mcmc(m, n_samples = 10, warmup = 10, chains = 1, verbose = FALSE)

  p <- calculate(X %*% w, w, values = draws, nsim = 3)
  expect_identical(names(p), c("X %*% w", "w"))
  expect_identical(dim(p[[1]]), c(3L, 7L, 1L))
  expect_identical(dim(p$w), c(3L, 3L, 1L))
  for (i in 1:3) {
    expect_gt(
      max(abs(p[[1]][i, , ] - drop(X_value %*% p$w[i, , ]))),
      1e-6
    )
  }
})

test_that("simulate() and mcmc() work on %*% with data", {
  skip_if_not(check_tf_version())

  X <- as_data(matrix(rnorm(21), 7, 3))
  w <- normal(0, 1, dim = c(3, 1))
  y <- as_data(rnorm(7))
  distribution(y) <- normal(X %*% w, 1)
  m <- model(w)

  sims <- simulate(m, nsim = 5)
  expect_identical(dim(sims$y), c(5L, 7L, 1L))

  draws <- mcmc(m, n_samples = 10, warmup = 10, chains = 2, verbose = FALSE)
  expect_identical(dim(as.matrix(draws)), c(20L, 3L))
})

test_that("data_values<-() on %*% with mutable data matches a rebuilt model without retracing", {
  skip_if_not(check_tf_version())

  X <- matrix(rnorm(21), 7, 3)
  X2 <- matrix(rnorm(21), 7, 3)
  y <- rnorm(7)
  fs <- matrix(rnorm(12), 4, 3)

  fixture <- matmul_model(X, y, mutable = TRUE)
  log_prob <- \(s) fixture$model$dag$tf_log_prob_function(s)$adjusted
  log_prob_and_grad(log_prob, fs)
  traces_before <- trace_count(fixture$model$dag$tf_log_prob_function)

  data_values(fixture$model, fixture$X) <- X2

  rebuilt <- matmul_model(X2, y)
  expect_equal(
    log_prob_and_grad(log_prob, fs),
    log_prob_and_grad(
      \(s) rebuilt$model$dag$tf_log_prob_function(s)$adjusted,
      fs
    ),
    tolerance = 1e-10
  )
  expect_identical(
    trace_count(fixture$model$dag$tf_log_prob_function),
    traces_before
  )
})

test_that("as_tf_function() with %*% on a dummy input handles batch sizes 1 and 3", {
  skip_if_not(check_tf_version())

  A <- matrix(rnorm(21), 7, 3)
  f <- as_tf_function(\(a) t(a) %*% a, a = as_data(A))

  for (n_batch in c(1, 3)) {
    inputs <- array(rnorm(n_batch * 21), c(n_batch, 7, 3))
    out <- as.array(f(tf$constant(inputs, dtype = tf_float())))
    for (i in seq_len(n_batch)) {
      expect_equal(
        out[i, , ],
        t(inputs[i, , ]) %*% inputs[i, , ],
        tolerance = 1e-10
      )
    }
  }
})

test_that("one data node in two %*% products matches the operation path", {
  skip_if_not(check_tf_version())

  X_value <- matrix(rnorm(21), 7, 3)
  y <- rnorm(7)
  fs <- matrix(rnorm(24), 4, 6)

  two_products <- function(via_op) {
    X <- if (via_op) as_data(X_value) * 1 else as_data(X_value)
    w1 <- normal(0, 1, dim = c(3, 1))
    w2 <- normal(0, 1, dim = c(3, 1))
    distribution(y) <- normal(X %*% w1 + X %*% w2, 1)
    model(w1, w2)
  }
  fit <- two_products(FALSE)
  ref <- two_products(TRUE)

  expect_equal(
    log_prob_and_grad(\(s) fit$dag$tf_log_prob_function(s)$adjusted, fs),
    log_prob_and_grad(\(s) ref$dag$tf_log_prob_function(s)$adjusted, fs),
    tolerance = 1e-10
  )
})

test_that("%*% with one-row and one-column data matches the operation path", {
  skip_if_not(check_tf_version())

  shapes <- list(c(1, 3), c(7, 1))
  for (shape in shapes) {
    for (side in c("left", "right")) {
      X <- matrix(rnorm(prod(shape)), shape[1], shape[2])
      y <- matmul_y(shape[1], side)
      fit <- matmul_model(X, y, side)
      ref <- matmul_model(X, y, side, via_op = TRUE)
      fs <- matrix(rnorm(4 * shape[2]), 4, shape[2])
      expect_equal(
        log_prob_and_grad(
          \(s) fit$model$dag$tf_log_prob_function(s)$adjusted,
          fs
        ),
        log_prob_and_grad(
          \(s) ref$model$dag$tf_log_prob_function(s)$adjusted,
          fs
        ),
        tolerance = 1e-10
      )
    }
  }
})

test_that("%*% with data that is also a likelihood target matches the operation path", {
  skip_if_not(check_tf_version())

  X_value <- matrix(rnorm(21), 7, 3)
  y <- rnorm(7)
  fs <- matrix(rnorm(12), 4, 3)

  targeted <- function(via_op) {
    X <- as_data(X_value)
    distribution(X) <- normal(0, 1)
    w <- normal(0, 1, dim = c(3, 1))
    distribution(y) <- normal((if (via_op) X * 1 else X) %*% w, 1)
    model(w)
  }
  fit <- targeted(FALSE)
  ref <- targeted(TRUE)

  expect_equal(
    log_prob_and_grad(\(s) fit$dag$tf_log_prob_function(s)$adjusted, fs),
    log_prob_and_grad(\(s) ref$dag$tf_log_prob_function(s)$adjusted, fs),
    tolerance = 1e-10
  )
})

test_that("%*% with an integer matrix as data is cast to float (guard on data_node$tf())", {
  skip_if_not(check_tf_version())

  X <- matrix(1:21, 7)
  for (side in c("left", "right")) {
    y <- matmul_y(7, side)
    fit <- matmul_model(X, y, side)
    ref <- matmul_model(X, y, side, via_op = TRUE)
    fs <- matrix(rnorm(12), 4, 3)
    expect_equal(
      log_prob_and_grad(
        \(s) fit$model$dag$tf_log_prob_function(s)$adjusted,
        fs
      ),
      log_prob_and_grad(
        \(s) ref$model$dag$tf_log_prob_function(s)$adjusted,
        fs
      ),
      tolerance = 1e-10
    )
  }
})
