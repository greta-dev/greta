ga_to_mat <- function(ga) {
  mat_nrow <- mat_ncol <- ncol(ga)
  matrix(ga, nrow = mat_nrow, ncol = mat_nrow, byrow = TRUE)
}
# ga_to_mat(calc_chol$chol_x)

get_upper_tri2 <- function(mat) {
  mat[upper.tri(mat)]
}

get_lower_tri <- function(mat) {
  mat[lower.tri(mat)]
}

expect_upper_tri <- function(object) {
  act <- quasi_label(rlang::enquo(object), arg = "object")

  act$mat <- object[1, , ]

  act$upper_tri <- get_upper_tri2(act$mat)
  act$lower_tri <- get_lower_tri(act$mat)

  all_lower_zero <- all(act$lower_tri == 0)
  all_upper_non_zero <- all(act$upper_tri != 0)
  is_upper_tri <- all_lower_zero && all_upper_non_zero
  if (is_upper_tri) {
    succeed()
    return(invisible(act$val))
  }

  if (!all_lower_zero) {
    vals <- glue::glue_collapse(
      glue::glue("{round(act$lower_tri, 3)}"),
      sep = " "
    )
    msg <- glue::glue(
      "{act$lab} is not upper triangular. Values below the \\
    main diagonal are not all zero: {vals}"
    )
  }

  if (!all_upper_non_zero) {
    vals <- glue::glue_collapse(
      glue::glue("{round(act$upper_tri, 3)}"),
      sep = " "
    )
    msg <- glue::glue_collapse(glue::glue(
      "{act$lab} is not upper triangular. Some values above \\
    the main diagonal contain zero: {vals}"
    ))
  }

  fail(msg)
}

expect_square <- function(object) {
  # 1. Capture object and label
  act <- quasi_label(rlang::enquo(object), arg = "object")

  # 2. Call expect()
  act$nrow <- dim(act$val[1, , ])[1]
  act$ncol <- dim(act$val[1, , ])[2]
  expect(
    ok = act$nrow == act$ncol,
    failure_message = glue::glue(
      "{act$lab} has dim {act$nrow}x{act$ncol}, and is not square."
    )
  )

  # 3. Invisibly return the value
  invisible(act$val)
}

# expect_square(calc_chol$chol_x)
# expect_square(array(data = 1:9, c(1,3,3)))
# expect_square(array(data = 1:12, c(1,3,4)))

expect_symmetric <- function(object) {
  # 1. Capture object and label
  act <- quasi_label(rlang::enquo(object), arg = "object")

  act$mat <- ga_to_mat(object)

  act$upper <- get_upper_tri2(act$mat)
  act$lower <- get_lower_tri(act$mat)

  # 2. Call expect()
  expect(
    ok = all.equal(act$upper, act$lower),
    failure_message = glue::glue("{act$lab} is not symmetric")
  )

  # 3. Invisibly return the value
  invisible(act$val)
}

# xmat <- calculate(x, nsim = 1)[[1]] |> ga_to_mat()
#
# xmat
#
# expect_symmetric(calculate(x, nsim = 1)[[1]])
#
# all.equal(get_lower_tri(xmat),get_upper_tri2(xmat))

# expect a model's log density and gradient at `free_state` to match those of
# the `expected` model, within a `tolerance` as in all.equal()
expect_same_log_prob <- function(
  object,
  expected,
  free_state,
  tolerance = 1e-10,
  log_prob = adjusted_log_prob
) {
  act <- quasi_label(rlang::enquo(object))
  exp <- quasi_label(rlang::enquo(expected))

  act_log_prob <- log_prob_and_grad(\(s) log_prob(act$val, s), free_state)
  exp_log_prob <- log_prob_and_grad(\(s) log_prob(exp$val, s), free_state)
  differences <- all.equal(exp_log_prob, act_log_prob, tolerance = tolerance)

  if (isTRUE(differences)) {
    pass()
  } else {
    fail(c(
      sprintf(
        "Expected %s to have the same log density and gradient as %s.",
        act$lab,
        exp$lab
      ),
      differences
    ))
  }

  invisible(act$val)
}

# expect the model of `operation_path`, from matmul_operation_path(), built with
# its X wrapped by `wrap`, to give the same log density and gradient as on the
# tf$matmul path, on each number of chains in `n_chains`. On one chain, also
# through one_row_adjusted_log_prob() if `check_one_row`, and the same hessian
# if `check_hessian`
expect_matmul_matches_operation_path <- function(
  operation_path,
  wrap,
  n_chains,
  tolerance = 1e-10,
  check_one_row = FALSE,
  check_hessian = FALSE
) {
  reference <- operation_path$model
  fixture <- matmul_model(
    operation_path$X,
    operation_path$y,
    operation_path$side,
    operation_path$k,
    wrap = wrap
  )
  fit <- fixture$model

  n_free <- ncol(operation_path$X) * operation_path$k
  free_states <- lapply(n_chains, \(n) matrix(rnorm(n * n_free), n))
  one_chain <- matrix(rnorm(n_free), 1)

  expect_same_log_prob_at <- function(free_state) {
    expect_same_log_prob(fit, reference, free_state, tolerance = tolerance)
  }
  lapply(free_states, expect_same_log_prob_at)

  if (check_one_row) {
    expect_same_log_prob(
      fit,
      reference,
      one_chain,
      tolerance = tolerance,
      log_prob = one_row_adjusted_log_prob
    )
  }

  if (check_hessian) {
    fit_hessian <- fit$dag$hessians(free_state = tf$Variable(one_chain))
    reference_hessian <- reference$dag$hessians(
      free_state = tf$Variable(one_chain)
    )
    expect_equal(fit_hessian, reference_hessian, tolerance = tolerance)
  }

  invisible(operation_path)
}

# expect every draw, a row of `products`, to differ from the same row of
# `fixed_products` by more than 1e-6 in at least one element
expect_draws_differ_from <- function(products, fixed_products) {
  largest_differences <- apply(abs(products - fixed_products), 1, max)
  expect_gt(min(largest_differences), 1e-6)
}

# expect each draw in one chain of calculate(X %*% w, values = draws) to be X
# times that chain's draw of w
expect_chain_products <- function(products, draws, X, chain) {
  chain_products <- draws_as_rows(products[[chain]])
  expected_products <- draws_as_rows(draws[[chain]]) %*% t(X)
  expect_equal(chain_products, expected_products, tolerance = 1e-10)
}

# expect each of `nsim` draws of X %*% w from calculate(), with w fixed at `w0`,
# to differ from the product of X's own value and w0, as X is drawn
expect_drawn_data_products <- function(drawn, w0, nsim) {
  # calculate() finds the greta arrays named in `values` by their names here
  X <- drawn$X
  w <- drawn$w
  draws <- calculate(X %*% w, values = list(w = w0), nsim = nsim)[[1]]
  products <- draws_as_rows(draws)
  fixed_data_product <- drop(drawn$X_value %*% w0)
  fixed_data_products <- matrix(
    fixed_data_product,
    nrow = nsim,
    ncol = length(fixed_data_product),
    byrow = TRUE
  )
  expect_draws_differ_from(products, fixed_data_products)
}

# expect `f`, a function from as_tf_function() of t(a) %*% a for a 7 x 3 matrix
# a, to give each product for a batch of `n_batch` matrices
expect_batch_crossprods <- function(f, n_batch) {
  inputs <- array(rnorm(n_batch * 21), c(n_batch, 7, 3))
  outputs <- as.array(f(tf$constant(inputs, dtype = tf_float())))
  expected_crossprods <- batch_crossprods(inputs)
  expect_equal(outputs, expected_crossprods, tolerance = 1e-10)
}
