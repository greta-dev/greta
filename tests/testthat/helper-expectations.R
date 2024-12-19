ga_to_mat <- function(ga){
  mat_nrow <- mat_ncol <- ncol(ga)
  matrix(ga, nrow = mat_nrow, ncol = mat_nrow, byrow = TRUE)
}
# ga_to_mat(calc_chol$chol_x)

get_upper_tri2 <- function(mat){
  mat[upper.tri(mat)]
}

get_lower_tri <- function(mat){
  mat[lower.tri(mat)]
}

expect_upper_tri <- function(object){
  act <- quasi_label(rlang::enquo(object), arg = "object")

  act$mat <- object[1,,]

  act$upper_tri <- get_upper_tri2(act$mat)
  act$lower_tri <- get_lower_tri(act$mat)

  all_lower_zero <- all(act$lower_tri == 0)
  all_upper_non_zero <- all(act$upper_tri != 0)
  is_upper_tri <- all_lower_zero && all_upper_non_zero
  if (is_upper_tri){
    succeed()
    return(invisible(act$val))
  }

  if (!all_lower_zero){
    vals <- glue::glue_collapse(glue::glue("{round(act$lower_tri, 3)}"), sep = " ")
    msg <- glue::glue("{act$lab} is not upper triangular. Values below the \\
    main diagonal are not all zero: {vals}")
  }

  if (!all_upper_non_zero){
    vals <- glue::glue_collapse(glue::glue("{round(act$upper_tri, 3)}"), sep = " ")
    msg <- glue::glue_collapse(glue::glue("{act$lab} is not upper triangular. Some values above \\
    the main diagonal contain zero: {vals}"))
  }

  fail(msg)

}

expect_square <- function(object){
  # 1. Capture object and label
  act <- quasi_label(rlang::enquo(object), arg = "object")

  # 2. Call expect()
  act$nrow <- dim(act$val[1,,])[1]
  act$ncol <- dim(act$val[1,,])[2]
  expect(
    ok = act$nrow == act$ncol,
    failure_message = glue::glue("{act$lab} has dim {act$nrow}x{act$ncol}, and is not square.")
  )

  # 3. Invisibly return the value
  invisible(act$val)

}

# expect_square(calc_chol$chol_x)
# expect_square(array(data = 1:9, c(1,3,3)))
# expect_square(array(data = 1:12, c(1,3,4)))


expect_symmetric <- function(object){
  # 1. Capture object and label
  act <- quasi_label(rlang::enquo(object), arg = "object")

  act$mat <- ga_to_mat(object)

  act$upper <- get_upper_tri2(act$mat)
  act$lower <- get_lower_tri(act$mat)

  # 2. Call expect()
  expect(
    ok = all.equal(act$upper,act$lower),
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
