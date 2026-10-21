#' @name operators
#'
#' @title arithmetic, logical and relational operators for greta arrays
#'
#' @description This is a list of currently implemented arithmetic, logical and
#'   relational operators to combine greta arrays into probabilistic models.
#'   Also see [functions] and [transforms].
#'
#' @section Usage: \preformatted{
#'  # arithmetic operators
#'  -x
#'  x + y
#'  x - y
#'  x * y
#'  x / y
#'  x ^ y
#'  x \%\% y
#'  x \%/\% y
#'  x \%*\% y
#'
#'  # logical operators
#'  !x
#'  x & y
#'  x | y
#'
#'  # relational operators
#'  x < y
#'  x > y
#'  x <= y
#'  x >= y
#'  x == y
#'  x != y
#'  }
#'
#' @details greta's operators are used just like R's the standard arithmetic,
#'   logical and relational operators, but they return other greta arrays. Since
#'   the operations are only carried during sampling, the greta array objects
#'   have unknown values.
#'
#' @return A `greta_array`, the result of applying the operator.
#' @examples
#' \dontrun{
#'
#' x <- as_data(-1:12)
#'
#' # arithmetic
#' a <- x + 1
#' b <- 2 * x + 3
#' c <- x %% 2
#' d <- x %/% 5
#'
#' # logical
#' e <- (x > 1) | (x < 1)
#' f <- e & (x < 2)
#' g <- !f
#'
#' # relational
#' h <- x < 1
#' i <- (-x) >= x
#' j <- h == x
#' }
NULL

# use S3 dispatch to apply the operators
#' @export
`+.greta_array` <- function(e1, e2) {
  check_dims(e1, e2)
  op("add", e1, e2, tf_operation = "tf$add", expand_scalars = TRUE)
}

#' @export
`-.greta_array` <- function(e1, e2) {
  # handle unary minus
  if (missing(e2)) {
    op("minus", e1, tf_operation = "tf$negative")
  } else {
    check_dims(e1, e2)
    op("subtract", e1, e2, tf_operation = "tf$subtract", expand_scalars = TRUE)
  }
}

#' @export
`*.greta_array` <- function(e1, e2) {
  check_dims(e1, e2)
  op("multiply", e1, e2, tf_operation = "tf$multiply", expand_scalars = TRUE)
}

#' @export
`/.greta_array` <- function(e1, e2) {
  check_dims(e1, e2)
  op("divide", e1, e2, tf_operation = "tf$truediv", expand_scalars = TRUE)
}

#' @export
`^.greta_array` <- function(e1, e2) {
  check_dims(e1, e2)
  op("power", e1, e2, tf_operation = "tf$pow", expand_scalars = TRUE)
}

#' @export
`%%.greta_array` <- function(e1, e2) {
  check_dims(e1, e2)
  op("`modulo`", e1, e2, tf_operation = "tf$math$mod", expand_scalars = TRUE)
}

#' @export
`%/%.greta_array` <- function(e1, e2) {
  # nolint
  check_dims(e1, e2)
  op(
    "`integer divide`",
    e1,
    e2,
    tf_operation = "tf$math$floordiv",
    expand_scalars = TRUE
  )
}

#' @name %*%
#' @rdname overloaded
#' @usage x \%*\% y
#' @export %*%
NULL

# base R's own %*%, exported so that packages importing %*% from greta (such
# as greta.gp) still find it
`%*%` <- base::`%*%`

# R's matrixOps group generic dispatches here when either operand is a greta
# array, so the other may be an R matrix or vector. The method is registered
# against base's %*% because greta's own %*% binding above, there for packages
# such as greta.gp that import %*% from greta, would make a plain S3method()
# file it in greta's S3 table, where base R's %*% called from outside greta
# does not look
#' @rawNamespace S3method(base::"%*%", greta_array)
#' @exportS3Method NULL
`%*%.greta_array` <- function(x, y) {
  # nolint
  x <- as.greta_array(x)
  y <- as.greta_array(y)

  check_both_2d(x, y, call = matmul_error_call())
  check_compatible_dimensions(x, y, call = matmul_error_call())

  data_side <- data_operand_side(x, y)
  if (is.null(data_side)) {
    return(op(
      "matrix multiply",
      x,
      y,
      dim = c(nrow(x), ncol(y)),
      tf_operation = "tf$matmul"
    ))
  }

  op(
    "matrix multiply",
    x,
    y,
    dim = c(nrow(x), ncol(y)),
    unbatched_data = TRUE,
    operation_args = list(data_side = data_side),
    tf_operation = "tf_matmul_data"
  )
}

# the call that reached %*%.greta_array, as x %*% y: R's dispatch calls the
# method by its own name. Passed as a check's `call`, it is built only when the
# check fails
matmul_error_call <- function() {
  error_call <- rlang::caller_call()
  error_call[[1]] <- as.name("%*%")
  error_call
}

# logical operators
#' @export
`!.greta_array` <- function(e1) {
  op("not", e1, tf_operation = "tf_not")
}

#' @export
`&.greta_array` <- function(e1, e2) {
  # nolint
  check_dims(e1, e2)
  op("and", e1, e2, tf_operation = "tf_and", expand_scalars = TRUE)
}

#' @export
`|.greta_array` <- function(e1, e2) {
  # nolint
  check_dims(e1, e2)
  op("or", e1, e2, tf_operation = "tf_or", expand_scalars = TRUE)
}

# relational operators

#' @export
`<.greta_array` <- function(e1, e2) {
  # nolint
  check_dims(e1, e2)
  op("less", e1, e2, tf_operation = "tf_lt", expand_scalars = TRUE)
}

#' @export
`>.greta_array` <- function(e1, e2) {
  # nolint
  check_dims(e1, e2)
  op("greater", e1, e2, tf_operation = "tf_gt", expand_scalars = TRUE)
}

#' @export
`<=.greta_array` <- function(e1, e2) {
  # nolint
  check_dims(e1, e2)
  op("less/equal", e1, e2, tf_operation = "tf_lte", expand_scalars = TRUE)
}

#' @export
`>=.greta_array` <- function(e1, e2) {
  # nolint
  check_dims(e1, e2)
  op("greater/equal", e1, e2, tf_operation = "tf_gte", expand_scalars = TRUE)
}

#' @export
`==.greta_array` <- function(e1, e2) {
  # nolint
  check_dims(e1, e2)
  op("equal", e1, e2, tf_operation = "tf_eq", expand_scalars = TRUE)
}

#' @export
`!=.greta_array` <- function(e1, e2) {
  # nolint
  check_dims(e1, e2)
  op("not equal", e1, e2, tf_operation = "tf_neq", expand_scalars = TRUE)
}
