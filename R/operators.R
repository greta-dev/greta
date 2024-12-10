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
  op("add", e1, e2,
    tf_operation = "tf$add",
    expand_scalars = TRUE
  )
}

#' @export
`-.greta_array` <- function(e1, e2) {
  # handle unary minus
  if (missing(e2)) {
    op("minus", e1,
      tf_operation = "tf$negative"
    )
  } else {
    check_dims(e1, e2)
    op("subtract", e1, e2,
      tf_operation = "tf$subtract",
      expand_scalars = TRUE
    )
  }
}

#' @export
`*.greta_array` <- function(e1, e2) {
  check_dims(e1, e2)
  op("multiply", e1, e2,
    tf_operation = "tf$multiply",
    expand_scalars = TRUE
  )
}

#' @export
`/.greta_array` <- function(e1, e2) {
  check_dims(e1, e2)
  op("divide", e1, e2,
    tf_operation = "tf$truediv",
    expand_scalars = TRUE
  )
}

#' @export
`^.greta_array` <- function(e1, e2) {
  check_dims(e1, e2)
  op("power", e1, e2,
    tf_operation = "tf$pow",
    expand_scalars = TRUE
  )
}

#' @export
`%%.greta_array` <- function(e1, e2) {
  check_dims(e1, e2)
  op("`modulo`", e1, e2,
    tf_operation = "tf$math$mod",
    expand_scalars = TRUE
  )
}

#' @export
`%/%.greta_array` <- function(e1, e2) { # nolint
  check_dims(e1, e2)
  op("`integer divide`", e1, e2,
    tf_operation = "tf$math$floordiv",
    expand_scalars = TRUE
  )
}

# overload %*% as an S3 generic
# would rather get S4 version working properly, but uuurgh S4.

#' @export
`%*%.default` <- function(x, y) { # nolint
  .Primitive("%*%")(x, y)
}

#' @rdname overloaded
#' @export
`%*%` <- function(x, y) { # nolint

  # if y is a greta array, coerce x before dispatch
  if (is.greta_array(y) & !is.greta_array(x)) {
    as_data(x) %*% y
  # if y is not a greta array and x is, coerce y before dispatch
  } else if (!is.greta_array(y) & is.greta_array(x)){
    x %*% as_data(y)
  } else {
    UseMethod("%*%", x)
  }
}

#' @export
`%*%.greta_array` <- function(x, y) { # nolint

  check_both_2d(x,y)
  check_compatible_dimensions(x, y)

  op("matrix multiply", x, y,
    dim = c(nrow(x), ncol(y)),
    tf_operation = "tf$matmul"
  )
}

# logical operators
#' @export
`!.greta_array` <- function(e1) {
  op("not", e1,
    tf_operation = "tf_not"
  )
}

#' @export
`&.greta_array` <- function(e1, e2) { # nolint
  check_dims(e1, e2)
  op("and", e1, e2,
    tf_operation = "tf_and",
    expand_scalars = TRUE
  )
}

#' @export
`|.greta_array` <- function(e1, e2) { # nolint
  check_dims(e1, e2)
  op("or", e1, e2,
    tf_operation = "tf_or",
    expand_scalars = TRUE
  )
}

# relational operators

#' @export
`<.greta_array` <- function(e1, e2) { # nolint
  check_dims(e1, e2)
  op("less", e1, e2,
    tf_operation = "tf_lt",
    expand_scalars = TRUE
  )
}

#' @export
`>.greta_array` <- function(e1, e2) { # nolint
  check_dims(e1, e2)
  op("greater", e1, e2,
    tf_operation = "tf_gt",
    expand_scalars = TRUE
  )
}

#' @export
`<=.greta_array` <- function(e1, e2) { # nolint
  check_dims(e1, e2)
  op("less/equal", e1, e2,
    tf_operation = "tf_lte",
    expand_scalars = TRUE
  )
}

#' @export
`>=.greta_array` <- function(e1, e2) { # nolint
  check_dims(e1, e2)
  op("greater/equal", e1, e2,
    tf_operation = "tf_gte",
    expand_scalars = TRUE
  )
}

#' @export
`==.greta_array` <- function(e1, e2) { # nolint
  check_dims(e1, e2)
  op("equal", e1, e2,
    tf_operation = "tf_eq",
    expand_scalars = TRUE
  )
}

#' @export
`!=.greta_array` <- function(e1, e2) { # nolint
  check_dims(e1, e2)
  op("not equal", e1, e2,
    tf_operation = "tf_neq",
    expand_scalars = TRUE
  )
}
