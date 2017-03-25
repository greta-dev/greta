#' @name greta-operators
#'
#' @title Operators for Greta Arrays
#'
#' @description This is a list of currently implemented arithmetic, logical and
#'   relational operators, and extract/replace syntax to combine greta arrays
#'   into probabilistic models. Also see \link{greta-functions} and
#'   \link{greta-transforms}.
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
#'  x = observed(-1:12)
#'
#'  # arithmetic
#'  a = x + 1
#'  b = 2 * x + 3
#'  c = x %% 2
#'  d = x %/% 5
#'
#'  # logical
#'  e = (x > 1) | (x < 1)
#'  f = e & (x < 2)
#'  g = !f
#'
#'  # relational
#'  h = x < 1
#'  i = (-x) >= x
#'  j = h == x
NULL

# use S3 dispatch to apply the operators
#' @export
`+.greta_array` <- function (e1, e2) {
  check_dims(e1, e2)
  op("`+`", e1, e2)
}

#' @export
`-.greta_array` <- function (e1, e2) {
  # handle unary minus
  if (missing(e2)) {
    op("`-`", e1)
  } else {
    check_dims(e1, e2)
    op("`-`", e1, e2)
  }
}

#' @export
`*.greta_array` <- function (e1, e2) {
  check_dims(e1, e2)
  op("`*`", e1, e2)
}

#' @export
`/.greta_array` <- function (e1, e2) {
  check_dims(e1, e2)
  op("`/`", e1, e2)
}

#' @export
`^.greta_array` <- function (e1, e2) {
  check_dims(e1, e2)
  op("tf$pow", e1, e2)
}

#' @export
`%%.greta_array` <- function (e1, e2) {
  check_dims(e1, e2)
  op("`%%`", e1, e2)
}

#' @export
`%/%.greta_array` <- function (e1, e2) {
  check_dims(e1, e2)
  op("`%/%`", e1, e2)
}

# overload %*% as an S3 generic
# would rather get S4 version working properly, but uuurgh S4.

#' @export
`%*%.default` <- function (x, y)
  .Primitive("%*%")(x, y)

#' @export
`%*%` <- function (x, y) {

  # if y is a greta array, coerce x before dispatch
  if (is.greta_array(y) & !is.greta_array(x))
    x <- ga(x)

  UseMethod('%*%', x)

}

#' @export
`%*%.greta_array` <- function(x, y) {

  # define a function to get the dimensions of the result
  dimfun <- function (elem_list) {

    x <- elem_list[[1]]
    y <- elem_list[[2]]

    # check they're matrices
    if (length(dim(x)) != 2 | length(dim(x)) != 2)
      stop ('only two-dimensional greta arrays can be matrix-multiplied')

    # check the dimensions match
    if (dim(x)[2] != dim(x)[1]) {
      msg <- sprintf('incompatible dimensions: %s vs %s',
                     paste0(dim(x), collapse = 'x'),
                     paste0(dim(y), collapse = 'x'))
      stop (msg)
    }

    # dimensions of the result
    c(dim(x)[1], dim(y)[2])

  }

  op("tf$matmul", x, y, dimfun = dimfun)

}


# logical operators
#' @export
`!.greta_array` <- function (e1) {
  op("`!`", e1)
}

#' @export
`&.greta_array` <- function (e1, e2) {
  check_dims(e1, e2)
  op("`&`", e1, e2)
}

#' @export
`|.greta_array` <- function (e1, e2) {
  check_dims(e1, e2)
  op("`|`", e1, e2)
}

# relational operators

#' @export
`<.greta_array` <- function (e1, e2) {
  check_dims(e1, e2)
  op("`<`", e1, e2)
}

#' @export
`>.greta_array` <- function (e1, e2) {
  check_dims(e1, e2)
  op("`>`", e1, e2)
}

#' @export
`<=.greta_array` <- function (e1, e2) {
  check_dims(e1, e2)
  op("`<=`", e1, e2)
}

#' @export
`>=.greta_array` <- function (e1, e2) {
  check_dims(e1, e2)
  op("`>=`", e1, e2)
}

#' @export
`==.greta_array` <- function (e1, e2) {
  check_dims(e1, e2)
  op("`==`", e1, e2)
}

#' @export
`!=.greta_array` <- function (e1, e2) {
  check_dims(e1, e2)
  op("`!=`", e1, e2)
}
