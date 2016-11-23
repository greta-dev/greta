#' @name greta-operators
#'
#' @title operators for greta nodes
#'
#' @description This is a list of currently implemented arithmetic, logical and
#'   relational operators to combine greta nodes into probabilistic models.
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
#'   logical and relational operators, but they return other greta nodes, rather
#'   than values.
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
`+.node` <- function (e1, e2) {
  check_dims(e1, e2)
  op("`+`", e1, e2)
}

#' @export
`-.node` <- function (e1, e2) {
  # handle unary minus
  if (missing(e2)) {
    op("`-`", e1)
  } else {
    check_dims(e1, e2)
    op("`-`", e1, e2)
  }
}

#' @export
`*.node` <- function (e1, e2) {
  check_dims(e1, e2)
  op("`*`", e1, e2)
}

#' @export
`/.node` <- function (e1, e2) {
  check_dims(e1, e2)
  op("`/`", e1, e2)
}

#' @export
`^.node` <- function (e1, e2) {
  check_dims(e1, e2)
  op("tf$pow", e1, e2)
}

#' @export
`%%.node` <- function (e1, e2) {
  check_dims(e1, e2)
  op("`%%`", e1, e2)
}

#' @export
`%/%.node` <- function (e1, e2) {
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

  # if y is a node, coerce x before dispatch
  if (is_node(y) & !is_node(x))
    x <- to_node(x)

  UseMethod('%*%', x)

}

#' @export
`%*%.node` <- function(x, y) {

  # define a function to get the dimensions of the result
  dimfun <- function (node_list) {

    x <- node_list[[1]]
    y <- node_list[[2]]

    # check they're matrices
    if (length(x$dim) != 2 | length(y$dim) != 2)
      stop ('only 2D arrays can be matrix-multiplied')

    # check the dimensions match
    if (x$dim[2] != y$dim[1]) {
      msg <- sprintf('incompatible dimensions: %s vs %s',
                     paste0(x$dim, collapse = 'x'),
                     paste0(y$dim, collapse = 'x'))
      stop (msg)
    }

    # dimensions of the result
    c(x$dim[1], y$dim[2])

  }

  op("tf$matmul", x, y, dimfun = dimfun)

}


# logical operators
#' @export
`!.node` <- function (e1) {
  op("`!`", e1)
}

#' @export
`&.node` <- function (e1, e2) {
  check_dims(e1, e2)
  op("`&`", e1, e2)
}

#' @export
`|.node` <- function (e1, e2) {
  check_dims(e1, e2)
  op("`|`", e1, e2)
}

# relational operators

#' @export
`<.node` <- function (e1, e2) {
  check_dims(e1, e2)
  op("`<`", e1, e2)
}

#' @export
`>.node` <- function (e1, e2) {
  check_dims(e1, e2)
  op("`>`", e1, e2)
}

#' @export
`<=.node` <- function (e1, e2) {
  check_dims(e1, e2)
  op("`<=`", e1, e2)
}

#' @export
`>=.node` <- function (e1, e2) {
  check_dims(e1, e2)
  op("`>=`", e1, e2)
}

#' @export
`==.node` <- function (e1, e2) {
  check_dims(e1, e2)
  op("`==`", e1, e2)
}

#' @export
`!=.node` <- function (e1, e2) {
  check_dims(e1, e2)
  op("`!=`", e1, e2)
}
