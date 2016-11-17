# overload operators with generics

#' @name greta-operations
#'
#' @title operations on greta nodes
#'
#' @description This is a list of currently implemented operations to combine
#'  greta nodes into probabilistic models. More will follow.
#'
#' @section Usage: \preformatted{
#'  x + y
#'  x - y
#'  x * y
#'  x / y
#'  log(x)
#'  exp(x)
#'  x \%*\% y
#'  }
NULL

# use S3 dispatch to apply the operators
#' @export
`+.node` <- function (e1, e2) {
  check_dims(e1, e2)
  op("`+`", e1, e2)
}

#' @export
`-.node` <- function (e1, e2) {
  check_dims(e1, e2)
  op("`-`", e1, e2)
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
`log.node` <- function (e1) {
  op("tf$log", e1)
}

#' @export
`exp.node` <- function (e1) {
  op("tf$exp", e1)
}

#' @export
`t.node` <- function (e1) {

  # reverse the dimensions
  dimfun <- function (node_list) {
    x <- node_list[[1]]
    if (length(x$dim) != 2)
      stop ('only 2D arrays can be transposed')
    rev(x$dim)
  }

  op("tf$transpose", e1, dimfun = dimfun)
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

