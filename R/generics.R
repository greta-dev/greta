# overload operators with generics

#' @name grete-operations
#'
#' @title operations on grete nodes
#'
#' @description This is a list of currently implemented operations to combine
#'  grete nodes into probabilistic models. More will follow.
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

# overload %*% as an S3 generic

#' @export
`%*%.default` <- function (x, y)
  .Primitive("%*%")(x, y)

#' @export
`%*%` <- function (x, y)
  UseMethod('%*%', x)

#' @export
`%*%.node` <- function(x, y) {

  # check dimensions of these objects
  if (length(x$dim) != 2 | length(y$dim) != 2)
    stop ('only 2D arrays can be matrix-multiplied')

  if (x$dim[2] != y$dim[1]) {
    msg <- sprintf('incompatible dimensions: %s vs %s',
                   paste0(x$dim, collapse = 'x'),
                   paste0(y$dim, collapse = 'x'))
    stop (msg)
  }

  op("tf$matmul", x, y)
}

