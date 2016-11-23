#' @name greta-functions
#'
#' @title R functions that work for greta nodes
#'
#' @description This is a list of functions in base R that are currently
#'   implemented to transform greta nodes.
#'
#' @section Usage: \preformatted{
#'
#'  # logarithms and exponentials
#'  log(x)
#'  exp(x)
#'  log1p(x)
#'  expm1(x)
#'
#'  # miscellaneous mathematics
#'  abs(x)
#'  sqrt(x)
#'  sign(x)
#'
#'  # rounding of numbers
#'  ceiling(x)
#'  floor(x)
#'  round(x, digits = 0)
#'
#'  # trigonometry
#'  cos(x)
#'  sin(x)
#'  tan(x)
#'  acos(x)
#'  asin(x)
#'  atan(x)
#'
#'  # special mathematical functions
#'  lgamma(x)
#'  digamma(x)
#'
#'  # matrix operations
#'  t(x)
#'  chol(x, ...)
#'  diag(x, nrow, ncol)
#'  }
#'
#' @details TensorFlow only enables rounding to integers, so \code{round()} will
#'   error is set to anything other than \code{0}.
#'
#'   Any additional arguments to \code{chol()} will be ignored, see the
#'   TensorFlow documentation for details of the cholesky routine.
#'
#'   \code{diag()} can only be used to extract the diagonal part of a square
#'   matrix, it cannot be used to create a matrix from a node, nor to assign the
#'   diagonal elements of a square matrix. A static diagonal matrix can always
#'   be created with e.g. \code{diag(3)}.
#'
#' @examples
#' x = observed(matrix(1:9, nrow = 3, ncol = 3))
#' a = log(exp(x))
#' b = log1p(expm1(x))
#' c = sign(x - 5)
#' d = abs(x - 5)
#'
#' e = diag(x)
#'
#' z = t(a)
#'
#'
NULL

#' @export
`log.node` <- function (e1) {
  op("tf$log", e1)
}

#' @export
`exp.node` <- function (e1) {
  op("tf$exp", e1)
}

#' @export
`log1p.node` <- function (e1) {
  log(1 + e1)
}

#' @export
`expm1.node` <- function (e1) {
  exp(e1) - 1
}

#' @export
`abs.node` <- function (e1) {
  op("tf$abs", e1)
}

#' @export
`sqrt.node` <- function (e1) {
  op("tf$sqrt", e1)
}

#' @export
`sign.node` <- function (e1) {
  op("tf$sign", e1)
}

#' @export
`ceiling.node` <- function (e1) {
  op("tf$ceil", e1)
}

#' @export
`floor.node` <- function (e1) {
  op("tf$floor", e1)
}

#' @export
`round.node` <- function (e1, digits = 0) {
  if (digits != 0)
    stop("TensorFlow round only supports rounding to integers")
  op("tf$round", e1)
}

# trigonometry functions
#' @export
`cos.node` <- function (e1) {
  op("tf$cos", e1)
}

#' @export
`sin.node` <- function (e1) {
  op("tf$sin", e1)
}

#' @export
`tan.node` <- function (e1) {
  op("tf$tan", e1)
}

#' @export
`acos.node` <- function (e1) {
  op("tf$acos", e1)
}

#' @export
`asin.node` <- function (e1) {
  op("tf$asin", e1)
}

#' @export
`atan.node` <- function (e1) {
  op("tf$atan", e1)
}

#' @export
`lgamma.node` <- function (e1) {
  op("tf$lgamma", e1)
}

#' @export
`digamma.node` <- function (e1) {
  op("tf$digamma", e1)
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

#' @export
`chol.node` <- function (e1, ...) {

  if (!identical(list(), list(...)))
    warning ('chol() options are ignored by TensorFlow')

  dimfun <- function (node_list) {
    dim <- node_list[[1]]$dim
    if ( !(length(dim) == 2 && dim[1] == dim[2]) )
      stop ('only 2D, square can Cholesky decomposed')
    dim
  }

  op("tf$cholesky", e1, dimfun = dimfun)
}

#' @export
diag.node <- function (x = 1, nrow, ncol) {

  # can only extract from a node, cannot create from a node or assign.
  if (missing(x) | !missing(nrow) | !missing(ncol))
    stop ('diag can only be used to extract diagonal elements from a matrix, not to create or assign values')

  dimfun <- function (node_list) {

    x <- node_list[[1]]
    dim <- x$dim

    # check the rank isn't too high
    if (length(dim) != 2)
      stop ('cannot only extract the diagonal from a node with exactly two dimensions')

    if (dim[1] != dim[2])
      stop ('diagonal elements can only be extracted from square matrices')

    # return the dimensions
    c(dim[1], 1)

  }

  # return the extraction op
  return (op('tf$diag_part', x, dimfun = dimfun))

}
