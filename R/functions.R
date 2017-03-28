#' @name greta-functions
#'
#' @title R functions that work for greta arrays
#'
#' @description This is a list of functions in base R that are currently
#'   implemented to transform greta arrays Also see \link{greta-operators} and
#'   \link{greta-transforms}.
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
#'  solve(a, b, ...)
#'  }
#'
#' @details TensorFlow only enables rounding to integers, so \code{round()} will
#'   error if \code{digits} is set to anything other than \code{0}.
#'
#'   Any additional arguments to \code{chol()} and \code{solve()} will be
#'   ignored, see the TensorFlow documentation for details of these routines.
#'
#'   \code{diag()} can only be used to extract the diagonal part of a square and
#'   two-dimensional greta array, it cannot be used to create a matrix-like
#'   greta array from a vector-like greta array, nor to assign the diagonal
#'   elements of a greta array. A static diagonal matrix can always be created
#'   with e.g. \code{diag(3)}.
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
log.greta_array <- function (e1) {
  op("tf$log", e1)
}

#' @export
exp.greta_array <- function (e1) {
  op("tf$exp", e1)
}

#' @export
log1p.greta_array <- function (e1) {
  log(1 + e1)
}

#' @export
expm1.greta_array <- function (e1) {
  exp(e1) - 1
}

#' @export
abs.greta_array <- function (e1) {
  op("tf$abs", e1)
}

#' @export
sqrt.greta_array <- function (e1) {
  op("tf$sqrt", e1)
}

#' @export
sign.greta_array <- function (e1) {
  op("tf$sign", e1)
}

#' @export
ceiling.greta_array <- function (e1) {
  op("tf$ceil", e1)
}

#' @export
floor.greta_array <- function (e1) {
  op("tf$floor", e1)
}

#' @export
round.greta_array <- function (e1, digits = 0) {
  if (digits != 0)
    stop("TensorFlow round only supports rounding to integers")
  op("tf$round", e1)
}

# trigonometry functions
#' @export
cos.greta_array <- function (e1) {
  op("tf$cos", e1)
}

#' @export
sin.greta_array <- function (e1) {
  op("tf$sin", e1)
}

#' @export
tan.greta_array <- function (e1) {
  op("tf$tan", e1)
}

#' @export
acos.greta_array <- function (e1) {
  op("tf$acos", e1)
}

#' @export
asin.greta_array <- function (e1) {
  op("tf$asin", e1)
}

#' @export
atan.greta_array <- function (e1) {
  op("tf$atan", e1)
}

#' @export
lgamma.greta_array <- function (e1) {
  op("tf$lgamma", e1)
}

#' @export
digamma.greta_array <- function (e1) {
  op("tf$digamma", e1)
}

#' @export
t.greta_array <- function (e1) {

  # reverse the dimensions
  dimfun <- function (elem_list) {
    x <- elem_list[[1]]
    if (length(dim(x)) != 2)
      stop ('only 2D arrays can be transposed')
    rev(dim(x))
  }

  op("tf$transpose", e1, dimfun = dimfun)
}

#' @export
chol.greta_array <- function (e1, ...) {

  if (!identical(list(), list(...)))
    warning ('chol() options are ignored by TensorFlow')

  dimfun <- function (elem_list) {
    dim <- dim(elem_list[[1]])
    if ( !(length(dim) == 2 && dim[1] == dim[2]) )
      stop ('only 2D, square greta arrays can be Cholesky decomposed')
    dim
  }

  op("tf$cholesky", e1, dimfun = dimfun)
}

#' @export
diag.greta_array <- function (x = 1, nrow, ncol) {

  # can only extract from a node, cannot create from a node or assign.
  if (missing(x) | !missing(nrow) | !missing(ncol))
    stop ('diag can only be used to extract diagonal elements from a matrix, not to create or assign values')

  dimfun <- function (elem_list) {

    x <- elem_list[[1]]
    dim <- dim(x)

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

#' @export
solve.greta_array <- function (a, b, ...) {

  # check the matrix is square
  if (dim(a)[1] != dim(a)[2]) {
    stop (sprintf('a must be square, but has %i rows and %i columns',
                  dim(a)[1], dim(a)[2]))
  }

  # if they just want the matrix inverse, do that
  if (missing(b)) {

    dimfun <- function (elem_list) {

      a <- elem_list[[1]]

      # a must be square
      if (dim(a)[1] != dim(a)[2]) {
        stop (sprintf('a must be square, but has %i rows and %i columns',
                      dim(a)[1], dim(a)[2]))
      }

      # return the dimensions
      dim(a)

    }

    return (op("tf$matrix_inverse", a, dimfun = dimfun))

  } else {


    dimfun <- function (elem_list) {

      a <- elem_list[[1]]
      b <- elem_list[[2]]

      # a must be square
      if (dim(a)[1] != dim(a)[2]) {
        stop (sprintf('a must be square, but has %i rows and %i columns',
                      dim(a)[1], dim(a)[2]))
      }

      # b must have the right number of rows
      if (dim(b)[1] != dim(a)[1]) {
        stop (sprintf('b must have the same number of rows as a (%i), but has %i rows instead',
                      dim(a)[1], dim(b)[1]))
      }

      # return the dimensions
      dim(b)

    }

    # ... and solve the linear equations
    return (op("tf$matrix_solve", a, b))

  }

}
