#' @name functions
#'
#' @title functions for greta arrays
#'
#' @description This is a list of functions in base R that are currently
#'   implemented to transform greta arrays. Also see \link{operators} and
#'   \link{transforms}.
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
#'  mean(x)
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
#'  choose(n, k)
#'  lchoose(n, k)
#'
#'  # matrix operations
#'  t(x)
#'  chol(x, ...)
#'  diag(x, nrow, ncol)
#'  diag(x) <- value
#'  solve(a, b, ...)
#'  kronecker2(X, Y)
#'
#'  # reducing operations
#'  sum(..., na.rm = TRUE)
#'  prod(..., na.rm = TRUE)
#'  min(..., na.rm = TRUE)
#'  max(..., na.rm = TRUE)
#'
#'  # cumulative operations
#'  cumsum(x)
#'  cumprod(x)
#'
#'  # miscellaneous operations
#'  sweep(x, MARGIN, STATS, FUN = c('-', '+', '/', '*'))
#'
#'  # solve an upper or lower triangular system
#'  backsolve(r, x, k = ncol(r), upper.tri = TRUE,
#'            transpose = FALSE)
#'  forwardsolve(l, x, k = ncol(l), upper.tri = FALSE,
#'               transpose = FALSE)
#'
#' }
#'
#' @details TensorFlow only enables rounding to integers, so \code{round()} will
#'   error if \code{digits} is set to anything other than \code{0}.
#'
#'   Any additional arguments to \code{chol()} and \code{solve()} will be
#'   ignored, see the TensorFlow documentation for details of these routines.
#'
#'   \code{diag()} can be used to extract or replace the diagonal part of a
#'   square and two-dimensional greta array, but it cannot be used to create a
#'   matrix-like greta array from a scalar or vector-like greta array. A static
#'   diagonal matrix can always be created with e.g. \code{diag(3)}, and then
#'   converted into a greta array.
#'
#'   \code{sweep()} only works on two-dimensional greta arrays (so \code{MARGIN}
#'   can only be either 1 or 2), and only for subtraction, addition, division
#'   and multiplication.
#'
#' @examples
#' \dontrun{
#'
#' x <- as_data(matrix(1:9, nrow = 3, ncol = 3))
#' a <- log(exp(x))
#' b <- log1p(expm1(x))
#' c <- sign(x - 5)
#' d <- abs(x - 5)
#'
#' e <- diag(x)
#' diag(x) <- e + 1
#'
#' z <- t(a)
#'
#' y <- sweep(x, 1, e, '-')
#' }
NULL

#' @export
log.greta_array <- function (x, base = exp(1)) {
  op("log", x, tf_operation = tf$log)
}

#' @export
exp.greta_array <- function (x) {
  op("exp", x, tf_operation = tf$exp)
}

#' @export
log1p.greta_array <- function (x) {
  op("log1p", x, tf_operation = tf$log1p)
}

#' @export
expm1.greta_array <- function (x) {
  op("expm1", x, tf_operation = tf$expm1)
}

#' @export
abs.greta_array <- function (x) {
  op("abs", x, tf_operation = tf$abs)
}

#' @export
sqrt.greta_array <- function (x) {
  op("sqrt", x, tf_operation = tf$sqrt)
}

#' @export
sign.greta_array <- function (x) {
  op("sign", x, tf_operation = tf$sign)
}

#' @export
ceiling.greta_array <- function (x) {
  op("ceil", x, tf_operation = tf$ceil)
}

#' @export
floor.greta_array <- function (x) {
  op("floor", x, tf_operation = tf$floor)
}

#' @export
round.greta_array <- function (x, digits = 0) {
  if (digits != 0)
    stop("TensorFlow round only supports rounding to integers")
  op("round", x, tf_operation = tf$round)
}

# trigonometry functions
#' @export
cos.greta_array <- function (x) {
  op("cos", x, tf_operation = tf$cos)
}

#' @export
sin.greta_array <- function (x) {
  op("sin", x, tf_operation = tf$sin)
}

#' @export
tan.greta_array <- function (x) {
  op("tan", x, tf_operation = tf$tan)
}

#' @export
acos.greta_array <- function (x) {
  op("acos", x, tf_operation = tf$acos)
}

#' @export
asin.greta_array <- function (x) {
  op("asin", x, tf_operation = tf$asin)
}

#' @export
atan.greta_array <- function (x) {
  op("atan", x, tf_operation = tf$atan)
}

#' @export
lgamma.greta_array <- function (x) {
  op("lgamma", x, tf_operation = tf$lgamma)
}

#' @export
digamma.greta_array <- function (x) {
  op("digamma", x, tf_operation = tf$digamma)
}

#' @export
t.greta_array <- function (x) {

  # reverse the dimensions
  dimfun <- function (elem_list) {
    x <- elem_list[[1]]
    if (length(dim(x)) != 2)
      stop ('only 2D arrays can be transposed')
    rev(dim(x))
  }

  op("transpose", x, dimfun = dimfun, tf_operation = tf$transpose)
}

#' @export
chol.greta_array <- function (x, ...) {

  if (!identical(list(), list(...)))
    warning ('chol() options are ignored by TensorFlow')

  dimfun <- function (elem_list) {
    dim <- dim(elem_list[[1]])
    if ( !(length(dim) == 2 && dim[1] == dim[2]) )
      stop ('only 2D, square greta arrays can be Cholesky decomposed')
    dim
  }

  op("chol", x, dimfun = dimfun, tf_operation = tf_chol)
}

#' @rdname overloaded
#' @export
diag <- function (x = 1, nrow, ncol)
  UseMethod('diag', x)

# wrapper function to avoid a CRAN check warning about using a .Internal() call
#' @export
diag.default <- function (...)
  base::diag(...)

#' @export
diag.greta_array <- function (x = 1, nrow, ncol) {

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
  op('diag', x, dimfun = dimfun, tf_operation = tf$diag_part)

}

#' @export
solve.greta_array <- function (a, b, ...) {

  # check a is 2D
  if (length(dim(a)) != 2) {
    stop (sprintf("'a' and 'b' must both be 2D, but 'a' has dimensions: %s",
                  paste(dim(a), collapse = ' x ')))
  }

  # check the matrix is square
  if (dim(a)[1] != dim(a)[2]) {
    stop (sprintf("'a' must be square, but has %i rows and %i columns",
                  dim(a)[1], dim(a)[2]))
  }

  # if they just want the matrix inverse, do that
  if (missing(b)) {

    dimfun <- function (elem_list) {

      a <- elem_list[[1]]

      # return the dimensions
      dim(a)

    }

    return (op("solve", a, dimfun = dimfun, tf_operation = tf$matrix_inverse))

  } else {

    # check b is 2D
    if (length(dim(b)) != 2) {
      stop (sprintf("'a' and 'b' must both be 2D, but 'b' has dimensions: %s",
                    paste(dim(b), collapse = ' x ')))
    }



    dimfun <- function (elem_list) {

      a <- elem_list[[1]]
      b <- elem_list[[2]]

      # b must have the right number of rows
      if (dim(b)[1] != dim(a)[1]) {
        stop (sprintf("'b' must have the same number of rows as 'a' (%i), but has %i rows instead",
                      dim(a)[1], dim(b)[1]))
      }

      # return the dimensions
      dim(b)

    }

    # ... and solve the linear equations
    return (op("solve", a, b, dimfun = dimfun, tf_operation = tf$matrix_solve))

  }

}

# sum, prod, min, mean, max

#' @export
sum.greta_array <- function (..., na.rm = TRUE) {

  # combine all elements into a column vector
  vec <- c(...)

  # results will be 1 x 1
  dimfun <- function (x) c(1, 1)

  # sum the elements
  op('sum',
     vec,
     dimfun = dimfun,
     tf_operation = tf$reduce_sum)

}

#' @export
prod.greta_array <- function (..., na.rm = TRUE) {

  # combine all elements into a column vector
  vec <- c(...)

  # results will be 1 x 1
  dimfun <- function (x) c(1, 1)

  # sum the elements
  op('prod',
     vec,
     dimfun = dimfun,
     tf_operation = tf$reduce_prod)

}

#' @export
min.greta_array <- function (..., na.rm = TRUE) {

  # combine all elements into a column vector
  vec <- c(...)

  # results will be 1 x 1
  dimfun <- function (x) c(1, 1)

  # sum the elements
  op('min',
     vec,
     dimfun = dimfun,
     tf_operation = tf$reduce_min)

}

#' @export
mean.greta_array <- function (x, trim = 0, na.rm = TRUE, ...) {

  # results will be 1 x 1
  dimfun <- function (x) c(1, 1)

  # sum the elements
  op('mean',
     x,
     dimfun = dimfun,
     tf_operation = tf$reduce_mean)

}

#' @export
max.greta_array <- function (..., na.rm = TRUE) {

  # combine all elements into a column vector
  vec <- c(...)

  # results will be 1 x 1
  dimfun <- function (x) c(1, 1)

  # sum the elements
  op('max',
     vec,
     dimfun = dimfun,
     tf_operation = tf$reduce_max)

}

check_cum_op <- function (x) {
  dims <- dim(x)
  if (length(dims) > 2 | dims[2] != 1) {
    stop ("'x' must be a column vector, but has dimensions ",
          paste(dims, collapse = ' x '),
          call. = FALSE)
  }
}

#' @export
cumsum.greta_array <- function (x) {
  check_cum_op(x)
  op("cumsum", x, tf_operation = tf$cumsum)
}

#' @export
cumprod.greta_array <- function (x) {
  check_cum_op(x)
  op("cumprod", x, tf_operation = tf$cumprod)
}

# get the incides to reduce over, for colSums, rowSums, colMeans, rowMeans
rowcol_idx <- function (x, dims, which = c("col", "row")) {

  if (dims < 1L || dims > length(dim(x)) - 1L)
    stop("invalid 'dims'", call. = FALSE)

  switch(which,
         row = (dims + 1):length(dim(x)),
         col = seq_len(dims))

}

# generate dimfun for colSums, rowSums, colMeans, rowMeans
rowcol_dimfun <- function (dims, which = c("row", "col")) {

  function (elem_list) {
    x <- elem_list[[1]]
    idx <- rowcol_idx(x, dims, which)
    dims <- dim(x)[-idx]
    if (length(dims) == 1)
      dims <- c(dims, 1L)
    dims
  }

}

#' @rdname overloaded
#' @export
colMeans <- function (x, na.rm = FALSE, dims = 1L)
  UseMethod("colMeans", x)

#' @export
colMeans.default <- function (x, na.rm = FALSE, dims = 1L)
  base::colMeans(x = x, na.rm = na.rm, dims = dims)

#' @export
colMeans.greta_array <- function (x, na.rm = FALSE, dims = 1L) {

  op("colMeans",
     x,
     operation_args = list(dims = dims),
     tf_operation = tf_colmeans,
     dimfun = rowcol_dimfun(dims, "col"))

}

#' @rdname overloaded
#' @export
rowMeans <- function (x, na.rm = FALSE, dims = 1L)
  UseMethod("rowMeans", x)

#' @export
rowMeans.default <- function (x, na.rm = FALSE, dims = 1L)
  base::rowMeans(x = x, na.rm = na.rm, dims = dims)

#' @export
rowMeans.greta_array <- function (x, na.rm = FALSE, dims = 1L) {

  dimfun <- function (elem_list)
    dim(elem_list[[1]])[dims]

  op("rowMeans",
     x,
     operation_args = list(dims = dims),
     tf_operation = tf_rowmeans,
     dimfun = rowcol_dimfun(dims, "row"))

}

#' @rdname overloaded
#' @export
colSums <- function (x, na.rm = FALSE, dims = 1L)
  UseMethod("colSums", x)

#' @export
colSums.default <- function (x, na.rm = FALSE, dims = 1L)
  base::colSums(x = x, na.rm = na.rm, dims = dims)

#' @export
colSums.greta_array <- function (x, na.rm = FALSE, dims = 1L) {

  op("colSums",
     x,
     operation_args = list(dims = dims),
     tf_operation = tf_colsums,
     dimfun = rowcol_dimfun(dims, "col"))

}

#' @rdname overloaded
#' @export
rowSums <- function (x, na.rm = FALSE, dims = 1L)
  UseMethod("rowSums", x)

#' @export
rowSums.default <- function (x, na.rm = FALSE, dims = 1L)
  base::rowSums(x = x, na.rm = na.rm, dims = dims)

#' @export
rowSums.greta_array <- function (x, na.rm = FALSE, dims = 1L) {

  op("rowSums",
     x,
     operation_args = list(dims = dims),
     tf_operation = tf_rowsums,
     dimfun = rowcol_dimfun(dims, "row"))

}

#' @rdname overloaded
#' @export
sweep <- function (x, MARGIN, STATS, FUN = "-", check.margin = TRUE, ...) {

  if (inherits(STATS, "greta_array"))
    x <- as.greta_array(x)

  UseMethod('sweep', x)

}

#' @export
sweep.default <- base::sweep

#' @export
sweep.greta_array <- function (x, MARGIN, STATS, FUN = c('-', '+', '/', '*'), check.margin = TRUE, ...) {

  # only allow these four functions
  FUN <- match.arg(FUN)

  STATS <- as.greta_array(STATS)

  if (!MARGIN %in% seq_len(2))
    stop ('MARGIN can only be 1 or 2')

  dimfun <- function (elem_list) {

    x <- elem_list[[1]]
    STATS <- elem_list[[2]]

    # x must be 2D
    if (length(dim(x)) != 2) {
      stop (sprintf('x must be a 2D array, but has %i dimensions',
                    length(dim(x))))
    }

    # STATS must be a column array
    if (!(length(dim(STATS)) == 2 && dim(STATS)[2] == 1)) {
      stop (sprintf('STATS must be a column vector array, but has dimensions %s',
                    paste(dim(STATS), collapse = ' x ')))
    }

    # STATS must have the same dimension as the correct dim of x
    if (dim(x)[MARGIN] != dim(STATS)[1])
      stop ('the number of elements of STATS does not match dim(x)[MARGIN]')

    # return the dimensions of x
    dim(x)

  }

  op("sweep",
     x, STATS,
     operation_args = list(MARGIN = MARGIN,
                           FUN = FUN),
     tf_operation = tf_sweep,
     dimfun = dimfun)

}

#' @export
kronecker2 <- function (X, Y, FUN = "*", make.dimnames = FALSE, ...) {
  
  # only allow these four functions
  FUN <- match.arg(FUN)
  
  dimfun <- function (elem_list) {
    
    x <- elem_list[[1]]
    y <- elem_list[[2]]
    
    # x must be 2D
    if (length(dim(x)) != 2) {
      stop (sprintf('x must be a 2D array, but has %i dimensions',
                    length(dim(x))))
    }
    
    # y must be 2D
    if (length(dim(y)) != 2) {
      stop (sprintf('y must be a 2D array, but has %i dimensions',
                    length(dim(y))))
    }

    # return the dimensions of x
    dim1 <- dim(x)
    dim2 <- dim(y)
    dim1 * dim2
    
  }
  
  op("kronecker2",
     X, Y,
     tf_operation = tf_kronecker,
     dimfun = dimfun)
  
}

#' @rdname overloaded
#' @export
backsolve <- function (r, x, k = ncol(r),
                       upper.tri = TRUE,
                       transpose = FALSE) {
  UseMethod('backsolve', x)
}

#' @export
backsolve.default <- function (r, x, k = ncol(r),
                               upper.tri = TRUE,
                               transpose = FALSE) {
  base::backsolve(r, x, k = ncol(r),
                  upper.tri = TRUE,
                  transpose = FALSE)
}

# define this explicitly so CRAN doesn't think we're using .Internal
#' @export
backsolve.greta_array <- function(r, x,
                                  k = ncol(r),
                                  upper.tri = TRUE,
                                  transpose = FALSE) {
  if (k != ncol(r)) {
    stop ("k must equal ncol(r) for greta arrays",
          call. = FALSE)
  }

  if (transpose) {
    stop ("transpose must be FALSE for greta arrays",
          call. = FALSE)
  }

  dimfun <- function (elem_list)
    dim(elem_list[[2]])

  op("backsolve",
     r, x,
     operation_args = list(lower = !upper.tri),
     tf_operation = tf$matrix_triangular_solve,
     dimfun = dimfun)

}

#' @rdname overloaded
#' @export
forwardsolve <- function (l, x, k = ncol(l),
                          upper.tri = FALSE,
                          transpose = FALSE) {
  UseMethod('forwardsolve', x)
}

# define this explicitly so CRAN doesn't think we're using .Internal
#' @export
forwardsolve.default <- function (l, x, k = ncol(l),
                                  upper.tri = FALSE,
                                  transpose = FALSE) {

  base::forwardsolve(l, x, k = ncol(l),
                     upper.tri = FALSE,
                     transpose = FALSE)
}

#' @export
forwardsolve.greta_array <- function (l, x,
                                      k = ncol(l),
                                      upper.tri = FALSE,
                                      transpose = FALSE) {
  if (k != ncol(l)) {
    stop ("k must equal ncol(l) for greta arrays",
          call. = FALSE)
  }

  if (transpose) {
    stop ("transpose must be FALSE for greta arrays",
          call. = FALSE)
  }

  dimfun <- function (elem_list)
    dim(elem_list[[2]])

  op("forwardsolve",
     l, x,
     operation_args = list(lower = !upper.tri),
     tf_operation = tf$matrix_triangular_solve,
     dimfun = dimfun)

}
