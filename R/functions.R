#' @name functions
#'
#' @title functions for greta arrays
#'
#' @description This is a list of functions (mostly from base R) that are
#'   currently implemented to transform greta arrays. Also see \link{operators}
#'   and \link{transforms}.
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
#'  chol2inv(x, ...)
#'  cov2cor(V)
#'  solve(a, b, ...)
#'  kronecker(X, Y, FUN = c('*', '/', '+', '-'))
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
#'  # solve an upper or lower triangular system
#'  backsolve(r, x, k = ncol(r), upper.tri = TRUE,
#'            transpose = FALSE)
#'  forwardsolve(l, x, k = ncol(l), upper.tri = FALSE,
#'               transpose = FALSE)
#'
#'  #'  # miscellaneous operations
#'  aperm(x, perm)
#'  apply(x, MARGIN, FUN = c("sum", "max", "mean", "min",
#'                           "prod", "cumsum", "cumprod"))
#'  sweep(x, MARGIN, STATS, FUN = c('-', '+', '/', '*'))
#'  tapply(X, INDEX, FUN = c("sum", "max", "mean", "min", "prod"), ...)
#'
#' }
#'
#' @details TensorFlow only enables rounding to integers, so \code{round()} will
#'   error if \code{digits} is set to anything other than \code{0}.
#'
#'   Any additional arguments to \code{chol()}, \code{chol2inv}, and
#'   \code{solve()} will be ignored, see the TensorFlow documentation for
#'   details of these routines.
#'
#'   \code{sweep()} only works on two-dimensional greta arrays (so \code{MARGIN}
#'   can only be either 1 or 2), and only for subtraction, addition, division
#'   and multiplication.
#'
#'   \code{tapply()} works on column vectors (2D greta arrays with one column),
#'   and \code{INDEX} cannot be a greta array. Currently five functions are
#'   available, and arguments passed to \dots are ignored.
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
#' z <- t(a)
#'
#' y <- sweep(x, 1, e, '-')
#' }
NULL

#' @export
log.greta_array <- function(x, base = exp(1)) {
  if (has_representation(x, "log")) {
    result <- copy_representation(x, "log")
  } else {
    result <- op("log", x, tf_operation = "tf$math$log",
                 representations = list(exp = x))
  }
  result
}

#' @export
exp.greta_array <- function(x) {
  # if this already has an exp representation, use that
  if (has_representation(x, "exp")) {
    result <- copy_representation(x, "exp")
  } else {
    # otherwise exponentiate it, and store the log representation
    result <- op("exp", x, tf_operation = "tf$math$exp",
                 representations = list(log = x))
  }
  result
}

#' @export
log1p.greta_array <- function(x) {
  op("log1p", x, tf_operation = "tf$math$log1p")
}

#' @export
expm1.greta_array <- function(x) {
  op("expm1", x, tf_operation = "tf$math$expm1")
}

#' @export
abs.greta_array <- function(x) {
  op("abs", x, tf_operation = "tf$abs")
}

#' @export
sqrt.greta_array <- function(x) {
  op("sqrt", x, tf_operation = "tf$sqrt")
}

#' @export
sign.greta_array <- function(x) {
  op("sign", x, tf_operation = "tf$sign")
}

#' @export
ceiling.greta_array <- function(x) {
  op("ceil", x, tf_operation = "tf$math$ceil")
}

#' @export
floor.greta_array <- function(x) {
  op("floor", x, tf_operation = "tf$floor")
}

#' @export
round.greta_array <- function(x, digits = 0) {
  if (digits != 0)
    stop("greta arrays can only be rounded to the nearest integer, ",
         "the digits argument cannot be set")
  op("round", x, tf_operation = "tf$round")
}

# trigonometry functions
#' @export
cos.greta_array <- function(x) {
  op("cos", x, tf_operation = "tf$cos")
}

#' @export
sin.greta_array <- function(x) {
  op("sin", x, tf_operation = "tf$sin")
}

#' @export
tan.greta_array <- function(x) {
  op("tan", x, tf_operation = "tf$tan")
}

#' @export
acos.greta_array <- function(x) {
  op("acos", x, tf_operation = "tf$acos")
}

#' @export
asin.greta_array <- function(x) {
  op("asin", x, tf_operation = "tf$asin")
}

#' @export
atan.greta_array <- function(x) {
  op("atan", x, tf_operation = "tf$atan")
}

#' @export
lgamma.greta_array <- function(x) {
  op("lgamma", x, tf_operation = "tf$math$lgamma")
}

#' @export
digamma.greta_array <- function(x) {
  op("digamma", x, tf_operation = "tf$math$digamma")
}

#' @export
t.greta_array <- function(x) {

  if (length(dim(x)) != 2)
    stop("only 2D arrays can be transposed")

  dims <- rev(dim(x))

  op("transpose",
     x,
     dim = dims,
     tf_operation = "tf_transpose")

}

#' @export
aperm.greta_array <- function(a, perm = NULL, ...) {

  dimnums <- seq_along(dim(a))

  if (is.null(perm))
    perm <- rev(dimnums)

  perm <- as.integer(perm)

  if (!identical(sort(perm), dimnums)) {
    stop("perm must be a reordering of the dimensions: ",
         paste(dimnums, collapse = ", "), " but was: ",
         paste(perm, collapse = ", "),
         call. = FALSE)
  }

  op("aperm", a,
     dim = dim(a)[perm],
     tf_operation = "tf$transpose",
     operation_args = list(perm = c(0L, perm)))

}

#' @export
chol.greta_array <- function(x, ...) {

  if (!identical(list(), list(...)))
    warning("chol() options are ignored for greta arrays")

  if (has_representation(x, "cholesky")) {
    result <- copy_representation(x, "cholesky")
  } else {
    dim <- dim(x)

    if (!(length(dim) == 2 && dim[1] == dim[2])) {
      stop("only two-dimensional, square, symmetric greta arrays ",
           "can be Cholesky decomposed")
    }

    result <- op("chol", x,
                 dim = dim,
                 tf_operation = "tf_chol")
  }

  result

}

#' @export
solve.greta_array <- function(a, b, ...) {

  # check a is 2D
  if (length(dim(a)) != 2) {
    stop(sprintf("'a' and 'b' must both be 2D, but 'a' has dimensions: %s",
                 paste(dim(a), collapse = " x ")))
  }

  # check the matrix is square
  if (dim(a)[1] != dim(a)[2]) {
    stop(sprintf("'a' must be square, but has %i rows and %i columns",
                 dim(a)[1], dim(a)[2]))
  }

  # if they just want the matrix inverse, do that
  if (missing(b)) {

    if (has_representation(a, "cholesky")) {
      U <- representation(a, "cholesky")
      result <- chol2inv(U)
    } else {
      result <- op("solve", a,
                   tf_operation = "tf$linalg$inv")
    }

  } else {

    # check b is 2D
    if (length(dim(b)) != 2) {
      stop(sprintf("'a' and 'b' must both be 2D, but 'b' has dimensions: %s",
                   paste(dim(b), collapse = " x ")))
    }

    # b must have the right number of rows
    if (dim(b)[1] != dim(a)[1]) {
      stop(sprintf(paste("'b' must have the same number of rows as 'a'",
                         "(%i), but has %i rows instead"),
                   dim(a)[1], dim(b)[1]))
    }

    # ... and solve the linear equations
    result <- op("solve", a, b,
                 dim = dim(b),
                 tf_operation = "tf$linalg$solve")

  }

  result

}

#' @rdname overloaded
#' @export
chol2inv <- function(x, size = NCOL(x), LINPACK = FALSE) {
  UseMethod("chol2inv", x)
}

#' @export
chol2inv.default <- function(x, size = NCOL(x), LINPACK = FALSE) {
  base::chol2inv(x = x, size = size, LINPACK = LINPACK)
}

#' @export
chol2inv.greta_array <- function(x, size = NCOL(x), LINPACK = FALSE) {

  if (!identical(LINPACK, FALSE)) {
    warning("'LINPACK' is ignored for greta arrays")
  }

  if (!identical(size, NCOL(x))) {
    warning("'size' is ignored for greta arrays")
  }

  op("chol2inv", x,
     tf_operation = "tf_chol2inv")

}

#' @rdname overloaded
#' @export
cov2cor <- function(V) {
  UseMethod("cov2cor", V)
}

#' @export
cov2cor.default <- function(V) {
  stats::cov2cor(V)
}

#' @export
cov2cor.greta_array <- function(V) {
  op("cov2cor", V,
     tf_operation = "tf_cov2cor")
}

# sum, prod, min, mean, max

#' @export
sum.greta_array <- function(..., na.rm = TRUE) {

  # combine all elements into a column vector
  vec <- c(...)

  # sum the elements
  op("sum", vec,
     dim = c(1, 1),
     tf_operation = "tf_sum")

}

#' @export
prod.greta_array <- function(..., na.rm = TRUE) {

  # combine all elements into a column vector
  vec <- c(...)

  # sum the elements
  op("prod", vec,
     dim = c(1, 1),
     tf_operation = "tf_prod")

}

#' @export
min.greta_array <- function(..., na.rm = TRUE) {

  # combine all elements into a column vector
  vec <- c(...)

  # sum the elements
  op("min", vec,
     dim = c(1, 1),
     tf_operation = "tf_min")

}

#' @export
mean.greta_array <- function(x, trim = 0, na.rm = TRUE, ...) {

  # sum the elements
  op("mean", x,
     dim = c(1, 1),
     tf_operation = "tf_mean")

}

#' @export
max.greta_array <- function(..., na.rm = TRUE) {

  # combine all elements into a column vector
  vec <- c(...)

  # sum the elements
  op("max", vec,
     dim = c(1, 1),
     tf_operation = "tf_max")

}

check_cum_op <- function(x) {
  dims <- dim(x)
  if (length(dims) > 2 | dims[2] != 1) {
    stop("'x' must be a column vector, but has dimensions ",
         paste(dims, collapse = " x "),
         call. = FALSE)
  }
}

#' @export
cumsum.greta_array <- function(x) {
  check_cum_op(x)
  op("cumsum", x, tf_operation = "tf_cumsum")
}

#' @export
cumprod.greta_array <- function(x) {
  check_cum_op(x)
  op("cumprod", x, tf_operation = "tf_cumprod")
}

# get the incides to reduce over, for colSums, rowSums, colMeans, rowMeans
rowcol_idx <- function(x, dims, which = c("col", "row")) {

  if (dims < 1L || dims > length(dim(x)) - 1L) {
    stop("invalid 'dims'", call. = FALSE)
  }

  switch(which,
         row = (dims + 1):length(dim(x)),
         col = seq_len(dims))

}

# get output dimension for colSums, rowSums, colMeans, rowMeans
rowcol_dim <- function(x, dims, which = c("row", "col")) {
  idx <- rowcol_idx(x, dims, which)
  dims <- dim(x)[-idx]
  if (length(dims) == 1)
    dims <- c(dims, 1L)
  dims
}

#' @rdname overloaded
#' @export
identity <- function(x) {
  UseMethod("identity", x)
}

#' @export
identity.default <- function(x) {
  base::identity(x)
}

#' @export
identity.greta_array <- function(x) {
  # make a copy
  op("identity", x, tf_operation = "tf$identity")
}

#' @rdname overloaded
#' @export
colMeans <- function(x, na.rm = FALSE, dims = 1L)
  UseMethod("colMeans", x)

#' @export
colMeans.default <- function(x, na.rm = FALSE, dims = 1L)
  base::colMeans(x = x, na.rm = na.rm, dims = dims)

#' @export
colMeans.greta_array <- function(x, na.rm = FALSE, dims = 1L) {

  op("colMeans", x,
     operation_args = list(dims = dims),
     tf_operation = "tf_colmeans",
     dim = rowcol_dim(x, dims, "col"))

}

#' @rdname overloaded
#' @export
rowMeans <- function(x, na.rm = FALSE, dims = 1L)
  UseMethod("rowMeans", x)

#' @export
rowMeans.default <- function(x, na.rm = FALSE, dims = 1L)
  base::rowMeans(x = x, na.rm = na.rm, dims = dims)

#' @export
rowMeans.greta_array <- function(x, na.rm = FALSE, dims = 1L) {

  op("rowMeans", x,
     operation_args = list(dims = dims),
     tf_operation = "tf_rowmeans",
     dim = rowcol_dim(x, dims, "row"))

}

#' @rdname overloaded
#' @export
colSums <- function(x, na.rm = FALSE, dims = 1L)
  UseMethod("colSums", x)

#' @export
colSums.default <- function(x, na.rm = FALSE, dims = 1L)
  base::colSums(x = x, na.rm = na.rm, dims = dims)

#' @export
colSums.greta_array <- function(x, na.rm = FALSE, dims = 1L) {

  op("colSums", x,
     operation_args = list(dims = dims),
     tf_operation = "tf_colsums",
     dim = rowcol_dim(x, dims, "col"))

}

#' @rdname overloaded
#' @export
rowSums <- function(x, na.rm = FALSE, dims = 1L)
  UseMethod("rowSums", x)

#' @export
rowSums.default <- function(x, na.rm = FALSE, dims = 1L)
  base::rowSums(x = x, na.rm = na.rm, dims = dims)

#' @export
rowSums.greta_array <- function(x, na.rm = FALSE, dims = 1L) {

  op("rowSums", x,
     operation_args = list(dims = dims),
     tf_operation = "tf_rowsums",
     dim = rowcol_dim(x, dims, "row"))

}

#' @rdname overloaded
#' @export
sweep <- function(x, MARGIN, STATS, FUN = "-", check.margin = TRUE, ...) {

  if (inherits(STATS, "greta_array"))
    x <- as.greta_array(x)

  UseMethod("sweep", x)

}

#' @export
sweep.default <- base::sweep

#' @export
sweep.greta_array <- function(x,
                              MARGIN,
                              STATS,
                              FUN = c("-", "+", "/", "*"),
                              check.margin = TRUE, ...) {

  # only allow these four functions
  FUN <- match.arg(FUN)

  STATS <- as.greta_array(STATS)

  if (!MARGIN %in% seq_len(2))
    stop("MARGIN can only be 1 or 2")

  # x must be 2D
  if (length(dim(x)) != 2) {
    stop(sprintf("x must be a 2D array, but has %i dimensions",
                 length(dim(x))))
  }

  # STATS must be a column array
  if (!(length(dim(STATS)) == 2 && dim(STATS)[2] == 1)) {
    stop(sprintf(paste("STATS must be a column vector array,",
                       "but has dimensions %s"),
                 paste(dim(STATS), collapse = " x ")))
  }

  # STATS must have the same dimension as the correct dim of x
  if (dim(x)[MARGIN] != dim(STATS)[1])
    stop("the number of elements of STATS does not match dim(x)[MARGIN]")

  op("sweep", x, STATS,
     operation_args = list(MARGIN = MARGIN, FUN = FUN),
     tf_operation = "tf_sweep",
     dim = dim(x)
  )

}

#' @import methods
setClass("greta_array")
setMethod("kronecker", signature(X = "greta_array", Y = "greta_array"),
          function(X, Y, FUN = c("*", "/", "+", "-"), make.dimnames = FALSE,
                   ...) {

            FUN <- match.arg(FUN)

            # X must be 2D
            if (length(dim(X)) != 2) {
              stop(sprintf("X must be a 2D array, but has %i dimensions",
                           length(dim(X))))
            }

            # Y must be 2D
            if (length(dim(Y)) != 2) {
              stop(sprintf("Y must be a 2D array, but has %i dimensions",
                           length(dim(X))))
            }

            tf_fun_name <- switch(FUN,
                                  `*` = "multiply",
                                  `/` = "truediv",
                                  `+` = "add",
                                  `-` = "subtract")

            op("kronecker", X, Y,
               tf_operation = "tf_kronecker",
               operation_args = list(tf_fun_name = tf_fun_name),
               dim = dim(X) * dim(Y))

          }
)

#' @import methods
setMethod(kronecker, signature(X = "array", Y = "greta_array"),
            function(X, Y, FUN = c("*", "/", "+", "-"), make.dimnames = FALSE,
                   ...) {

              kronecker(as.greta_array(X), Y, FUN, make.dimnames = FALSE)

          }
)

#' @import methods
setMethod(kronecker, signature(X = "greta_array", Y = "array"),
            function(X, Y, FUN = c("*", "/", "+", "-"), make.dimnames = FALSE,
                   ...) {

              kronecker(X, as.greta_array(Y), FUN, make.dimnames = FALSE)

          }
)

#' @rdname overloaded
#' @export
backsolve <- function(r, x, k = ncol(r),
                      upper.tri = TRUE,
                      transpose = FALSE) {
  UseMethod("backsolve", x)
}

#' @export
backsolve.default <- function(r, x, k = ncol(r),
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
    stop("k must equal ncol(r) for greta arrays",
         call. = FALSE)
  }

  if (transpose) {
    stop("transpose must be FALSE for greta arrays",
         call. = FALSE)
  }

  op("backsolve", r, x,
     operation_args = list(lower = !upper.tri),
     tf_operation = "tf$linalg$triangular_solve",
     dim = dim(x))

}

#' @rdname overloaded
#' @export
forwardsolve <- function(l, x, k = ncol(l),
                         upper.tri = FALSE,
                         transpose = FALSE) {
  UseMethod("forwardsolve", x)
}

# define this explicitly so CRAN doesn't think we're using .Internal
#' @export
forwardsolve.default <- function(l, x, k = ncol(l),
                                 upper.tri = FALSE,
                                 transpose = FALSE) {

  base::forwardsolve(l, x, k = ncol(l),
                     upper.tri = FALSE,
                     transpose = FALSE)
}

#' @export
forwardsolve.greta_array <- function(l, x,
                                     k = ncol(l),
                                     upper.tri = FALSE,
                                     transpose = FALSE) {
  if (k != ncol(l)) {
    stop("k must equal ncol(l) for greta arrays",
         call. = FALSE)
  }

  if (transpose) {
    stop("transpose must be FALSE for greta arrays",
         call. = FALSE)
  }

  op("forwardsolve", l, x,
     operation_args = list(lower = !upper.tri),
     tf_operation = "tf$linalg$triangular_solve",
     dim = dim(x))

}

#' @rdname overloaded
#' @export
apply <- function(X, MARGIN, FUN, ...) {
  UseMethod("apply", X)
}

#' @export
apply.default <- function(X, MARGIN, FUN, ...) {
  base::apply(X = X,
              MARGIN = MARGIN,
              FUN = FUN,
              ...)
}

#' @export
apply.greta_array <- function(X, MARGIN,
                              FUN = c("sum", "max", "mean", "min", "prod",
                                      "cumsum", "cumprod"),
                              ...) {

  FUN <- match.arg(FUN)

  if (inherits(MARGIN, "greta_array")) {
    stop("MARGIN cannot be a greta array",
         call. = FALSE)
  }

  MARGIN <- as.integer(MARGIN)

  # permute as in base::apply
  d <- dim(X)
  ds <- seq_along(d)

  s.call <- ds[-MARGIN]
  s.ans <- ds[MARGIN]

  d.call <- d[-MARGIN]
  d.ans <- d[MARGIN]

  d2 <- prod(d.ans)

  new_x <- aperm(X, c(s.call, s.ans))
  dim(new_x) <- c(prod(d.call), d2)

  # handle output dimensions
  reducing <- !FUN %in% c("cumsum", "cumprod")

  # set final and intermediate dimensions
  if (reducing) {
    dim_out <- d[MARGIN]
    if (length(dim_out) == 1)
      dim_out <- c(dim_out, 1L)
    dim <- c(prod(dim_out), 1)
  } else {
    dim_out <- c(prod(d[-MARGIN]), d[MARGIN])
    dim <- dim(new_x)
  }

  tf_fun_name <- FUN
  if (reducing) {
    tf_fun_name <- paste("reduce", tf_fun_name, sep = "_")
  }

  out <- op("apply", new_x,
            operation_args = list(axis = -2L,
                                  tf_fun_name = tf_fun_name),
            tf_operation = "tf_apply",
            dim = dim)

  # need to reshape when MARGIN is a vector, or when not reducing
  if (!reducing | length(MARGIN) > 1) {
    dim(out) <- dim_out
  }

  out

}


#' @rdname overloaded
#' @export
tapply <- function(X, INDEX, FUN, ...) {
  UseMethod("tapply", X)
}

#' @export
tapply.default <- function(X, INDEX, FUN = NULL, ...,
                           default = NA, simplify = TRUE) {
  base::tapply(X = X,
               INDEX = INDEX,
               FUN = FUN,
               ...,
               default = default,
               simplify = simplify)
}

#' @export
tapply.greta_array <- function(X, INDEX,
                               FUN = c("sum", "max", "mean", "min", "prod"),
                               ...) {

  FUN <- match.arg(FUN)

  if (inherits(INDEX, "greta_array")) {
    stop("INDEX cannot be a greta array",
         call. = FALSE)
  }

  # convert index to successive integers starting at 0
  groups <- sort(unique(INDEX))
  id <- match(INDEX, groups) - 1L
  len <- length(groups)

  dim_x <- dim(X)
  if (!(length(dim_x) == 2L && dim_x[2] == 1L)) {
    stop("X must be 2D greta array with one column, but has dimensions ",
         paste(dim_x, collapse = " x "),
         call. = FALSE)
  }

  op("tapply", X,
     operation_args = list(segment_ids = id,
                           num_segments = len,
                           op_name = FUN),
     tf_operation = "tf_tapply",
     dim = c(len, 1))

}

#' @rdname overloaded
#' @export
eigen <- function(x, symmetric, only.values, EISPACK) {
  UseMethod("eigen")
}

#' @export
eigen.default <- function(x, symmetric,
                          only.values = FALSE, EISPACK = FALSE) {
  base::eigen(x = x,
              symmetric = symmetric,
              only.values = only.values,
              EISPACK = EISPACK)
}

#' @export
eigen.greta_array <- function(x, symmetric,
                              only.values = FALSE, EISPACK = FALSE) {

  x <- as.greta_array(x)

  if (missing(symmetric)) symmetric <- TRUE
  dims <- dim(x)

  if (length(dims) != 2 | dims[1] != dims[2] | !symmetric) {
    stop("only two-dimensional, square, symmetric greta arrays ",
         "can be eigendecomposed",
         call. = FALSE)
  }

  # they just want the eigenvalues, use that tf method
  if (only.values) {

    values <- op("eigenvalues", x,
                 dim = nrow(x),
                 tf_operation = "tf_only_eigenvalues")

    vectors <- NULL

  } else {

    # if we're doing the whole eigendecomposition, do it in three operations

    # a wacky greta array which apparently has the same dimension as x; but in
    # fact is a list of the two elements. But that's OK so long as the user
    # never sees it
    eig <- op("eigen", x,
              tf_operation = "tf$linalg$eigh")

    # get the eigenvalues and vectors as actual, sane greta arrays
    values <- op("values", eig, dim = c(nrow(eig), 1L),
                 tf_operation = "tf_extract_eigenvalues")

    vectors <- op("vectors", eig,
                  tf_operation = "tf_extract_eigenvectors")

  }

  list(values = values,
       vectors = vectors)

}


#' @rdname overloaded
#' @export
rdist <- function(x1, x2 = NULL, compact = FALSE) {
  UseMethod("rdist")
}

#' @export
rdist.default <- function(x1, x2 = NULL, compact = FALSE) {
  # error nicely if they don't have fields installed
  fields_installed <- requireNamespace("fields", quietly = TRUE)
  if (!fields_installed) {
    stop("rdist is being called on R arrays (not greta arrays), ",
         "but the fields package is not installed",
         call. = FALSE)
  }
  fields::rdist(x1 = x1,
                x2 = x2,
                compact = compact)
}

#' @export
rdist.greta_array <- function(x1, x2 = NULL, compact = FALSE) {

  if (!identical(compact, FALSE)) {
    warning("'compact' is ignored for greta arrays")
  }

  x1 <- as.greta_array(x1)

  # like rdist, convert to a column vector if it has too many dimensions
  if (length(dim(x1)) != 2) {
    x1 <- flatten(x1)
  }

  n1 <- nrow(x1)

  # square self-distance matrix
  if (is.null(x2)) {

    op("rdist", x1,
       tf_operation = "tf_self_distance",
       dim = c(n1, n1))

  } else {

    # possibly non-square pairwise distance matrix

    x2 <- as.greta_array(x2)

    if (length(dim(x2)) != 2) {
      x2 <- flatten(x2)
    }

    # error if they have different number of columns. fields::rdist allows
    # different numbers of columns, takes the number of columns from x1,and
    # sometimes gives nonsense results
    if (ncol(x1) != ncol(x2)) {

      stop("x1 and x2 must have the same number of columns, but had ",
           ncol(x1), " and ", ncol(x2), " columns",
           call. = FALSE)

    }

    n2 <- nrow(x2)

    op("rdist", x1, x2,
       tf_operation = "tf_distance",
       dim = c(n1, n2))

  }

}
