#' @name greta-transforms
#' @aliases greta-inverse-links
#' @title Transformation Functions for Greta Arrays
#'
#' @description transformations for greta arrays, which may also be
#'   used as inverse link functions. Also see \link{greta-operators} and
#'   \link{greta-functions}.
#'
#' @param x a real-valued (i.e. values ranging from -Inf to Inf) greta array to
#'   transform to a constrained value
#' @details greta does not allow you to state the transformation/link on the
#'   left hand side of an assignment, as is common in the BUGS and STAN
#'   modelling languages. That's because the same syntax has a very different
#'   meaning in R, and can only be applied to objects that are already in
#'   existence. The inverse forms of the common link functions (prefixed with an
#'   'i') are therefore more likely to useful in modelling, and these are what
#'   are provided here.
#'
#'   The \code{log1pe} inverse link function is equivalent to \code{log(1 + exp(x))},
#'   yielding a positive transformed parameter. Unlike the log transformation,
#'   this transformation is approximately linear for x > 1. i.e. when \eqn{x >
#'   1}, \eqn{y \approx x}
#'
#' @examples
#'  x = normal(1, 3, dim = 10)
#'
#'  # transformation to the unit interval
#'  p1 <- iprobit(x)
#'  p2 <- ilogit(x)
#'  p3 <- icloglog(x)
#'  p4 <- icauchit(x)
#'
#'  # and to positive reals
#'  y <- log1pe(x)
NULL

# inverse link functions in tensorflow
tf_iprobit <- function (x) {
  (tf$erf(x / fl(sqrt(2))) + fl(1)) / fl(2)
}

tf_ilogit <- function (x)
  tf$nn$sigmoid(x)

tf_icloglog <- function (x)
  fl(1) - tf$exp(-tf$exp(x))

tf_icauchit <- function (x)
  fl(1 / pi) * tf$atan(x) + fl(0.5)

tf_log1pe <- function (x)
  tf$nn$softplus(x)

#' @rdname greta-transforms
#' @export
iprobit <- function (x)
  op('iprobit', x,
     tf_operation = 'tf_iprobit')

#' @rdname greta-transforms
#' @export
ilogit <- function (x)
  op('ilogit', x,
     tf_operation = 'tf_ilogit')

#' @rdname greta-transforms
#' @export
icloglog <- function (x)
  op('icloglog', x,
     tf_operation = 'tf_icloglog')

#' @rdname greta-transforms
#' @export
icauchit <- function (x)
  op('icauchit', x,
     tf_operation = 'tf_icauchit')

#' @rdname greta-transforms
#' @export
log1pe <- function (x)
  op('log1pe', x,
     tf_operation = 'tf_log1pe')
