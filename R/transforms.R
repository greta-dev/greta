#' @name transforms
#' @aliases inverse-links
#' @title transformation functions for greta arrays
#'
#' @description transformations for greta arrays, which may also be used as
#'   inverse link functions. Also see \link{operators} and \link{functions}.
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
#' \dontrun{
#'
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
#' }
NULL

#' @rdname transforms
#' @export
iprobit <- function (x)
  op('iprobit', x, tf_operation = tf_iprobit)

#' @rdname transforms
#' @export
ilogit <- function (x)
  op('ilogit', x, tf_operation = tf$nn$sigmoid)

#' @rdname transforms
#' @export
icloglog <- function (x)
  op('icloglog', x, tf_operation = tf_icloglog)

#' @rdname transforms
#' @export
icauchit <- function (x)
  op('icauchit', x, tf_operation = tf_icauchit)

#' @rdname transforms
#' @export
log1pe <- function (x)
  op('log1pe', x, tf_operation = tf$nn$softplus)
