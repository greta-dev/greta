#' @name greta-transforms
#' @aliases greta-inverse-links
#' @title transformation functions for greta nodes
#'
#' @description transformations for greta nodes, which may also be
#'   used as inverse link functions. Also see \link{greta-operators} and
#'   \link{greta-functions}.
#'
#' @param x a real-valued (i.e. values ranging from -Inf to Inf) greta node to
#'   transform to a constrained value
#' @details greta does not allow you to state the transformation/link on the
#'   left hand side of an assignment, as is common in the BUGS and STAN
#'   modelling languages. That's because the same syntax has a very different
#'   meaning in R, and can only be applied to objects that are already in
#'   existence. The inverse forms of the common link functions (prefixed with an
#'   'i') are therefore more likely to useful in modelling, and these are what
#'   are provided here.
#'
#'   The log1pe inverse link function is equivalent to \code{log(1 + exp(x))},
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
  sqrt2 <- tf$constant(sqrt(2))
  (tf$erf(x / sqrt2) + 1) / 2
}

tf_ilogit <- function (x)
  1 / (1 + exp(-x))

tf_icloglog <- function (x)
  1 - exp(-exp(x))

tf_icauchit <- function (x)
  (1 / pi) * atan(x) + 0.5

tf_log1pe <- function (x)
  log1p(exp(x))

#' @rdname greta-transforms
#' @export
iprobit <- function (x) op('tf_iprobit', x)

#' @rdname greta-transforms
#' @export
ilogit <- function (x) op('tf_ilogit', x)

#' @rdname greta-transforms
#' @export
icloglog <- function (x) op('tf_icloglog', x)

#' @rdname greta-transforms
#' @export
icauchit <- function (x) op('tf_icauchit', x)

#' @rdname greta-transforms
#' @export
log1pe <- function (x) op('tf_log1pe', x)


