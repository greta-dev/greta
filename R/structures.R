#' @name greta-structures
#' @title greta Data Structures
#' @description These structures can be used to set up more complex models. For
#'   example, scalar parameters can be embedded in a greta array by first creating a
#'   greta array with \code{zeros()} or \code{ones()}, and then embedding the
#'   parameter value using greta's replacement syntax
#' @param ... dimensions of the greta arrays to create
#' @return a greta array object
NULL

#' @export
#' @rdname greta-structures
#' @examples
#' # a 3 row, 4 column greta array of 0s
#' z <- zeros(3, 4)
#'
zeros <- function (...)
  ga(array(data = 0, dim = c(...)))

#' @export
#' @rdname greta-structures
#' @examples
#' # a 3x3x3 greta array of 1s
#' z <- ones(3, 3, 3)
ones <- function (...)
  ga(array(data = 1, dim = c(...)))
