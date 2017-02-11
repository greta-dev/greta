#' @name greta-structures
#' @title greta data structures
#' @description These structures can be used to set up more complex models. For
#'   example, scalar parameters can be embedded in a matrix by first creating a
#'   matrix with \code{zeros()} or \code{ones()}, and then embedding the
#'   parameter value using greta's replacement syntax
#' @param ... dimensions of the arrays to create
#' @return a greta node object which may be manipulated in the same way as data
#'   or parameter nodes
NULL

#' @export
#' @rdname greta-structures
#' @examples
#' # a 3 row, 4 column array of 0s
#' z <- zeros(3, 4)
#'
zeros <- function (...)
  to_node(array(data = 0, dim = c(...)))

#' @export
#' @rdname greta-structures
#' @examples
#' # a 3x3x3 array of 1s
#' z <- ones(3, 3, 3)
ones <- function (...)
  to_node(array(data = 1, dim = c(...)))
