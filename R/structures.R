#' @name structures
#' @title create data greta arrays
#' @description These structures can be used to set up more complex models. For
#'   example, scalar parameters can be embedded in a greta array by first
#'   creating a greta array with \code{zeros()} or \code{ones()}, and then
#'   embedding the parameter value using greta's replacement syntax.
#' @param ... dimensions of the greta arrays to create
#' @param data a vector giving data to fill the greta array. Other object types
#'   are coerced by \code{\link{as.vector}}.
#' @param dim an integer vector giving the dimensions for the greta array to be
#'   created.
#'
#' @details \code{greta_array} is a convenience function to create an R array
#'   with \code{\link{array}} and then coerce it to a greta array. I.e. it is
#'   equivalent to \code{as_data(array(data, dim))}.
#'
#' @return a greta array object
#' @examples
#' \dontrun{
#'
#' # a 3 row, 4 column greta array of 0s
#' z <- zeros(3, 4)
#'
#' # a 3x3x3 greta array of 1s
#' z <- ones(3, 3, 3)
#'
#' # a 2x4 greta array filled with pi
#' z <- greta_array(pi, dim = c(2, 4))
#'
#' # a 3x3x3 greta array filled with 1, 2, and 3
#' z <- greta_array(1:3, dim = c(3, 3, 3))
#' }

NULL

#' @export
#' @rdname structures
zeros <- function (...)
  as.greta_array(array(data = 0, dim = c(...)))

#' @export
#' @rdname structures
ones <- function (...)
  as.greta_array(array(data = 1, dim = c(...)))

#' @export
#' @rdname structures
greta_array <- function (data = 0, dim = length(data))
  as.greta_array(array(data = data, dim = dim))
