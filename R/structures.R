#' @name structures
#' @title create data greta arrays
#' @description These structures can be used to set up more complex models. For
#'   example, scalar parameters can be embedded in a greta array by first
#'   creating a greta array with `zeros()` or `ones()`, and then
#'   embedding the parameter value using greta's replacement syntax.
#' @param ... dimensions of the greta arrays to create
#' @param data a vector giving data to fill the greta array. Other object types
#'   are coerced by [as.vector()].
#' @param dim an integer vector giving the dimensions for the greta array to be
#'   created.
#'
#' @details `greta_array` is a convenience function to create an R array
#'   with [array()] and then coerce it to a greta array. I.e. when
#'   passed something that can be coerced to a numeric array, it is equivalent
#'   to `as_data(array(data, dim))`.
#'
#'   If `data` is a greta array and
#'   dim is different than `dim(data)`, a reshaped greta array is returned.
#'   This is equivalent to: `dim(data) <- dim`.
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
zeros <- function(...) {
  dims <- c(...)
  names(dims) <- NULL
  greta_array(0, dims)
}

#' @export
#' @rdname structures
ones <- function(...) {
  dims <- c(...)
  names(dims) <- NULL
  greta_array(1, dims)
}

#' @export
#' @rdname structures
greta_array <- function(data = 0, dim = length(data))
  UseMethod("greta_array")


# safely handle self-coersion, possibly with reshaping
#' @export
greta_array.greta_array <- function(data = 0, dim = length(data)) {

  # reshape if necessary (apparently users expect this functionality)
  dim <- as.integer(dim)
  if (length(dim) == 1)
    dim <- c(dim, 1L)

  if (!identical(dim, dim(data))) {
    dim(data) <- dim
  }

  data

}

# else try to coerce to a greta array
#' @export
greta_array.default <- function(data = 0, dim = length(data)) {
  as.greta_array(array(data = data, dim = dim))
}
