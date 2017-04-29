#' @name as_data
#' @title Declare R Objects as Data
#' @description define an object in an R session as a data greta array for use
#'   as data in a greta model.
#' @param x an R object that can be coerced to a greta_array (see details).
#' @details \code{as_data()} can currently convert R objects to greta_arrays if they are
#'   numeric or logical vectors, matrices or arrays; or if they are dataframes
#'   with only numeric (including integer) or logical elements. Logical elements
#'   are always converted to numerics. R objects cannot be converted if they
#'   contain missing (\code{NA}) or infinite (\code{-Inf} or \code{Inf}) values.
#' @export
#' @examples
#'
#' # numeric/integer/logical vectors, matrices and arrays can all be coerced to
#' # data greta arrays
#'
#' vec <- rnorm(10)
#' mat <- matrix(seq_len(3 * 4), nrow = 3)
#' arr <- array(sample(c(TRUE, FALSE), 2 * 2 * 2, replace = TRUE), dim = c(2, 2, 2))
#' (a <- as_data(vec))
#' (b <- as_data(mat))
#' (c <- as_data(arr))
#'
#' # dataframes can also be coerced, provided all the columns are numeric,
#' # integer or logical
#' df <- data.frame(x1 = rnorm(10),
#'                  x2 = sample(1L:10L),
#'                  x3 = sample(c(TRUE, FALSE), 10, replace = TRUE))
#' (d <- as_data(df))
#'
as_data <- function (x)
  UseMethod('as_data', x)

# if it's already a data greta_array fine, else error
#' @export
as_data.greta_array <- function (x) {
  if (x$node$type != 'data')
    stop ('cannot coerce a non-data greta_array to data')
  x
}

# coerce logical vectors to numerics
#' @export
as_data.logical <- function (x)
  as_data.numeric(as.numeric(x))

# coerce dataframes if all columns can safely be converted to numeric, error
# otherwise
#' @export
as_data.data.frame <- function (x) {
  classes <- vapply(x, class, '')
  valid <- classes %in% c('numeric', 'integer', 'logical')

  if (!all(valid)) {
    invalid_types <- unique(classes[!valid])
    stop ('cannot coerce a dataframe to a greta_array unless all columns are ',
          'numeric, integer or logical. This dataframe had columns of type:   ',
          paste(invalid_types, collapse = ', '))
  }

  as_data.numeric(as.matrix(x))

}

# coerce logical matrices to numeric matrices, and error if they aren't logical
# or numeric
#' @export
as_data.matrix <- function (x) {
  if (!is.numeric(x)) {

    if (is.logical(x))
      x[] <- as.numeric(x[])
    else
      stop ('cannot convert a matrix to a greta_array unless it is numeric or logical')

  }

  as_data.numeric(x)

}

# finally, reject if there are any missing values, or set up the greta_array
#' @export
as_data.numeric <- function (x) {
  if (any(!is.finite(x)))
    stop ('cannot convert objects with missing or infinite values to greta_arrays')
  ga(data_node$new(x))
}

# otherwise error
#' @export
as_data.generic <- function (x) {
  stop ('objects of class ',
        paste(class(x), collapse = ' or '),
        ' cannot be coerced to greta arrays')
}

