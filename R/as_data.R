#' @name as_data
#' @title convert other objects to greta arrays
#' @description define an object in an R session as a data greta array for use
#'   as data in a greta model.
#' @param x an R object that can be coerced to a greta_array (see details).
#' @details `as_data()` can currently convert R objects to greta_arrays if
#'   they are numeric or logical vectors, matrices or arrays; or if they are
#'   dataframes with only numeric (including integer) or logical elements.
#'   Logical elements are always converted to numerics. R objects cannot be
#'   converted if they contain missing (`NA`) or infinite (`-Inf` or
#'   `Inf`) values.
#' @export
#' @examples
#' \dontrun{
#'
#' # numeric/integer/logical vectors, matrices and arrays can all be coerced to
#' # data greta arrays
#'
#' vec <- rnorm(10)
#' mat <- matrix(seq_len(3 * 4), nrow = 3)
#' arr <- array(sample(c(TRUE, FALSE), 2 * 2 * 2, replace = TRUE),
#'   dim = c(2, 2, 2)
#' )
#' (a <- as_data(vec))
#' (b <- as_data(mat))
#' (c <- as_data(arr))
#'
#' # dataframes can also be coerced, provided all the columns are numeric,
#' # integer or logical
#' df <- data.frame(
#'   x1 = rnorm(10),
#'   x2 = sample(1L:10L),
#'   x3 = sample(c(TRUE, FALSE), 10, replace = TRUE)
#' )
#' (d <- as_data(df))
#' }
as_data <- function(x) {
  check_tf_version("error")
  UseMethod("as_data", x)
}


# if it's already a *data* greta_array fine, else error
# nolint start
#' @export
as_data.greta_array <- function(x) {
  # nolint end
  non_data_greta_array <- !is.data_node(get_node(x))
  if (non_data_greta_array) {
    cli::cli_abort(
      "cannot coerce a non-data {.cls greta_array} to data"
    )
  }
  x
}

# otherwise try to coerce to a greta array
# nolint start
#' @export
as_data.default <- function(x) {
  # nolint end
  as.greta_array(x)
}
