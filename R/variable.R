#' @rdname variable
#' @export
#' @title greta variables
#' @description \code{variable()} creates greta arrays representing unknown
#'   parameters, to be lerned during model fitting. These parameters are not
#'   associated with a probability distribution. To create a variable greta
#'   array following a specific probability distribution, see
#'   \code{\link{greta-distributions}}.
#'
#'  \code{free()} is an alias for \code{variable()}. It is deprecated and
#'  will be removed in version 0.2
#'
#' @param lower,upper scalar values giving optional limits to variables. These
#'  must be specified as numerics, they cannot be greta arrays (though see
#'  details for a workaround). They can be set to \code{-Inf} (\code{lower}) or
#'  \code{Inf} (\code{upper}), though \code{lower} must always be less than
#'  \code{upper}.
#'
#' @param dim the dimensions of the greta array to be returned, either a scalar
#'  or a vector of positive integers. See details.
#'
#' @details \code{lower} and \code{upper} must be fixed, they cannot be greta arrays.
#'  This ensures these values can always be transformed to a continuous scale to
#'  run the samplers efficiently. However, a variable parameter with dynamic
#'  limits can always be created by first defining a variable constrained
#'  between 0 and 1, and then transforming it to the required scale. See below
#'  for an example.
#'
#' @examples
#' # a scalar variable
#' a <- variable()
#'
#' # a positive length-three variable
#' b <- variable(lower = 0, dim = 3)
#'
#' # a 2x2x2 variable bounded between 0 and 1
#' c <- variable(lower = 0, upper = 1, dim = c(2, 2, 2))
#'
#' # create a variable, with lower and upper defined by greta arrays
#' min <- as_data(iris$Sepal.Length)
#' max <- min ^ 2
#' d <- min + variable(0, 1, dim = nrow(iris)) * (max - min)
#'
variable <- function (lower = -Inf, upper = Inf, dim = 1) {

  if (is.greta_array(lower) | is.greta_array(upper))
    stop ('lower and upper must be fixed, they cannot be another greta array')

  node <- variable_node$new(lower, upper, dim)
  as.greta_array(node)

}

#' @rdname variable
#' @export
free <- variable
