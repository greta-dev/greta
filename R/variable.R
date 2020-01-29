#' @rdname variable
#' @export
#' @title create greta variables
#' @description \code{variable()} creates greta arrays representing unknown
#'   parameters, to be learned during model fitting. These parameters are not
#'   associated with a probability distribution. To create a variable greta
#'   array following a specific probability distribution, see
#'   \code{\link{distributions}}.
#'
#' @param lower,upper optional limits to variables. These must be specified as
#'   numerics, they cannot be greta arrays (though see details for a
#'   workaround). They can be set to \code{-Inf} (\code{lower}) or \code{Inf}
#'   (\code{upper}), though \code{lower} must always be less than \code{upper}.
#'
#' @param dim the dimensions of the greta array to be returned, either a scalar
#'  or a vector of positive integers. See details.
#'
#' @details \code{lower} and \code{upper} must be fixed, they cannot be greta
#'   arrays. This ensures these values can always be transformed to a continuous
#'   scale to run the samplers efficiently. However, a variable parameter with
#'   dynamic limits can always be created by first defining a variable
#'   constrained between 0 and 1, and then transforming it to the required
#'   scale. See below for an example.
#'
#' @examples
#' \dontrun{
#'
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
#' }
variable <- function(lower = -Inf, upper = Inf, dim = NULL) {

  check_tf_version("error")

  if (inherits(lower, "greta_array") | inherits(upper, "greta_array"))
    stop("lower and upper must be fixed, they cannot be another greta array")

  node <- variable_node$new(lower, upper, dim)
  as.greta_array(node)

}

# to be exported later = a cholesky factor variable (possibly for a correlation matrix)
cholesky_variable <- function (dim, correlation = FALSE) {

  # dimension of the free state version
  free_dim <- ifelse(correlation,
                     dim * (dim - 1) / 2,
                     dim + dim * (dim - 1) / 2)

  # create variable node
  node <- vble(truncation = c(-Inf, Inf),
               dim = c(dim, dim),
               free_dim = free_dim)

  # set the constraint, to enable transformation
  node$constraint <- ifelse(correlation,
                            "correlation_matrix",
                            "covariance_matrix")

  # set the printed value to be nicer
  cholesky_value <- unknowns(c(dim, dim))
  cholesky_value[lower.tri(cholesky_value, )] <- 0
  node$value(cholesky_value)

  # reeturn as a greta array
  as.greta_array(node)

}
