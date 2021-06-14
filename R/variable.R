#' @rdname variable
#' @export
#' @title create greta variables
#' @description `variable()` creates greta arrays representing unknown
#'   parameters, to be learned during model fitting. These parameters are not
#'   associated with a probability distribution. To create a variable greta
#'   array following a specific probability distribution, see
#'   [distributions()].
#'
#' @param lower,upper optional limits to variables. These must be specified as
#'   numerics, they cannot be greta arrays (though see details for a
#'   workaround). They can be set to `-Inf` (`lower`) or `Inf`
#'   (`upper`), though `lower` must always be less than `upper`.
#'
#' @param dim the dimensions of the greta array to be returned, either a scalar
#'   or a vector of positive integers. See details.
#'
#' @details `lower` and `upper` must be fixed, they cannot be greta
#'   arrays. This ensures these values can always be transformed to a continuous
#'   scale to run the samplers efficiently. However, a variable parameter with
#'   dynamic limits can always be created by first defining a variable
#'   constrained between 0 and 1, and then transforming it to the required
#'   scale. See below for an example.
#'
#'   The constraints in `simplex_variable()` and `ordered_variable()`
#'   operate on the final dimension, which must have more than 1 element.
#'   Passing in a scalar value for `dim` therefore results in a row-vector.
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
#' max <- min^2
#' d <- min + variable(0, 1, dim = nrow(iris)) * (max - min)
#' }
variable <- function(lower = -Inf, upper = Inf, dim = NULL) {
  check_tf_version("error")

  if (inherits(lower, "greta_array") | inherits(upper, "greta_array")) {
    msg <- cli::format_error(
      "lower and upper must be fixed, they cannot be another greta array"
    )
    stop(
      msg,
      call. = FALSE
    )
  }

  node <- variable_node$new(lower, upper, dim)
  as.greta_array(node)
}

#' @export
#' @rdname variable
#'
#' @param correlation whether to return a cholesky factor corresponding to a
#'   correlation matrix (diagonal elements equalling 1, off-diagonal elements
#'   between -1 and 1).
#'
#' @examples
#' # 4x4 cholesky factor variables for covariance and correlation matrices
#' e_cov <- cholesky_variable(dim = 4)
#' e_correl <- cholesky_variable(dim = 4, correlation = TRUE)
#'
#' # these can be converted to symmetic matrices with chol2symm
#' # (equivalent to t(e_cov) %*% e_cov, but more efficient)
#' cov <- chol2symm(e_cov)
#' correl <- chol2symm(e_correl)
cholesky_variable <- function(dim, correlation = FALSE) {
  n_dim <- length(dim)
  if (n_dim == 1) {
    dim <- c(dim, dim)
  } else if (n_dim == 2) {
    if (dim[1] != dim[2]) {
      msg <- cli::format_error(
        c(
          "cholesky variables must be square",
          "However dim was: {.val {paste(dim, collapse = ' x ')}}"
        )
      )
      stop(
        msg,
        call. = FALSE
      )
    }
  } else {
    msg <- cli::format_error(
      "dim can either be a scalar or a vector of length 2"
    )
    stop(
      msg,
      call. = FALSE
    )
  }

  k <- dim[1]

  # dimension of the free state version
  free_dim <- ifelse(correlation,
    k * (k - 1) / 2,
    k + k * (k - 1) / 2
  )

  # create variable node
  node <- vble(
    truncation = c(-Inf, Inf),
    dim = dim,
    free_dim = free_dim
  )

  # set the constraint, to enable transformation
  node$constraint <- ifelse(correlation,
    "correlation_matrix",
    "covariance_matrix"
  )

  # set the printed value to be nicer
  cholesky_value <- unknowns(dim)
  cholesky_value[lower.tri(cholesky_value, )] <- 0
  node$value(cholesky_value)

  # reeturn as a greta array
  as.greta_array(node)
}

#' @export
#' @rdname variable
#'
#' @examples
#' # a 4D simplex (sums to 1, all values positive)
#' f <- simplex_variable(4)
#'
#' # a 4D simplex on the final dimension
#' g <- simplex_variable(dim = c(2, 3, 4))
simplex_variable <- function(dim) {

  # for scalar dims, return a row vector
  if (length(dim) == 1) {
    dim <- c(1, dim)
  }

  dim <- check_dims(target_dim = dim)

  # dimension of the free state version
  n_dim <- length(dim)
  last_dim <- dim[n_dim]
  if (!last_dim > 1) {
    msg <- cli::format_error(
      "the final dimension of a simplex variable must have more than one \\
      element"
    )
    stop(
      msg,
      call. = FALSE
    )
  }

  raw_dim <- dim
  raw_dim[n_dim] <- last_dim - 1
  free_dim <- prod(raw_dim)

  # create variable node
  node <- vble(
    truncation = c(-Inf, Inf),
    dim = dim,
    free_dim = free_dim
  )

  # set the constraint, to enable transformation
  node$constraint <- "simplex"

  # reeturn as a greta array
  as.greta_array(node)
}

#' @export
#' @rdname variable
#' @examples
#' # a 2D variable with each element higher than the one in the cell to the left
#' h <- ordered_variable(dim = c(3, 4))
#'
#' # more constraints can be added with monotonic transformations, e.g. an
#' # ordered positive variable
#' i <- exp(ordered_variable(5))
ordered_variable <- function(dim) {

  # for scalar dims, return a row vector
  if (length(dim) == 1) {
    dim <- c(1, dim)
  }

  dim <- check_dims(target_dim = dim)

  # dimension of the free state version
  n_dim <- length(dim)
  if (!dim[n_dim] > 1) {
    msg <- cli::format_error(
      "the final dimension of an ordered variable must have more than \\
      one element"
    )
    stop(
      msg,
      call. = FALSE
    )
  }

  # create variable node
  node <- vble(truncation = c(-Inf, Inf), dim = dim)

  # set the constraint, to enable transformation
  node$constraint <- "ordered"

  # reeturn as a greta array
  as.greta_array(node)
}
