#' @title Cholesky Factor to Symmetric Matrix
#'
#' @description Evaluate `t(x) \%*\% x` efficiently, where `x` is the
#'   (upper-triangular) Cholesky factor of a symmetric, positive definite square
#'   matrix. I.e. it is the inverse of `chol`.
#'
#' @param x a square, upper triangular matrix representing the Cholesky
#'   factor of a symmetric, positive definite square matrix
#'
#' @export
#'
#' @examples
#' # a symmetric, positive definite square matrix
#' y <- rWishart(1, 4, diag(3))[, , 1]
#' y
#' u <- chol(y)
#' u
#' chol2symm(u)
#' identical(y, chol2symm(u))
#' identical(chol2symm(u), t(u) %*% u)
#' \dontrun{
#' u_greta <- cholesky_variable(3)
#' y_greta <- chol2symm(u)
#' }
chol2symm <- function(x) {
  UseMethod("chol2symm")
}

#' @export
chol2symm.default <- function(x) {
  check_chol2symm_square_symmetric_upper_tri_matrix(x)

  t(x) %*% x
}

#' @export
chol2symm.greta_array <- function(x) {
  ## TF1/2
  ## Does this extra coercion need to be here?
  ## in order for this to dispatch with S3 it must be a greta array already?
  x <- as.greta_array(x)

  check_chol2symm_2d_square_upper_tri_greta_array(x)

  # sum the elements
  op(
    "chol2symm",
    x,
    tf_operation = "tf_chol2symm",
    representations = list(cholesky = x)
  )
}
