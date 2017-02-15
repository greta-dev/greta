# dynamics module

# iterate matrix tensor `mat` `niter` times, each time using and updating vector
# tensor `state`
tf_iterate_lambda <- function (mat, state, niter) {

  # store states (can't overwrite since we need to maintain the chain of nodes)
  states <- list(state)

  # iterate the matrix
  for (i in seq_len(niter))
    states[[i + 1]] <-  tf$matmul(mat, states[[i]])

  # return the final growth rate (should be same for all states at convergence)
  lambda <- states[[niter + 1]][1] / states[[niter]][1]
  tf$reshape(lambda, shape = c(1L, 1L))

}

# apply iterate_lambda to a series of n matrices of dimension m, stored as an n
# x m^2 matrix, each row being unpacked *rowwise*
tf_iterate_lambda_vectorised <- function (mat, state, n, m, niter) {

  # loop through rows, getting lambda
  lambdas_list <- lapply(seq_len(n) - 1,
                         function (i) {
                           mat_i <- tf$reshape(mat[i, ], shape = shape(m, m))
                           tf_iterate_lambda(mat_i, state, niter)
                         })

  lambdas <- tf$concat(0L, lambdas_list)
  lambdas

}

# node functions exposed via module
iterate_lambda <- function(matrix, state, niter) {

  niter <- as.integer(niter)

  dimfun <- function(node_list) {

    # input dimensions
    matrix_dim <- node_list[[1]]$dim
    state_dim <- node_list[[2]]$dim

    if (length(state_dim) != 2 | state_dim[2] != 1)
      stop ('state must be a column vector (rank 2 tensor)')

    if (length(matrix_dim) != 2 | matrix_dim[1] != matrix_dim[2])
      stop ('matrix must be a square matrix (rank 2 tensor)')

    if (matrix_dim[2] != state_dim[1])
      stop ('number of elements in state must match the dimension of matrix')

    # output dimensions
    state_dim
  }

  op('tf_iterate_lambda',
     matrix,
     state,
     operation_args = list(niter = niter),
     dimfun = dimfun)

}

iterate_lambda_vectorised <- function(matrices, state, n, m, niter) {

  n <- as.integer(n)
  m <- as.integer(m)
  niter <- as.integer(niter)

  dimfun <- function(node_list) {

    # input dimensions
    matrices_dim <- node_list[[1]]$dim
    state_dim <- node_list[[2]]$dim

    if (length(state_dim) != 2 | state_dim[2] != 1)
      stop ('state must be a column vector (rank 2 tensor)')

    if (m != state_dim[1])
      stop ('number of elements in state must match the dimension of matrix')

    if (length(matrices_dim) != 2 | matrices_dim[2] != (m ^ 2) | matrices_dim[1] != n)
      stop ('matrix must be a rectangular matrix (rank 2 tensor) with dimensions n x m^2')

    # output dimensions
    c(n, 1)

  }

  op('tf_iterate_lambda_vectorised',
     matrices,
     state,
     operation_args = list(n = n,
                           m = m,
                           niter = niter),
     dimfun = dimfun)

}

#' @name dynamics-module
#' @aliases dynamics
#' @title Functions for modelling dynamical systems
#'
#' @description A module providing functions specific to dynamical modelling. So
#'   far only for iterating Leslie matrices. \code{iterate_lambda} iterates a
#'   matrix a certain number of times and returns, as a scalar node, the
#'   terminal growth rate for the first element of the state vector.
#'   \code{iterate_lambda_vectorised} is a vectorised version for iterating over
#'   multiple matrices, returning a vector of growth rates.
#'
NULL

#' @name iterate_lambda
#' @rdname dynamics-module
#' @usage dynamics$iterate_lambda(matrix, state, niter)
#' @param matrix a square matrix node representing transition probabilities
#'   between states
#' @param state a column vector node representing the initial state from which
#'   to iterate the matrix
#' @param niter a positive integer giving the number of times to iterate the
#'   matrix
NULL

#' @name iterate_lambda_vectorised
#' @rdname dynamics-module
#' @usage dynamics$iterate_lambda_vectorised(matrices, state, n, m, niter)
#' @param matrices a rectangular matrix node of dimension n x m^2, each row of
#'   which gives the rowwise elements of a different m x m matrix to iterate
#' @param n the number of m x m matrices to be iterated (firstdimensions of
#'   \code{matrices})
#' @param m the dimension of each matrix to be iterated
NULL

#' @export
dynamics <- list(iterate_lambda = iterate_lambda,
                 iterate_lambda_vectorised = iterate_lambda_vectorised)
