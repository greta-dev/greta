# dynamics module

# iterate matrix tensor `mat` `niter` times, each time using and updating vector
# tensor `state`, and return lambda for the final iteration
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

# iterate matrix tensor `mat` `niter` times, each time using and updating vector
# tensor `state`, and return the final state
tf_iterate_state <- function (mat, state, niter) {

  # store states (can't overwrite since we need to maintain the chain of nodes)
  states <- list(state)

  # iterate the matrix
  for (i in seq_len(niter))
    states[[i + 1]] <-  tf$matmul(mat, states[[i]])

  # return the final growth rate (should be same for all states at convergence)
  states[[niter + 1]]

}

# apply iterate_lambda to a series of n matrices of dimension m, stored as an n
# x m^2 matrix, each row being unpacked *rowwise*
tf_iterate_lambda_vectorised <- function (mat, state, n, m, niter) {

  # create indices for a block-diagonal sparse matrix
  # column wise offset to move the matrices along one horizontally
  offset <- rep(seq_len(n) - 1, each = m ^ 2) * m
  idx_cols <- rep(seq_len(m), m * n) + offset
  idx_rows <- rep(rep(seq_len(m), each = m), n) + offset

  # set up sparse tensor
  indices <- tf$constant(cbind(idx_rows, idx_cols) - 1,
                         dtype = tf$int64)
  values <- tf$reshape(mat,
                       shape = shape(n * m ^ 2))
  shape <- tf$constant(as.integer(rep(n * m, 2)),
                       dtype = tf$int64)
  full_mat <- tf$SparseTensor(indices = indices,
                              values = values,
                              dense_shape = shape)

  # replicate state for all patches
  state <- tf$tile(state, tf$constant(c(n, 1L), shape = shape(2)))

  # store states (can't overwrite since we need to maintain the chain of nodes)
  states <- list(state)

  # iterate the matrix
  for (i in seq_len(niter))
    states[[i + 1]] <-  tf$sparse_tensor_dense_matmul(full_mat, states[[i]])

  # indices to subset the first state for each patch
  idx_begin <- tf$constant(c(0L, 0L))
  idx_end <- tf$constant(c(m * n, 1L))
  idx_strides <- tf$constant(c(m, 1L))

  # subset these iterated states to get the first state per sub-matrix
  before <- tf$strided_slice(states[[niter]],
                             begin = idx_begin,
                             end = idx_end,
                             strides = idx_strides)

  after <- tf$strided_slice(states[[niter + 1]],
                            begin = idx_begin,
                            end = idx_end,
                            strides = idx_strides)

  # get growth rate
  lambdas <- after/before
  lambdas

}

# node functions exposed via module
iterate_lambda <- function(matrix, state, niter) {

  niter <- as.integer(niter)

  dimfun <- function(elem_list) {

    # input dimensions
    matrix_dim <- dim(elem_list[[1]])
    state_dim <- dim(elem_list[[2]])

    if (length(state_dim) != 2 | state_dim[2] != 1)
      stop ('state must be a column vector (rank 2 tensor)')

    if (length(matrix_dim) != 2 | matrix_dim[1] != matrix_dim[2])
      stop ('matrix must be a square matrix (rank 2 tensor)')

    if (matrix_dim[2] != state_dim[1])
      stop ('number of elements in state must match the dimension of matrix')

    # output dimensions
    c(1, 1)
  }

  op('tf_iterate_lambda',
     matrix,
     state,
     operation_args = list(niter = niter),
     dimfun = dimfun)

}

# node functions exposed via module
iterate_state <- function(matrix, state, niter) {

  niter <- as.integer(niter)

  dimfun <- function(elem_list) {

    # input dimensions
    matrix_dim <- dim(elem_list[[1]])
    state_dim <- dim(elem_list[[2]])

    if (length(state_dim) != 2 | state_dim[2] != 1)
      stop ('state must be a column vector (rank 2 tensor)')

    if (length(matrix_dim) != 2 | matrix_dim[1] != matrix_dim[2])
      stop ('matrix must be a square matrix (rank 2 tensor)')

    if (matrix_dim[2] != state_dim[1])
      stop ('number of elements in state must match the dimension of matrix')

    # output dimensions
    state_dim
  }

  op('tf_iterate_state',
     matrix,
     state,
     operation_args = list(niter = niter),
     dimfun = dimfun)

}

iterate_lambda_vectorised <- function(matrices, state, n, m, niter) {

  n <- as.integer(n)
  m <- as.integer(m)
  niter <- as.integer(niter)

  dimfun <- function(elem_list) {

    # input dimensions
    matrices_dim <- dim(elem_list[[1]])
    state_dim <- dim(elem_list[[2]])

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
#'   matrix a certain number of times and returns, as a scalar greta array, the
#'   terminal growth rate for the first element of the state vector.
#'   \code{iterate_state} carries out the same procedure, but returns the final
#'   state vector. \code{iterate_lambda_vectorised} is a vectorised version of
#'   \code{iterate_lambda} for iterating over multiple matrices, returning a
#'   vector of growth rates.
#'
#' @section Usage:
#' \preformatted{
#'   dynamics$iterate_lambda(matrix, state, niter)
#'   dynamics$iterate_state(matrix, state, niter)
#'   dynamics$iterate_lambda_vectorised(matrices, state, n, m, niter)
#' }
#'

NULL

#' @name iterate_lambda
#' @rdname dynamics-module
#' @param matrix a square, two-dimensional (i.e. matrix-like) greta array
#'   representing transition probabilities between states
#' @param state a column vector greta array representing the initial state from
#'   which to iterate the matrix
#' @param niter a positive integer giving the number of times to iterate the
#'   matrix
NULL

#' @name iterate_state
#' @rdname dynamics-module
NULL

#' @name iterate_lambda_vectorised
#' @rdname dynamics-module
#' @param matrices a rectangular two-dimensional greta array of dimension n x
#'   m^2, each row of which gives the rowwise elements of a different m x m
#'   matrix to iterate
#' @param n the number of m x m matrices to be iterated (first dimensions of
#'   \code{matrices})
#' @param m the dimension of each matrix to be iterated
NULL

#' @export
dynamics <- list(iterate_lambda = iterate_lambda,
                 iterate_state = iterate_state,
                 iterate_lambda_vectorised = iterate_lambda_vectorised)
