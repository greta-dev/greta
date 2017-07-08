# tensorflow functions

# convert Tensor to logical
tf_as_logical <- function (x)
  tf$cast(x, tf$bool)

# and to float
tf_as_float <- function (x)
  tf$cast(x, tf_float())

# and to integer
tf_as_integer <- function (x)
  tf$cast(x, tf_int())

tf_lchoose <- function (n, k) {
  one <- fl(1)
  -tf$lgamma(one + n - k) - tf$lgamma(one + k) + tf$lgamma(one + n)
}

tf_lbeta <- function (a, b)
  tf$lgamma(a) + tf$lgamma(b) - tf$lgamma(a + b)

# given a flat tensor, convert it into a square symmetric matrix by considering
# it  as the non-zero elements of the lower-triangular decomposition of the
# square matrix
tf_flat_to_chol = function (x, dims) {

  # indices to the cholesky factor
  L_dummy <- dummy(dims)
  indices_diag <- diag(L_dummy)
  indices_offdiag <- sort(L_dummy[upper.tri(L_dummy, diag = FALSE)])

  # indices to the free state
  x_index_diag <- seq_along(indices_diag) - 1
  x_index_offdiag <- length(indices_diag) + seq_along(indices_offdiag) - 1

  # create an empty vector to fill with the values
  values_0 <- tf$zeros(shape(prod(dims), 1), dtype = tf_float())
  values_0_diag <- tf_recombine(values_0, indices_diag, tf$exp(x[x_index_diag]))
  values_z <- tf_recombine(values_0_diag, indices_offdiag, x[x_index_offdiag])

  # reshape into lower triangular and return
  tf$reshape(values_z, shape(dims[1], dims[2]))

}

# convert an unconstrained vector into symmetric correlation matrix
tf_flat_to_chol_correl = function (x, dims) {

  # to -1, 1 scale
  y <- tf$tanh(x)

  k <- dims[1]

  # list of indices mapping relevant part of each row to an element of y
  y_index_list <- list()
  count <- 0
  for (i in 1:(k - 1)) {
    nelem <- k - i
    y_index_list[[i]] <- count + seq_len(nelem) - 1
    count <- count + nelem
  }

  # dummy list to store transformed versions of rows
  values_list <- y_index_list
  values_list[[1]] <- tf$reshape(y[y_index_list[[1]]], shape(k - 1))
  sum_sqs <- tf$square(values_list[[1]])

  if (k > 2) {

    for (i in 2:(k - 1)) {
      # relevant columns (0-indexed)
      idx <- i:(k - 1) - 1
      # components of z on this row (straight from y)
      z <- tf$reshape(y[y_index_list[[i]]], shape(k - i))
      # assign to w, using relevant parts of the sum of squares
      values_list[[i]] <- z * tf$sqrt(fl(1) - sum_sqs[idx])
      # increment sum of squares
      sum_sqs_part <- tf$square(values_list[[i]]) + sum_sqs[idx]
      sum_sqs <- tf_recombine(sum_sqs, idx, sum_sqs_part)
    }

  }

  # dummy array to find the indices
  L_dummy <- dummy(dims)
  indices_diag <- diag(L_dummy)
  indices_offdiag <- sort(L_dummy[upper.tri(L_dummy, diag = FALSE)])

  # diagonal & off-diagonal elements
  values_diag <- tf$concat(list(tf$ones(1L, dtype = tf_float()),
                                sqrt(fl(1) - sum_sqs)), 0L)
  values_offdiag <- tf$concat(values_list, 0L)

  # plug elements into a vector of 0s
  values_0 <- tf$zeros(shape(prod(dims), 1), dtype = tf_float())
  values_0_diag <- tf_recombine(values_0, indices_diag, values_diag)
  values_z <- tf_recombine(values_0_diag, indices_offdiag, values_offdiag)

  # reshape into cholesky and return
  tf$reshape(values_z, shape(dims[1], dims[2]))

}

tf_chol_to_symmetric <- function (U)
  tf$matmul(tf$transpose(U), U)

# combine as module for export via internals
tf_functions <- as_module(tf_as_logical,
                          tf_as_float,
                          tf_as_integer,
                          tf_lchoose,
                          tf_lbeta,
                          tf_flat_to_chol,
                          tf_flat_to_chol_correl,
                          tf_chol_to_symmetric)
