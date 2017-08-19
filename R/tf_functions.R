# tensorflow functions

# convert Tensor to logical
tf_as_logical <- function (x)
  tf$cast(x, tf$bool)

# and to float
tf_as_float <- function (x)
  tf$cast(x, tf_float())

# and to integer
tf_as_integer <- function (x)
  tf$cast(x, tf$int32)

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

# tensorflow version of sweep, based on broadcasting of tf ops
tf_sweep <- function (x, STATS, MARGIN, FUN) {

  # if the second margin, transpose before and after
  if (MARGIN == 2)
    x <- tf$transpose(x)

  # apply the function rowwise
  result <- do.call(FUN, list(x, STATS))

  if (MARGIN == 2)
    result <- tf$transpose(result)

  result

}

# transpose and get the right matrix, like R
tf_chol <- function (x)
  tf$transpose(tf$cholesky(x))

tf_not <- function(x)
  tf_as_float(!tf_as_logical(x))

tf_and <- function(x, y)
  tf_as_float(tf_as_logical(x) & tf_as_logical(y))

tf_or <- function(x, y)
  tf_as_float(tf_as_logical(x) | tf_as_logical(y))

tf_lt <- function(x, y)
  tf_as_float(x < y)

tf_gt <- function(x, y)
  tf_as_float(x > y)

tf_lte <- function(x, y)
  tf_as_float(x <= y)

tf_gte <- function(x, y)
  tf_as_float(x >= y)

tf_eq <- function(x, y)
  tf_as_float(x == y)

tf_neq <- function(x, y)
  tf_as_float(x != y)

# inverse link functions in tensorflow
tf_iprobit <- function (x)
  (tf$erf(x / fl(sqrt(2))) + fl(1)) / fl(2)

tf_icloglog <- function (x)
  fl(1) - tf$exp(-tf$exp(x))

tf_icauchit <- function (x)
  fl(1 / pi) * tf$atan(x) + fl(0.5)


# map R's extract and replace syntax to tensorflow, for use in operation nodes
# the following arguments are required:
#   nelem - number of elements in the original array,
#   tf_index - rank 1 tensor giving index to subsetted elements in flattened
#     input tensor
#   dims_out - dimension of output array
tf_extract <- function (x, nelem, index, dims_out) {

  # flatten tensor, gather using index, reshape to output dimension
  tensor_in_flat <- tf$reshape(x, shape(nelem))
  tf_index <- tf$constant(as.integer(index), dtype = tf$int32)
  tensor_out_flat <- tf$gather(tensor_in_flat, tf_index)
  tensor_out <- tf$reshape(tensor_out_flat, to_shape(dims_out))
  tensor_out

}

# using tf$concat, update the elements of a tensor `ref`, putting the new
# values, a tensor `updates` at the elements given by the R vector `index` (in
# 0-indexing)
tf_recombine <- function (ref, index, updates) {

  # vector denoting whether an element is being updated
  nelem <- ref$get_shape()$as_list()[1]
  replaced <- rep(0, nelem)
  replaced[index + 1] <- seq_along(index)
  runs <- rle(replaced)

  # number of blocks to concatenate
  nblock <- length(runs$lengths)

  # start location (R-style) for each block in the original object
  starts_old <- cumsum(c(0, runs$lengths[-nblock])) + 1

  # list of non-updated values
  keep_idx <- which(runs$values == 0)
  keep_list <- lapply(keep_idx, function (i) {
    idx <- starts_old[i] + 0:(runs$lengths[i] - 1) - 1
    tf$reshape(ref[idx], shape(length(idx), 1))
  })

  run_id <- runs$values[runs$values != 0]
  update_idx <- match(run_id, runs$values)
  # get them in order increasing order
  update_list <- lapply(run_id, function (i) {
    tf$reshape(updates[i - 1], shape(1, 1))
  })

  # combine them
  full_list <- list()
  full_list[keep_idx] <- keep_list
  full_list[update_idx] <- update_list

  # concatenate the vectors
  result <- tf$concat(full_list, 0L)

  # rotate it
  result <- tf$reshape(result, shape(result$get_shape()$as_list()[1]))
  result

}

# replace elements in a tensor with another tensor
tf_replace <- function (x, replacement, index, dims) {

  # flatten original tensor and new values
  nelem <- prod(dims)
  x_flat <- tf$reshape(x, shape(nelem))
  replacement_flat <- tf$reshape(replacement, shape(length(index)))

  # update the values into a new tensor
  result_flat <- tf_recombine(ref = x_flat,
                              index = index,
                              updates = replacement_flat)

  # reshape the result
  result <- tf$reshape(result_flat, to_shape(dims))
  result

}

# mapping of cbind and rbind to tf$concat
tf_cbind <- function (...) {
  elem_list <- list(...)
  tf$concat(elem_list, 1L)
}

tf_rbind <- function (...) {
  elem_list <- list(...)
  tf$concat(elem_list, 0L)
}

# combine as module for export via internals
tf_functions_module <- module(tf_as_logical,
                              tf_as_float,
                              tf_as_integer,
                              tf_lchoose,
                              tf_lbeta,
                              tf_chol,
                              tf_flat_to_chol,
                              tf_flat_to_chol_correl,
                              tf_chol_to_symmetric,
                              tf_sweep,
                              tf_not,
                              tf_and,
                              tf_or,
                              tf_lt,
                              tf_gt,
                              tf_lte,
                              tf_gte,
                              tf_eq,
                              tf_neq,
                              tf_iprobit,
                              tf_icloglog,
                              tf_icauchit,
                              tf_extract,
                              tf_recombine,
                              tf_replace,
                              tf_cbind,
                              tf_rbind)
