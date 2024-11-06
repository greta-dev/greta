# tensorflow functions

tf_log10 <- function(x) {
  tf$math$log(x) / fl(log(10))
}

tf_log2 <- function(x) {
  tf$math$log(x) / fl(log(2))
}

# TF doesn't have the clever efficient & stable versions of these, so do them
# the slow way
tf_cospi <- function(x) {
  tf$math$cos(x * fl(pi))
}

tf_sinpi <- function(x) {
  tf$math$sin(x * fl(pi))
}

tf_tanpi <- function(x) {
  tf$math$tan(x * fl(pi))
}

tf_gamma_fun <- function(x) {
  log_gamma <- tf$math$lgamma(x)
  tf$math$exp(log_gamma)
}

tf_trigamma <- function(x) {
  tf$math$polygamma(fl(1), x)
}

# convert Tensor to logical
tf_as_logical <- function(x) {
  tf$cast(x, tf$bool)
}

# and to float
tf_as_float <- function(x) {
  tf$cast(x, tf_float())
}

# and to integer
tf_as_integer <- function(x) {
  tf$cast(x, tf$int32)
}

tf_lchoose <- function(n, k) {
  one <- fl(1)
  -tf$math$lgamma(one + n - k) -
    tf$math$lgamma(one + k) +
    tf$math$lgamma(one + n)
}

tf_lbeta <- function(a, b) {
  tf$math$lgamma(a) + tf$math$lgamma(b) - tf$math$lgamma(a + b)
}

# set up the tf$reduce_* functions to ignore the first dimension
skip_dim <- function(op_name, x, drop = FALSE) {
  ndim <- n_dim(x)
  reduction_dims <- seq_len(ndim - 1)
  tf[[op_name]](x, axis = reduction_dims, keepdims = !drop)
}

tf_sum <- function(x, drop = FALSE) {
  skip_dim("reduce_sum", x, drop)
}

tf_prod <- function(x, drop = FALSE) {
  skip_dim("reduce_prod", x, drop)
}

tf_min <- function(x, drop = FALSE) {
  skip_dim("reduce_min", x, drop)
}

tf_mean <- function(x, drop = FALSE) {
  skip_dim("reduce_mean", x, drop)
}

tf_max <- function(x, drop = FALSE) {
  skip_dim("reduce_max", x, drop)
}

tf_cumsum <- function(x) {
  tf$math$cumsum(x, axis = 1L)
}

tf_cumprod <- function(x) {
  tf$math$cumprod(x, axis = 1L)
}

# set the dimensions of a tensor, reshaping in the same way (column-major) as R
tf_set_dim <- function(x, dims) {

  # transpose to do work in row-major order
  perm_old <- c(0L, rev(seq_along(dim(x)[-1])))
  x <- tf$transpose(x, perm_old)

  # reshape (with transposed shape) as row-major
  x <- tf$reshape(x, shape = c(-1L, rev(dims)))

  # transpose back
  perm_new <- c(0L, rev(seq_along(dims)))
  x <- tf$transpose(x, perm_new)
  x
}

# expand the dimensions of a scalar tensor, reshaping in the same way
# (column-major) as R
tf_expand_dim <- function(x, dims) {

  # prepend a batch dimension to dims (a 1 so we can tile with it)
  dims <- c(1L, dims)

  # pad/shrink x to have the correct number of dimensions
  x_dims <- n_dim(x)
  target_dims <- length(dims)

  # add extra dimensions at the end
  if (target_dims > x_dims) {
    extra_dims <- target_dims - x_dims
    for (i in seq_len(extra_dims)) {
      x <- tf$expand_dims(x, -1L)
    }
  }

  # tile x to match target dimensions
  tf$tile(x, dims)
}

# skip the first index when transposing
tf_transpose <- function(x) {
  nelem <- n_dim(x)
  perm <- c(0L, (nelem - 1):1)
  tf$transpose(x, perm = perm)
}

tf_apply <- function(x, axis, tf_fun_name) {
  fun <- tf$math[[tf_fun_name]]
  out <- fun(x, axis = axis)
  # if we reduced we lost a dimension, make sure we have enough
  if (n_dim(out) < 3) {
    out <- tf$expand_dims(out, 2L)
  }
  out
}

# permute the tensor to get the non-batch dim first, do the relevant
# "unsorted_segment_*" op, then permute it back
tf_tapply <- function(x, segment_ids, num_segments, op_name) {
  op_name <- glue::glue("unsorted_segment_{op_name}")

  x <- tf$transpose(x, perm = c(1:2, 0L))
  x <- tf$math[[op_name]](x,
    segment_ids = segment_ids,
    num_segments = num_segments)
  x <- tf$transpose(x, perm = c(2L, 0:1))
  x
}

# given a (batched, column) vector tensor of elements, corresponding to the
# correlation-constrained (between -1 and 1) free state of a single row of the
# cholesky factor of a correlation matrix, return the (upper-triangular
# elements, including the diagonal, of the) row of the choleskied correlation
# matrix. Or return the log jacobian of this transformation. When which =
# "values", the output vector has one more element than the input, since the
# diagonal element depends deterministically on the other elements.
tf_corrmat_row <- function(z, which = c("values", "ljac")) {
  which <- match.arg(which)

  n <- dim(z)[[2]]

  # use a tensorflow while loop to do the recursion:
  body_values <- function(z, x, sumsq, lp, iter, maxiter) {
    x_new <- z[, iter] * tf$sqrt(fl(1) - sumsq)
    sumsq <- sumsq + tf$square(x_new)
    x_new <- tf$expand_dims(x_new, 1L)
    x <- tf$concat(list(x, x_new), axis = 1L)
    list(z, x, sumsq, lp, iter + 1L, maxiter)
  }

  # body for the log jacobian adjustment
  body_ljac <- function(z, x, sumsq, lp, iter, maxiter) {
    lp <- lp + fl(0.5) * log(fl(1) - sumsq)
    x_new <- z[, iter] * tf$sqrt(fl(1) - sumsq)
    sumsq <- sumsq + tf$square(x_new)
    list(z, x, sumsq, lp, iter + 1L, maxiter)
  }

  # initial sum of squares is from the first element
  z_0 <- z[, 0]
  sumsq <- z_0^2
  x <- tf$expand_dims(z_0, 1L)
  lp <- tf$zeros(shape(1), tf_float())
  lp <- expand_to_batch(lp, z)

  # x has no elements yet, append them
  values <- list(
    z,
    x,
    sumsq,
    lp,
    tf$constant(1L),
    tf$constant(n)
  )

  cond <- function(z, x, sumsq, lp, iter, maxiter) {
    tf$less(iter, maxiter)
  }

  # nolint start
  shapes <- list(
    tf$TensorShape(shape(NULL, n)),
    tf$TensorShape(shape(NULL, NULL)),
    tf$TensorShape(shape(NULL)),
    tf$TensorShape(shape(NULL)),
    tf$TensorShape(shape()),
    tf$TensorShape(shape())
  )
  # nolint end

  body <- switch(which,
    values = body_values,
    ljac = body_ljac
  )

  out <- tf$while_loop(cond,
    body,
    values,
    shape_invariants = shapes
  )

  if (which == "values") {
    x <- out[[2]]
    sumsq <- out[[3]]
    final_x <- tf$sqrt(fl(1) - sumsq)
    final_x <- tf$expand_dims(final_x, 1L)
    x <- tf$concat(list(x, final_x), axis = 1L)
    return(x)
  } else {
    lp <- out[[4]]
    return(lp)
  }
}

tf_chol2symm <- function(x) {
  tf$matmul(x, x, adjoint_a = TRUE)
}

tf_colmeans <- function(x, dims) {
  idx <- rowcol_idx(x, dims, "col")
  y <- tf$reduce_mean(x, axis = idx)

  if (is_2d(y)) {
    dims_out <- c(-1L, unlist(dim(y)[-1]), 1L)
    y <- tf$reshape(y, dims_out)
  }

  y
}

tf_rowmeans <- function(x, dims) {
  idx <- rowcol_idx(x, dims, "row")
  idx <- idx[-length(idx)]
  y <- tf$reduce_mean(x, axis = idx)

  if (is_2d(y)) {
    dims_out <- c(-1L, unlist(dim(y)[-1]), 1L)
    y <- tf$reshape(y, dims_out)
  }

  y
}

tf_colsums <- function(x, dims) {
  idx <- rowcol_idx(x, dims, "col")
  y <- tf$reduce_sum(x, axis = idx)

  if (is_2d(y)) {
    dims_out <- c(-1L, unlist(dim(y)[-1]), 1L)
    y <- tf$reshape(y, dims_out)
  }

  y
}

tf_rowsums <- function(x, dims) {
  idx <- rowcol_idx(x, dims, "row")
  idx <- idx[-length(idx)]
  y <- tf$reduce_sum(x, axis = idx)

  if (is_2d(y)) {
    dims_out <- c(-1L, unlist(dim(y)[-1]), 1L)
    y <- tf$reshape(y, dims_out)
  }

  y
}

# calculate kronecker product of two matrices
tf_kronecker <- function(x, y, tf_fun_name) {
  tf_function <- tf[[tf_fun_name]]

  dims <- unlist(c(dim(x)[-1], dim(y)[-1]))

  # expand dimensions of tensors to allow direct multiplication for kronecker
  # prod
  x_rsh <- tf$reshape(x, tensorflow::as_tensor(shape(-1, dims[1], 1L, dims[2], 1L)))
  y_rsh <- tf$reshape(y, tensorflow::as_tensor(shape(-1, 1L, dims[3], 1L, dims[4])))

  # multiply tensors and reshape with appropriate dimensions
  z <- tf_function(x_rsh, y_rsh)
  shape_out <- shape(-1, dims[1] * dims[3], dims[2] * dims[4])
  tensor_out <- tf$reshape(z, tensorflow::as_tensor(shape_out))

  tensor_out
}

# tensorflow version of sweep, based on broadcasting of tf ops
tf_sweep <- function(x, stats, margin, fun) {

  # if the second margin, transpose before and after
  if (margin == 2) {
    x <- tf_transpose(x)
  }

  # apply the function rowwise
  result <- do.call(fun, list(x, stats))

  if (margin == 2) {
    result <- tf_transpose(result)
  }

  result
}

# transpose and get the right matrix, like R
tf_chol <- function(x) {
  x_chol <- tf$linalg$cholesky(x)
  x_chol_t <- tf_transpose(x_chol)
  x_chol_t
}

tf_chol2inv <- function(u) {
  n <- dim(u)[[2]]
  eye <- fl(add_first_dim(diag(n)))
  eye <- expand_to_batch(eye, u)
  l <- tf$linalg$matrix_transpose(u)
  tf$linalg$cholesky_solve(l, eye)
}

tf_cov2cor <- function(v) {
  # sweep out variances
  diag <- tf$linalg$diag_part(v)
  diag <- tf$expand_dims(diag, 2L)
  eyes <- tf$sqrt(fl(1) / diag)
  v <- eyes * v
  v <- tf_transpose(v)
  v <- v * eyes
  # set diagonals to 1
  n <- dim(v)[[2]]
  new_diag <- fl(t(rep(1, n)))
  new_diag <- expand_to_batch(new_diag, v)
  tf$linalg$set_diag(v, new_diag)
}

tf_not <- function(x) {
  tf_as_float(!tf_as_logical(x))
}

tf_and <- function(x, y) {
  tf_as_float(tf_as_logical(x) & tf_as_logical(y))
}

tf_or <- function(x, y) {
  tf_as_float(tf_as_logical(x) | tf_as_logical(y))
}

tf_lt <- function(x, y) {
  tf_as_float(x < y)
}

tf_gt <- function(x, y) {
  tf_as_float(x > y)
}

tf_lte <- function(x, y) {
  tf_as_float(x <= y)
}

tf_gte <- function(x, y) {
  tf_as_float(x >= y)
}

tf_eq <- function(x, y) {
  tf_as_float(x == y)
}

tf_neq <- function(x, y) {
  tf_as_float(x != y)
}

# inverse link functions in tensorflow
tf_iprobit <- function(x) {
  (tf$math$erf(x / fl(sqrt(2))) + fl(1)) / fl(2)
}

tf_icloglog <- function(x) {
  fl(1) - tf$exp(-tf$exp(x))
}

tf_icauchit <- function(x) {
  fl(1 / pi) * tf$atan(x) + fl(0.5)
}

tf_imultilogit <- function(x) {
  batch_size <- tf$shape(x)[[0]]
  shape <- c(batch_size, dim(x)[[2]], 1L)
  zeros <- tf$zeros(shape, tf_float())
  latent <- tf$concat(list(x, zeros), 2L)
  tf$nn$softmax(latent)
}

# map R's extract and replace syntax to tensorflow, for use in operation nodes
# the following arguments are required:
#   nelem - number of elements in the original array,
#   tf_index - rank 1 tensor giving index to subsetted elements in flattened
#     input tensor
#   dims_out - dimension of output array
tf_extract <- function(x, nelem, index, dims_out) {

  # flatten tensor, gather using index, reshape to output dimension
  tensor_in_flat <- tf$reshape(x, tensorflow::as_tensor(shape(-1, nelem)))
  tf_index <- tf$constant(as.integer(index), dtype = tf$int32)
  tensor_out_flat <- tf$gather(tensor_in_flat, tf_index, axis = 1L)

  # reshape, handling unknown dimensions even when the output has 0-dimension
  # (which prevents us from just using -1 on the first dimension)
  batch_size <- tf$shape(x)[[0]]
  shape_list <- c(list(batch_size), as.integer(to_shape(dims_out)))
  shape_out <- tf$stack(shape_list)
  tensor_out <- tf$reshape(tensor_out_flat, shape_out)
  tensor_out
}

# using tf$concat, update the elements of a tensor `ref`, putting the new
# values, a tensor `updates` at the elements given by the R vector `index` (in
# 0-indexing)
tf_recombine <- function(ref, index, updates) {

  # vector denoting whether an element is being updated
  nelem <- dim(ref)[[2]]
  replaced <- rep(0, nelem)
  replaced[index + 1] <- seq_along(index)
  runs <- rle(replaced)

  # number of blocks to concatenate
  nblock <- length(runs$lengths)

  # start location (R-style) for each block in the original object
  starts_old <- cumsum(c(0, runs$lengths[-nblock])) + 1

  # list of non-updated values
  keep_idx <- which(runs$values == 0)
  keep_list <- lapply(keep_idx, function(i) {
    idx <- starts_old[i] + 0:(runs$lengths[i] - 1) - 1
    tf$reshape(ref[, idx, ], tensorflow::as_tensor(shape(-1, length(idx), 1)))
  })

  run_id <- runs$values[runs$values != 0]
  update_idx <- match(run_id, runs$values)
  # get them in  increasing order
  update_list <- lapply(run_id, function(i) {
    tf$reshape(updates[, i - 1, ], tensorflow::as_tensor(shape(-1, 1, 1)))
  })

  # combine them
  full_list <- list()
  full_list[keep_idx] <- keep_list
  full_list[update_idx] <- update_list

  # concatenate the vectors
  result <- tf$concat(full_list, 1L)

  result
}

# flatten a tensor x, ignoring the first (batch) dimension, and optionally
# adding a trailing 1 to make it an explicit column vector
tf_flatten <- function(x, extra_ones = 0) {
  nelem <- prod(unlist(dim(x)[-1]))
  out_dim <- c(-1, nelem, rep(1, extra_ones))
  tf$reshape(x, tensorflow::as_tensor(to_shape(out_dim)))
}

# replace elements in a tensor with another tensor
tf_replace <- function(x, replacement, index, dims) {

  # flatten original tensor and new values
  x_flat <- tf_flatten(x, 1)
  replacement_flat <- tf_flatten(replacement, 1)

  # update the values into a new tensor
  result_flat <- tf_recombine(
    ref = x_flat,
    index = index,
    updates = replacement_flat
  )

  # reshape the result
  result <- tf$reshape(
    result_flat,
    tensorflow::as_tensor(to_shape(c(-1, dims)))
  )
  result
}

# mapping of cbind and rbind to tf$concat
tf_cbind <- function(...) {
  elem_list <- list(...)
  tf$concat(elem_list, axis = 2L)
}

tf_rbind <- function(...) {
  elem_list <- list(...)
  tf$concat(elem_list, axis = 1L)
}

tf_abind <- function(..., axis) {
  elem_list <- list(...)
  tf$concat(elem_list, axis = axis)
}

tf_only_eigenvalues <- function(x) {
  vals <- tf$linalg$eigvalsh(x)
  dim <- tf$constant(1L, shape = list(1L))
  tf$reverse(vals, dim)
}

tf_extract_eigenvectors <- function(x) {
  vecs <- x[[2]]
  dim <- tf$constant(2L, shape = list(1L))
  tf$reverse(vecs, dim)
}

tf_extract_eigenvalues <- function(x) {
  vals <- x[[1]]
  dim <- tf$constant(1L, shape = list(1L))
  tf$reverse(vals, dim)
}

tf_self_distance <- function(x1) {
  tf_distance(x1, x1)
}

tf_distance <- function(x1, x2) {
  n1 <- dim(x1)[[2]]
  n2 <- dim(x2)[[2]]

  x1 <- tf$tile(tf$expand_dims(x1, 3L), list(1L, 1L, 1L, n2))
  x2 <- tf$transpose(x2, perm = c(0L, 2L, 1L))
  x2 <- tf$tile(tf$expand_dims(x2, 1L), list(1L, n1, 1L, 1L))

  dists <- (x1 - x2)^2
  dist <- tf$reduce_sum(dists, axis = 2L)
  dist <- tf$sqrt(dist)

  dist
}

# common construction of a chained bijector for scalars, optionally adding a
# final reshaping step
tf_scalar_biject <- function(..., dim) {
  steps <- list(...)

  if (!is.null(dim)) {
    steps <- c(tfp$bijectors$Reshape(dim), steps)
  }

  tfp$bijectors$Chain(steps)
}

tf_scalar_bijector <- function(dim, lower, upper) {
  tf_scalar_biject(
    tfp$bijectors$Identity(),
    dim = dim
  )
}

tfb_shift_scale <- function(shift, scale){
  tfb_shift <- tfp$bijectors$Shift(shift)
  tfb_scale <- tfp$bijectors$Scale(scale)
  tfb_shift_scale <- tfb_shift(tfb_scale)
  tfb_shift_scale
}

tf_scalar_pos_bijector <- function(dim, lower, upper) {
  tf_scalar_biject(
    tfp$bijectors$Shift(fl(lower)),
    tfp$bijectors$Exp(),
    dim = dim
  )
}

tf_scalar_neg_bijector <- function(dim, lower, upper) {
  tf_scalar_biject(
    tfb_shift_scale(fl(upper), fl(-1)),
    tfp$bijectors$Exp(),
    dim = dim
  )
}

tf_scalar_neg_pos_bijector <- function(dim, lower, upper) {
  tf_scalar_biject(
    tfb_shift_scale(shift = fl(lower), scale = fl(upper - lower)),
    tfp$bijectors$Sigmoid(),
    dim = dim
  )
}

# a blockwise combination of other transformations, with final reshaping
tf_scalar_mixed_bijector <- function(dim, lower, upper, constraints) {
  constructors <-
    list(
      none = tf_scalar_bijector,
      low = tf_scalar_neg_bijector,
      high = tf_scalar_pos_bijector,
      both = tf_scalar_neg_pos_bijector
    )

  # get the constructors, lower and upper bounds for each block
  rle <- rle(constraints)
  blocks <- rep(seq_along(rle$lengths), rle$lengths)
  constructor_idx <- match(rle$values, names(constructors))
  block_constructors <- constructors[constructor_idx]
  lowers <- split(lower, blocks)
  uppers <- split(upper, blocks)

  # combine into lists of arguments
  n_blocks <- length(rle$lengths)
  dims <- replicate(n_blocks, NULL, simplify = FALSE)
  block_parameters <- mapply(list, dims, lowers, uppers, SIMPLIFY = FALSE)
  block_parameters <- lapply(
    block_parameters,
    `names<-`,
    c("dim", "lower", "upper")
  )

  # create bijectors for each block
  names(block_constructors) <- NULL
  bijectors <- mapply(do.call,
    block_constructors,
    block_parameters,
    SIMPLIFY = FALSE
  )

  # roll into single bijector
  tf_scalar_biject(
    tfp$bijectors$Blockwise(bijectors, block_sizes = rle$lengths),
    dim = dim
  )
}

tf_correlation_cholesky_bijector <- function() {
  steps <- list(
    tfp$bijectors$Transpose(perm = 1:0),
    tfp$bijectors$CorrelationCholesky()
  )

  tfp$bijectors$Chain(steps)

}

tf_covariance_cholesky_bijector <- function() {

  steps <- list(
    tfp$bijectors$Transpose(perm = 1:0),
    tfp$bijectors$FillScaleTriL(diag_shift = fl(1e-5))
  )

  tfp$bijectors$Chain(steps)
}


tf_simplex_bijector <- function(dim) {
  n_dim <- length(dim)
  last_dim <- dim[n_dim]
  raw_dim <- dim
  raw_dim[n_dim] <- last_dim - 1L

  steps <- list(
    tfp$bijectors$IteratedSigmoidCentered(),
    tfp$bijectors$Reshape(raw_dim)
  )
  tfp$bijectors$Chain(steps)
}

tf_ordered_bijector <- function(dim) {
  steps <- list(
    # tfp$bijectors$Invert(tfp$bijectors$Ordered()),
    tfp$bijectors$Ascending(),
    tfp$bijectors$Reshape(dim)
  )
  tfp$bijectors$Chain(steps)
}

# generate a tensor of random standard uniforms with a given shape,
# including the batch dimension
tf_randu <- function(dim, dag) {
  uniform <- tfp$distributions$Uniform(low = fl(0), high = fl(1))
  shape <- c(dag$tf_environment$batch_size, as.list(dim))
  uniform$sample(sample_shape = shape, seed = get_seed())
}

# generate an integer tensor of values up to n (indexed from 0) with a given
# shape, including the batch dimension
tf_randint <- function(n, dim, dag) {
  u <- tf_randu(dim, dag)
  tf$floor(u * as.integer(n))
}

# combine as module for export via internals
tf_functions_module <- module(
  tf_as_logical,
  tf_as_float,
  tf_as_integer,
  tf_lchoose,
  tf_lbeta,
  tf_chol,
  tf_chol2inv,
  tf_corrmat_row,
  tf_chol2symm,
  tf_colmeans,
  tf_rowmeans,
  tf_colsums,
  tf_rowsums,
  tf_kronecker,
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
  tf_imultilogit,
  tf_extract,
  tf_recombine,
  tf_flatten,
  tf_replace,
  tf_cbind,
  tf_rbind,
  tf_only_eigenvalues,
  tf_extract_eigenvectors,
  tf_extract_eigenvalues,
  tf_self_distance,
  tf_distance,
  tf_scalar_bijector,
  tf_scalar_neg_bijector,
  tf_scalar_pos_bijector,
  tf_scalar_neg_pos_bijector,
  tf_correlation_cholesky_bijector,
  tf_covariance_cholesky_bijector
)
