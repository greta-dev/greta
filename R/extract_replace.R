# map R's extract and replace syntax to tensorflow, for use in operation nodes
# the following arguments are required:
#   nelem - number of elements in the original array,
#   tf_index - rank 1 tensor giving index to subsetted elements in flattened
#     input tensor
#   dims_out - dimension of output array
tf_extract <- function (x, nelem, tf_index, dims_out) {

  # flatten tensor, gather using index, reshape to output dimension
  tensor_in_flat <- tf$reshape(x, shape(nelem))
  tensor_out_flat <- tf$gather(tensor_in_flat, tf_index)
  tensor_out <- tf$reshape(tensor_out_flat, to_shape(dims_out))
  tensor_out

}

# using tf$concat, update the elements of a tensor `ref`, putting the new
# values, a tensor `updates` at the elements given by the R vector `index` (in
# 0-indexing)
recombine <- function (ref, index, updates) {

  # vector denoting whether an element is being updated
  nelem <- ref$get_shape()$as_list()
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
  result <- tf$concat(tf$constant(0L),
                      full_list)

  # rotate it
  result <- tf$reshape(result, shape(result$get_shape()$as_list()[1]))
  result

}

# replace elements in a tensor with another tensor
tf_replace <- function (x, value, index, dims) {

  # flatten original tensor and new values
  nelem <- prod(dims)
  x_flat <- tf$reshape(x, shape(nelem))
  value_flat <- tf$reshape(value, shape(length(index)))

  # update the values into a new tensor
  result_flat <- recombine(ref = x_flat,
                          index = index,
                          updates = value_flat)

  # reshape the result
  result <- tf$reshape(result_flat, to_shape(dims))
  result

}


# extract syntax for nodes
#' @export
`[.node` <- function(x, ...) {

  # store the full call to mimic on a dummy array, plus the array's dimensions
  call <- sys.call()
  dims_in <- x$dim

  # create a dummy array containing the order of elements Python-style
  dummy_in <- dummy(dims_in)

  # subset the dummy array using the original subsetting call
  call[[1]] <- `[`
  call[[2]] <- dummy_in
  dummy_out <- array(eval(call))

  # get number of elements in input and dimension of output
  nelem <- prod(dims_in)
  dims_out <- dim(dummy_out)

  # get the index in flat python format, as a tensor
  index <- flatten_rowwise(dummy_out)
  tf_index <- tf$constant(as.integer(index),
                          dtype = tf$int32)


  # function to return dimensions of output
  dimfun <- function (node_list)
    dims_out

  # create operation node, passing call and dims as additional arguments
  op('tf_extract',
     x,
     dimfun = dimfun,
     operation_args = list(nelem = nelem,
                           tf_index = tf_index,
                           dims_out = dims_out))

}


# extract syntax for nodes
#' @export
`[<-.node` <- function(x, ..., value) {

  if (inherits(x, 'stochastic_node'))
    stop('cannot replace values in a stochastic node')

  value <- to_node(value)

  # store the full call to mimic on a dummy array, plus the array's dimensions
  call <- sys.call()
  dims <- x$dim

  # create a dummy array containing the order of elements Python-style
  dummy <- dummy(dims)

  # subset the dummy array using the original subsetting call
  call[[1]] <- `[`
  call[[2]] <- dummy
  call$value <- NULL

  index <- as.vector(eval(call))

  if (length(index) != prod(value$dim))
    stop('number of items to replace does not match number of items to insert')

  # function to return dimensions of output
  dimfun <- function (node_list)
    dims

  # create operation node, passing call and dims as additional arguments
  op('tf_replace',
     x,
     value,
     dimfun = dimfun,
     operation_args = list(index = index,
                           dims = dims))

}
