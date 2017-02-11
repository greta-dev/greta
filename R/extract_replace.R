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
