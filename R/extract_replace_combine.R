#' @name extract-replace-combine
#' @aliases extract replace cbind rbind c rep
#' @title Extract, Replace and Combine greta Arrays
#'
#' @description Generic methods to extract and replace elements of greta arrays, or to
#'   combine greta arrays.
#'
#' @section Usage: \preformatted{
#' # extract
#' x[i]
#' x[i, j, ..., drop = FALSE]
#'
#' # replace
#' x[i] <- value
#' x[i, j, ...] <- value
#'
#' # combine
#' cbind(...)
#' rbind(...)
#' c(..., recursive = FALSE)
#' rep(x, times, ..., recursive = FALSE)
#'
#' }
#'
#' @param i,j indices specifying elements to extract or replace
#' @param value a greta array to replace elements
#' @param ... either further indices specifying elements to extract or replace
#'   (\code{[}), or multiple greta arrays to combine (\code{cbind()}, \code{rbind()} &
#'   \code{c()}), or additional arguments (\code{rep()})
#' @param drop,recursive generic arguments that are ignored for greta arrays
#'
#' @examples
#' \dontrun{
#'  x = as_data(matrix(1:12, 3, 4))
#'
#'  # extract/replace
#'  x[1:3, ]
#'  x[, 2:4] <- 1:9
#'
#'  # combine
#'  cbind(x[, 2], x[, 1])
#'  rbind(x[1, ], x[3, ])
#'  c(x[, 1], x)
#'  rep(x[, 2], times = 3)
#' }
NULL

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


# extract syntax for greta_array objects
#' @export
`[.greta_array` <- function(x, ...) {

  # store the full call to mimic on a dummy array, plus the array's dimensions
  call <- sys.call()
  dims_in <- dim(x)

  # create a dummy array containing the order of elements Python-style
  dummy_in <- dummy(dims_in)

  # modify the call, switching to primitive subsetting, changing the target
  # object, and ensuring no dropping happens
  call_list <- as.list(call)[-1]
  call_list[[1]] <- as.name("._dummy_in")
  call_list$drop <- FALSE

  # put dummy_in in the parent environment & execute there (to evaluate any
  # promises using variables in that environment), then remove
  pf <- parent.frame()
  assign('._dummy_in', dummy_in, envir = pf)
  dummy_out <- do.call(.Primitive("["), call_list, envir = pf)
  rm('._dummy_in', envir = pf)

  # if this is a data node, also subset the values and pass on
  if (inherits(x$node, 'data_node')) {

    values_in <- x$node$value()
    call_list <- as.list(call)[-1]
    call_list[[1]] <- as.name("._values_in")
    call_list$drop <- FALSE

    # put values_in in the parent environment & execute there (to evaluate any
    # promises using variables in that environment), then remove
    pf <- parent.frame()
    assign('._values_in', values_in, envir = pf)
    values_out <- do.call(.Primitive("["), call_list, envir = pf)
    rm('._values_in', envir = pf)

    # make sure it's an array
    values <- as.array(values_out)

  } else {

    values <- NULL

  }

  # coerce result to an array
  dummy_out <- as.array(dummy_out)

  # get number of elements in input and dimension of output
  nelem <- prod(dims_in)
  dims_out <- dim(dummy_out)

  # make sure it's a column vector
  if (length(dims_out) == 1)
    dims_out <- c(dims_out, 1)

  # give values these dimensions
  if (is.array(values))
    values <- array(values, dim = dims_out)

  # get the index in flat python format, as a tensor
  index <- flatten_rowwise(dummy_out)

  # function to return dimensions of output
  dimfun <- function (elem_list)
    dims_out

  # create operation node, passing call and dims as additional arguments
  op('extract',
     x,
     dimfun = dimfun,
     operation_args = list(nelem = nelem,
                           index = index,
                           dims_out = dims_out),
     tf_operation = 'tf_extract',
     value = values)

}

# replace syntax for greta array objects
#' @export
`[<-.greta_array` <- function(x, ..., value) {

  if (inherits(x$node, 'variable_node')) {
    stop('cannot replace values in a variable greta array',
         call. = FALSE)
  }

  # rename value since it is confusing when passed to the op
  replacement <- as.greta_array(value)

  # store the full call to mimic on a dummy array, plus the array's dimensions
  call <- sys.call()
  dims <- dim(x)

  # create a dummy array containing the order of elements Python-style
  dummy <- dummy(dims)

  # modify the call, switching to primitive subsetting, changing the target
  # object, and ensuring no dropping happens
  call_list <- as.list(call)[-1]
  call_list[[1]] <- as.name("._dummy_in")
  call_list$value <- NULL

  # put dummy in the parent environment & execute there (to evaluate any
  # promises using variables in that environment), then remove
  pf <- parent.frame()
  assign('._dummy_in', dummy, envir = pf)
  dummy_out <- do.call(.Primitive("["), call_list, envir = pf)
  rm('._dummy_in', envir = pf)

  index <- as.vector(dummy_out)

  if (length(index) != length(replacement)) {

    if ((length(index) %% length(replacement)) != 0) {

      stop ('number of items to replace is not a multiple of ',
            'replacement length')

    } else {

      replacement <- rep(replacement, length.out = length(index))

    }
  }

  # function to return dimensions of output
  dimfun <- function (elem_list)
    dims

  # do replace on the values (unknowns or arrays)
  x_value <- x$node$value()
  replacement_value <- replacement$node$value()

  new_value <- x_value
  r_index <- match(index, dummy)
  new_value[r_index] <- replacement_value

  # if either parent has an unknowns array as a value, coerce this to unknowns
  if (inherits(x_value, 'unknowns') | inherits(replacement_value, 'unknowns'))
    new_value <- as.unknowns(new_value)

  # create operation node, passing call and dims as additional arguments
  op('replace',
     x,
     replacement,
     dimfun = dimfun,
     operation_args = list(index = index,
                           dims = dims),
     value = new_value,
     tf_operation = 'tf_replace')

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

#' @export
cbind.greta_array <- function (...) {

  dimfun <- function (elem_list) {

    dims <- lapply(elem_list, function(x) x$dim)
    ndims <- vapply(dims, length, FUN.VALUE = 1)
    if (!all(ndims == 2))
      stop ('all greta arrays must be two-dimensional')

    # dimensions
    rows <- vapply(dims, `[`, 1, FUN.VALUE = 1)
    cols <- vapply(dims, `[`, 2, FUN.VALUE = 1)

    # check all the same
    if (!all(rows == rows[1]))
      stop ('all greta arrays must be have the same number of rows',
            call. = FALSE)

    # output dimensions
    c(rows[1], sum(cols))

  }

  op('cbind', ..., dimfun = dimfun,
     tf_operation = 'tf_cbind')

}

#' @export
rbind.greta_array <- function (...) {

  dimfun <- function (elem_list) {

    dims <- lapply(elem_list, function(x) x$dim)
    ndims <- vapply(dims, length, FUN.VALUE = 1)
    if (!all(ndims == 2))
      stop ('all greta arrays must be two-dimensional')

    # dimensions
    rows <- vapply(dims, `[`, 1, FUN.VALUE = 1)
    cols <- vapply(dims, `[`, 2, FUN.VALUE = 1)

    # check all the same
    if (!all(cols == cols[1]))
      stop ('all greta arrays must be have the same number of columns',
            call. = FALSE)

    # output dimensions
    c(sum(rows), cols[1])

  }

  op('rbind', ..., dimfun = dimfun,
     tf_operation = 'tf_rbind')

}

#' @export
c.greta_array <- function (...) {

  # loop through arrays, flattening them R-style
  arrays <- lapply(list(...), flatten)

  # get output dimensions
  length_vec <- vapply(arrays, length, FUN.VALUE = 1)
  dim_out <- c(sum(length_vec), 1L)

  # get the output dimension
  dimfun <- function (elem_list)
    dim_out

  # create the op, expanding 'arrays' out to match op()'s dots input
  do.call(op,
          c(operation = 'rbind',
            arrays,
            dimfun = dimfun,
            tf_operation = 'tf_rbind'))

}

#' @export
rep.greta_array <- function (x, ...) {

  # get the index
  idx <- rep(seq_along(x), ...)

  # apply (implicitly coercing to a column vector)
  x[idx]

}
