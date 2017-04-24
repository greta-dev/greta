# utility functions

# helper for *apply statements on R6 objects
member <- function(x, method)
  eval(parse(text = paste0('x$', method)))

# coerce an integer(ish) vector to a list as expected in tensorflow shape arguments
to_shape <- function(dim)
  do.call(shape, as.list(dim))

# run code ins specified environment, e.g.
# in_env(nm <- dag$child_names(), env)
in_env <- function (call, env)
  eval(substitute(call), envir = env)

# placeholder error
notimplemented <- function ()
  stop ('method not yet implemented')

# is this greta_array actually a scalar?
is_scalar <- function (x)
  identical(dim(x), c(1L, 1L))

# check dimensions of arguments to ops, and return the maximum dimension
check_dims <- function (..., target_dim = NULL) {

  # coerce args to greta arrays
  elem_list <- list(...)
  elem_list <- lapply(elem_list, as.greta_array)

  # dimensions of each
  dim_list <- lapply(elem_list, dim)

  # as text, for printing
  dims_paste <- vapply(dim_list, paste, '', collapse= 'x')
  dims_text <- paste(dims_paste, collapse = ', ')

  # which are scalars
  scalars <- vapply(elem_list, is_scalar, FALSE)

  # if more than one is non-scalar, need to check them
  if (sum(!scalars) > 1) {

    match_first <- vapply(dim_list,
                          identical,
                          FUN.VALUE = FALSE,
                          dim_list[[1]])

    # if they're non-scalar, but have the same dimensions, that's fine too
    if (!all(match_first)) {

      # otherwise it's not fine
      msg <- sprintf('incompatible dimensions: %s',
                     dims_text)
      stop (msg)

    }
  }

  # if there's a target dimension, make sure they all match it
  if (!is.null(target_dim)) {

    # make sure it's 2D
    if (length(target_dim) == 1)
      target_dim <- c(target_dim, 1)

    target_dim <- as.integer(target_dim)

    # if they are all scalars, that's fine too
    if (!all(scalars)) {

      # check all arguments against this
      matches_target <- vapply(dim_list,
                               identical,
                               FUN.VALUE = FALSE,
                               target_dim)

      # error if not
      if (!all(matches_target)) {
        stop (sprintf('array dimensions should be %s, but input dimensions were %s',
                      paste(target_dim, collapse = 'x'),
                      dims_text))
      }

    }

    output_dim <- target_dim

  } else {

    # otherwise, find the correct output dimension
    output_dim <- do.call(pmax, dim_list)

  }

  output_dim

}

# convert an array to a vector row-wise
flatten_rowwise <- function (array) {
  dim <- dim(array)
  array <- aperm(array, rev(seq_along(dim)))
  dim(array) <- NULL
  array
}

# convert an vector to an array row-wise
unflatten_rowwise <- function (array, dim) {

  array <- as.array(array)
  # if any dim has length 1, make it a column vector
  if (length(dim) == 1)
    dim <- c(dim, 1)

  dim(array) <- rev(dim)
  array <- aperm(array, rev(seq_along(dim)))
  dim(array) <- dim
  array
}

# unlist and flatten a list of arrays to a vector row-wise
unlist_tf <- function (x) {
  # flatten each element row-wise and concatenate
  x <- lapply(x, flatten_rowwise)
  do.call(c, x)
}

# relist a vector into a list of arrays row-wise
relist_tf <- function (x, list_template) {

  # get expected dimensions of arrays and number of elements
  dims <- lapply(list_template, dim)
  lengths <- vapply(dims, prod, 1)
  runs <- rep(seq_along(lengths), lengths)

  # chop up vector into shorter lengths
  vectors <- split(x, runs)
  names(vectors) <- NULL

  # loop through vectors coercing into arrays
  list <- mapply(unflatten_rowwise, vectors, dims, SIMPLIFY = FALSE)
  names(list) <- names(list_template)
  list

}

# create an array with the same dimensions as tensor and fill it with
# consecutive increasing integers in python order
dummy <- function (dims) {
  vec <- seq_len(prod(dims)) - 1
  unflatten_rowwise(vec, dims)
}

# evaluate a greta_array, node, or tensor
grab <- function (x) {

  if (is.greta_array(x))
    x <- x$node

  if (is.node(x)) {
    x$define_tf(environment())
    x <- get(x$name)
  }

  tf$Session()$run(x)

}

# flatten a tensor to a rank-2 column vector
tf_flatten <- function (x)
  tf$reshape(x, shape = c(tf$size(x), 1L))

# flatten a greta array. Uses Python-like row-major order, so not exposed to
# users. Useful for the reducing functions
flatten <- function (x) {

  stopifnot(is.greta_array(x))

  dimfun <- function (elem_list) {
    len <- prod(dim(elem_list[[1]]))
    c(len, 1)
  }

  op('tf_flatten',
     x,
     dimfun = dimfun)
}

# function to get and check dim for univariate distributions
get_dims <- function (..., target_dim) {

  # check the dims are compatible with one another and the target if specified
  check_dims(..., target_dim)

  elem_list <- list(...)
  dims_in <- lapply(elem_list, dim)


  # if dim is null, make sure the parameters all have the same dimension (or are scalar)

  # do this in the initialization for each distribution
  # on add_parameter, expand out any scalar parameters


}

# look in the environment specified by env, and return a named list of all greta
# arrays in that environment
all_greta_arrays <- function (env = parent.frame(),
                              include_data = TRUE) {

  # all objects in that environment as a named list
  all_object_names <- ls(envir = env)
  all_objects <- lapply(all_object_names, get, envir = env)
  names(all_objects) <- all_object_names

  # find the greta arrays
  is_greta_array <- vapply(all_objects,
                           is.greta_array,
                           FUN.VALUE = FALSE)
  all_arrays <- all_objects[is_greta_array]

  # optionally strip out the data arrays
  if (!include_data) {

    is_data <- vapply(all_arrays,
                      function (x) x$node$type == 'data',
                      FUN.VALUE = FALSE)
    all_arrays <- all_arrays[!is_data]

  }

  all_arrays

}

