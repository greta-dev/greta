# utility functions

# helper for *apply statements on R6 objects
member <- function(x, method)
  eval(parse(text = paste0('x$', method)))

# coerce an integer(ish) vector to a list as expected in tensorflow shape arguments
to_shape <- function(dim)
  do.call(shape, as.list(dim))

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
      stop (msg, call. = FALSE)

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
                      dims_text),
              call. = FALSE)
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

# convert Tensor to logical
tf_as_logical <- function (x)
  tf$cast(x, tf$bool)

# and to float
tf_as_float <- function (x)
  tf$cast(x, tf$float32)

# and to integer
tf_as_integer <- function (x)
  tf$cast(x, tf$int64)

# flatten a greta array into a column vector in column-major order
flatten <- function (x)
  x[seq_along(x)]

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
                      function(x) inherits(x$node, 'data_node'),
                      FUN.VALUE = FALSE)
    all_arrays <- all_arrays[!is_data]

  }

  all_arrays

}

# check the version of tensorflow is valid. error, warn, or message if not and
# (if not an error) return an invisible logical saying whether it is valid
check_tf_version <- function (alert = c('error', 'warn', 'message')) {

  alert <- match.arg(alert)

  tf_version <- tf$`__version__`
  tf_version_split <- strsplit(tf_version, '.', fixed = TRUE)[[1]]
  tf_version_valid <- as.numeric(tf_version_split[1]) >= 1

  if (!tf_version_valid) {

    text <- paste0('\n\n  greta requires TensorFlow version 1.0.0 or higher, ',
                   'but you have version ', tf_version, '\n  ',
                   'You can write models, but not sample from them.\n  ',
                   'See https://www.tensorflow.org/install for installation ',
                   'instructions.\n\n')

    switch(alert,
           error = stop (text, call. = FALSE),
           warn = warning (text, call. = FALSE),
           message = message(text))

  }

  # if not an error, return a logical on whether it was valid
  invisible(tf_version_valid)

}

# given a flat tensor, convert it into a square symmetric matrix by considering
# it  as the non-zero elements of the lower-triangular decomposition of the
# square matrix
tf_flat_to_symmetric = function (x, dims) {

  # create a dummy array to find the indices
  L_dummy <- dummy(dims)
  indices <- sort(L_dummy[upper.tri(L_dummy, diag = TRUE)])

  # create an empty vector to fill with the values
  values <- tf$zeros(shape(prod(dims), 1), dtype = tf$float32)
  values <- recombine(values, indices, x)

  # reshape into lower triangular, then symmetric matrix
  L <- tf$reshape(values, shape(dims[1], dims[2]))
  tf$matmul(tf$transpose(L), L)
}

flat_to_symmetric <- function (x, dim) {

  dimfun <- function (elem_list)
    dim

  # sum the elements
  op('flat_to_symmetric',
     x,
     operation_args = list(dims = dim),
     tf_operation = 'tf_flat_to_symmetric',
     dimfun = dimfun)

}

node_type <- function (node) {
  classes <- class(node)
  type <- grep('*_node', classes, value = TRUE)
  gsub('_node', '', type)
}

# colour scheme for plotting
greta_col <- function (which = c('main',
                                 'dark',
                                 'light',
                                 'lighter',
                                 'super_light')) {
  which <- match.arg(which)
  switch (which,
          main = '#a464b4',
          dark = '#8b4b9b',
          light = '#ba87c5',
          lighter = '#e1cce5',
          super_light = '#f5eef6')
}

