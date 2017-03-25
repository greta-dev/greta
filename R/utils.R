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
is_scalar <- function (x) {
  identical(dim(x), c(1L, 1L))
}

# check dimensions of arguments to ops
check_dims <- function(x, y) {

  # coerce to nodes
  x <- as.greta_array(x)
  y <- as.greta_array(y)

  # if one is a scalar, it should be fine
  if ( !( is_scalar(x) | is_scalar(y) ) ) {

    # if they're non-scalar, but have the same dimensions, that's fine too
    if ( !identical(dim(x), dim(y)) ) {

      # otherwise it's not fine
      msg <- sprintf('incompatible dimensions: %s vs %s',
                     paste0(dim(x), collapse = 'x'),
                     paste0(dim(y), collapse = 'x'))
      stop (msg)

    }
  }
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
