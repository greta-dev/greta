#' @name extract-replace-combine
#' @aliases extract replace cbind rbind c rep
#' @title extract, replace and combine greta arrays
#'
#' @description Generic methods to extract and replace elements of greta arrays,
#'   or to combine greta arrays.
#'
#' @section Usage: \preformatted{
#' # extract
#' x[i]
#' x[i, j, ..., drop = FALSE]
#' head(x, n = 6L, ...)
#' tail(x, n = 6L, ...)
#' diag(x, nrow, ncol)
#'
#' # replace
#' x[i] <- value
#' x[i, j, ...] <- value
#' diag(x) <- value
#'
#' # combine
#' cbind(...)
#' rbind(...)
#' abind(...)
#' c(..., recursive = FALSE)
#' rep(x, times, ..., recursive = FALSE)
#'
#' # get and set dimensions
#' length(x)
#' dim(x)
#' dim(x) <- value
#' }
#'
#' @param x a greta array
#' @param i,j indices specifying elements to extract or replace
#' @param n a single integer, as in \code{utils::head()} and
#'   \code{utils::tail()}
#' @param nrow,ncol optional dimensions for the resulting greta array when x is
#'   not a matrix.
#' @param value for \code{`[<-`} a greta array to replace elements, for
#'   \code{`dim<-`} either NULL or a numeric vector of dimensions
#' @param ... either further indices specifying elements to extract or replace
#'   (\code{[}), or multiple greta arrays to combine (\code{cbind()},
#'   \code{rbind()} & \code{c()}), or additional arguments (\code{rep()},
#'   \code{head()}, \code{tail()})
#' @param drop,recursive generic arguments that are ignored for greta arrays
#'
#' @details \code{diag()} can be used to extract or replace the diagonal part of
#'   a square and two-dimensional greta array, but it cannot be used to create a
#'   matrix-like greta array from a scalar or vector-like greta array. A static
#'   diagonal matrix can always be created with e.g. \code{diag(3)}, and then
#'   converted into a greta array.
#'
#' @examples
#' \dontrun{
#'
#'  x <- as_data(matrix(1:12, 3, 4))
#'
#'  # extract/replace
#'  x[1:3, ]
#'  x[, 2:4] <- 1:9
#'  e <- diag(x)
#'  diag(x) <- e + 1
#'
#'  # combine
#'  cbind(x[, 2], x[, 1])
#'  rbind(x[1, ], x[3, ])
#'  abind(x[1, ], x[3, ], along = 1)
#'  c(x[, 1], x)
#'  rep(x[, 2], times = 3)
#' }
NULL

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
  assign("._dummy_in", dummy_in, envir = pf)
  dummy_out <- do.call(.Primitive("["), call_list, envir = pf)
  rm("._dummy_in", envir = pf)

  if (any(is.na(dummy_out))) {
    stop("subscript out of bounds",
         call. = FALSE)
  }

  node <- get_node(x)
  # if this is a data node, also subset the values and pass on
  if (inherits(node, "data_node")) {

    values_in <- node$value()
    call_list <- as.list(call)[-1]
    call_list[[1]] <- as.name("._values_in")
    call_list$drop <- FALSE

    # put values_in in the parent environment & execute there (to evaluate any
    # promises using variables in that environment), then remove
    pf <- parent.frame()
    assign("._values_in", values_in, envir = pf)
    values_out <- do.call(.Primitive("["), call_list, envir = pf)
    rm("._values_in", envir = pf)

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

  # create operation node, passing call and dims as additional arguments
  op("extract",
     x,
     dim = dims_out,
     operation_args = list(nelem = nelem,
                           index = index,
                           dims_out = dims_out),
     tf_operation = "tf_extract",
     value = values)

}

# replace syntax for greta array objects
#' @export
`[<-.greta_array` <- function(x, ..., value) {

  node <- get_node(x)

  if (inherits(node, "variable_node")) {
    stop("cannot replace values in a variable greta array",
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
  assign("._dummy_in", dummy, envir = pf)
  dummy_out <- do.call(.Primitive("["), call_list, envir = pf)
  rm("._dummy_in", envir = pf)

  if (any(is.na(dummy_out))) {
    stop("subscript out of bounds",
         call. = FALSE)
  }

  index <- as.vector(dummy_out)

  if (length(index) != length(replacement)) {

    if (length(index) %% length(replacement) != 0) {

      stop("number of items to replace is not a multiple of ",
           "replacement length")

    } else {

      replacement <- rep(replacement, length.out = length(index))

    }
  }

  # do replace on the values (unknowns or arrays)
  x_value <- node$value()
  replacement_value <- get_node(replacement)$value()

  new_value <- x_value
  r_index <- match(index, dummy)
  new_value[r_index] <- replacement_value

  # if either parent has an unknowns array as a value, coerce this to unknowns
  if (inherits(x_value, "unknowns") | inherits(replacement_value, "unknowns"))
    new_value <- as.unknowns(new_value)

  # create operation node, passing call and dims as additional arguments
  op("replace",
     x,
     replacement,
     dim = dims,
     operation_args = list(index = index,
                           dims = dims),
     value = new_value,
     tf_operation = "tf_replace")

}

#' @export
cbind.greta_array <- function(...) {

  dots <- list(...)
  dots <- lapply(dots, as.greta_array)

  dims <- lapply(dots, dim)
  ndims <- vapply(dims, length, FUN.VALUE = 1)
  if (!all(ndims == 2))
    stop("all greta arrays must be two-dimensional")

  # dimensions
  rows <- vapply(dims, `[`, 1, FUN.VALUE = 1)
  cols <- vapply(dims, `[`, 2, FUN.VALUE = 1)

  # check all the same
  if (!all(rows == rows[1]))
    stop("all greta arrays must be have the same number of rows",
         call. = FALSE)

  # output dimensions
  dims <- c(rows[1], sum(cols))

  op("cbind", ...,
     dim = dims,
     tf_operation = "tf_cbind")

}

#' @export
rbind.greta_array <- function(...) {

  dots <- list(...)
  dots <- lapply(dots, as.greta_array)

  dims <- lapply(dots, dim)
  ndims <- vapply(dims, length, FUN.VALUE = 1)
  if (!all(ndims == 2))
    stop("all greta arrays must be two-dimensional")

  # dimensions
  rows <- vapply(dims, `[`, 1, FUN.VALUE = 1)
  cols <- vapply(dims, `[`, 2, FUN.VALUE = 1)

  # check all the same
  if (!all(cols == cols[1]))
    stop("all greta arrays must be have the same number of columns",
         call. = FALSE)

  # output dimensions
  dims <- c(sum(rows), cols[1])

  op("rbind", ...,
     dim = dims,
     tf_operation = "tf_rbind")

}

#' @rdname overloaded
#' @export
abind <- function(...,
                  along = N, rev.along = NULL, new.names = NULL,
                  force.array = TRUE, make.names = use.anon.names,
                  use.anon.names = FALSE, use.first.dimnames = FALSE,
                  hier.names = FALSE, use.dnns = FALSE) {
  UseMethod("abind")
}

# clear CRAN checks spotting floating global variables
#' @importFrom utils globalVariables
utils::globalVariables("N", "greta")

#' @export
abind.default <- function(...,
                          along = N, rev.along = NULL, new.names = NULL,
                          force.array = TRUE, make.names = use.anon.names,
                          use.anon.names = FALSE, use.first.dimnames = FALSE,
                          hier.names = FALSE, use.dnns = FALSE) {

  # error nicely if they don't have abind installed
  abind_installed <- requireNamespace("abind", quietly = TRUE)
  if (!abind_installed) {
    stop("abind is being called on R arrays (not greta arrays), ",
         "but the abind package is not installed",
         call. = FALSE)
  }

  call <- sys.call()
  call_list <- as.list(call)[-1]
  do.call(abind::abind, call_list)

}

#' @export
abind.greta_array <- function(...,
                              along = N, rev.along = NULL, new.names = NULL,
                              force.array = TRUE, make.names = use.anon.names,
                              use.anon.names = FALSE,
                              use.first.dimnames = FALSE, hier.names = FALSE,
                              use.dnns = FALSE) {

  # warn if any of the arguments have been changed
  user_set_args <- !is.null(rev.along) |
    !is.null(new.names) |
    !isTRUE(force.array) |
    !identical(make.names, FALSE) |
    !identical(use.anon.names, FALSE) |
    !identical(use.first.dimnames, FALSE) |
    !identical(hier.names, FALSE) |
    !identical(use.dnns, FALSE)

  if (user_set_args) {
    warning("only the argument 'along' is supported when using abind ",
            "with greta arrays, any other arguments will be ignored")
  }

  arg.list <- list(...)

  # drop any NULLs
  to_discard <- vapply(arg.list, is.null, FUN.VALUE = FALSE)
  if (any(to_discard)) {
    arg.list <- arg.list[!to_discard]
  }

  # get N first, in case they used the default value for along
  dims <- lapply(arg.list, dim)
  N <- max(vapply(dims, length, FUN.VALUE = 1L))
  along <- as.integer(force(along))

  # rationalise along, and pad N if we're prepending/appending a dimension
  if (along < 1 || along > N || (along > floor(along) &&
                                 along < ceiling(along))) {
    N <- N + 1
    along <- max(1, min(N + 1, ceiling(along)))
  }

  if (!along %in% 0:N) {
    stop("along must be between 0 and ", N, call. = FALSE)
  }

  pre <- seq(from = 1, len = along - 1)
  post <- seq(to = N - 1, len = N - along)


  # rationalise the dimensions
  arg.list <- arg.list

  # loop though all elements of arg.list padding if necessary:
  arg.list <- lapply(arg.list,
                     function(x) {
                       dim <- dim(x)
                       if (length(dim) == N - 1)
                         dim(x) <- c(dim[pre], 1, dim[post])
                       x
                     })

  # check the non-binding dimensions match
  dims <- lapply(arg.list, dim)
  dim_out <- rep(NA, N)
  for (dim in seq_len(N)[-along]) {
    this_dim <- vapply(dims, `[`, dim, FUN.VALUE = 1L)
    if (!all(this_dim == this_dim[1])) {
      stop("all greta arrays must have the same dimensions ",
           "except on the 'along' dimension, but dimension ", dim,
           " had varying sizes: ", paste(this_dim, collapse = ", "),
           call. = FALSE)
    } else {
      dim_out[dim] <- this_dim[1]
    }
  }

  bind_dims <- vapply(dims, `[`, along, FUN.VALUE = 1L)
  dim_out[along] <- sum(bind_dims)

  do.call(op,
          c(operation = "abind",
            arg.list,
            dim = list(dim_out),
            operation_args = list(list(axis = as.integer(along))),
            tf_operation = "tf_abind"))

}

#' @export
c.greta_array <- function(...) {

  args <- list(...)

  # drop NULLs from the list
  is_null <- vapply(args, is.null, FUN.VALUE = FALSE)
  args <- args[!is_null]

  # try to coerce to greta arrays
  args <- lapply(args, as.greta_array, optional = TRUE)

  # return a list if they aren't all greta arrays
  is_greta_array <- vapply(args,
                           inherits, "greta_array",
                           FUN.VALUE = FALSE)

  if (!all(is_greta_array))
    return(args)

  # loop through arrays, flattening them R-style
  arrays <- lapply(args, flatten)

  # get output dimensions
  length_vec <- vapply(arrays, length, FUN.VALUE = 1)
  dim_out <- c(sum(length_vec), 1L)

  # create the op, expanding 'arrays' out to match op()'s dots input
  do.call(op,
          c(operation = "rbind",
            arrays,
            dim = list(dim_out),
            tf_operation = "tf_rbind"))

}

#' @export
rep.greta_array <- function(x, ...) {

  # get the index
  idx <- rep(seq_along(x), ...)

  # apply (implicitly coercing to a column vector)
  x[idx]

}

# get dimensions
#' @export
dim.greta_array <- function(x)
  as.integer(get_node(x)$dim)

#' @export
length.greta_array <- function(x)
  prod(dim(x))

# reshape greta arrays
#' @export
`dim<-.greta_array` <- function(x, value) {

  dims <- value

  if (is.null(dims))
    dims <- length(x)

  if (length(dims) == 0L)
    stop("length-0 dimension vector is invalid",
         call. = FALSE)

  if (length(dims) == 1L)
    dims <- c(dims, 1L)

  if (any(is.na(dims)))
    stop("the dims contain missing values",
         call. = FALSE)

  dims <- as.integer(dims)

  if (any(dims < 0L))
    stop("the dims contain negative values",
         call. = FALSE)

  prod_dims <- prod(dims)
  len <- length(x)

  if (prod_dims != len) {
    msg <- sprintf("dims [product %i] do not match the length of object [%i]",
                   prod_dims, len)
    stop(msg, call. = FALSE)
  }

  new_value <- get_node(x)$value()
  dim(new_value) <- dims

  op("set_dim",
     x,
     operation_args = list(dims = dims),
     tf_operation = "tf_set_dim",
     dim = dims,
     value = new_value)

}

# head handles matrices differently to arrays, so explicitly handle 2D greta
# arrays
#' @export
#' @importFrom utils head
head.greta_array <- function(x, n = 6L, ...) {

  stopifnot(length(n) == 1L)

  # if x is matrix-like, take the top n rows
  if (length(dim(x)) == 2) {

    nrx <- nrow(x)
    if (n < 0L)
      n <- max(nrx + n, 0L)
    else
      n <- min(n, nrx)

    ans <- x[seq_len(n), , drop = FALSE]

  } else {
    # otherwise, take the first n elements

    if (n < 0L)
      n <- max(length(x) + n, 0L)
    else
      n <- min(n, length(x))

    ans <- x[seq_len(n)]

  }

  ans

}

#' @export
#' @importFrom utils tail
tail.greta_array <- function(x, n = 6L, ...) {

  stopifnot(length(n) == 1L)

  # if x is matrix-like, take the top n rows
  if (length(dim(x)) == 2) {

    nrx <- nrow(x)

    if (n < 0L)
      n <- max(nrx + n, 0L)
    else
      n <- min(n, nrx)

    sel <- as.integer(seq.int(to = nrx, length.out = n))
    ans <- x[sel, , drop = FALSE]

  } else {
    # otherwise, take the first n elements

    xlen <- length(x)

    if (n < 0L)
      n <- max(xlen + n, 0L)
    else
      n <- min(n, xlen)

    ans <- x[seq.int(to = xlen, length.out = n)]

  }

  ans

}

#' @rdname overloaded
#' @export
diag <- function(x = 1, nrow, ncol)
  UseMethod("diag", x)

# wrapper function to avoid a CRAN check warning about using a .Internal() call
#' @export
diag.default <- function(...)
  base::diag(...)

#' @export
diag.greta_array <- function(x = 1, nrow, ncol) {

  dim <- dim(x)

  # check the rank isn't too high
  if (length(dim) != 2) {
    stop("cannot only extract the diagonal from a node ",
         "with exactly two dimensions")
  }

  if (dim[1] != dim[2]) {
    stop("diagonal elements can only be extracted from square matrices")
  }

  # return the dimensions
  dims <- c(dim[1], 1)

  # return the extraction op
  op("diag", x,
     dim = dims,
     tf_operation = "tf$matrix_diag_part")

}
