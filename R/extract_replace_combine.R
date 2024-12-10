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
#' @param n a single integer, as in `utils::head()` and
#'   `utils::tail()`
#' @param nrow,ncol optional dimensions for the resulting greta array when x is
#'   not a matrix.
#' @param value for ``[<-`` a greta array to replace elements, for
#'   ``dim<-`` either NULL or a numeric vector of dimensions
#' @param ... either further indices specifying elements to extract or replace
#'   (`[`), or multiple greta arrays to combine (`cbind()`,
#'   `rbind()` & `c()`), or additional arguments (`rep()`,
#'   `head()`, `tail()`)
#' @param drop,recursive generic arguments that are ignored for greta arrays
#'
#' @details `diag()` can be used to extract or replace the diagonal part of
#'   a square and two-dimensional greta array, but it cannot be used to create a
#'   matrix-like greta array from a scalar or vector-like greta array. A static
#'   diagonal matrix can always be created with e.g. `diag(3)`, and then
#'   converted into a greta array.
#'
#'   Also note that since R 4.0.0, `head` and `tail` methods for arrays changed
#'   to print a vector rather than maintain the array structure. The `greta`
#'   package supports both methods, and will do so based on which version of R
#'    you are using.
#'
#' @examples
#' \dontrun{
#'
#' x <- as_data(matrix(1:12, 3, 4))
#'
#' # extract and replace
#' x[1:3, ]
#' x[, 2:4] <- 1:9
#' e <- diag(x)
#' diag(x) <- e + 1
#'
#' # combine
#' cbind(x[, 2], x[, 1])
#' rbind(x[1, ], x[3, ])
#' abind(x[1, ], x[3, ], along = 1)
#' c(x[, 1], x)
#' rep(x[, 2], times = 3)
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
    cli::cli_abort(
      "subscript out of bounds"
    )
  }

  node <- get_node(x)
  # if this is a data node, also subset the values and pass on
  if (is.data_node(node)) {
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
  if (length(dims_out) == 1) {
    dims_out <- c(dims_out, 1)
  }

  # give values these dimensions
  if (is.array(values)) {
    values <- array(values, dim = dims_out)
  }

  # get the index in flat python format, as a tensor
  index <- flatten_rowwise(dummy_out)

  # create operation node, passing call and dims as additional arguments
  op("extract",
    x,
    dim = dims_out,
    operation_args = list(
      nelem = nelem,
      index = index,
      dims_out = dims_out
    ),
    tf_operation = "tf_extract",
    value = values
  )
}

# replace syntax for greta array objects
#' @export
`[<-.greta_array` <- function(x, ..., value) { # nolint

  node <- get_node(x)

  if (is.variable_node(node)) {
    cli::cli_abort(
      "cannot replace values in a variable {.cls greta_array}"
    )
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
    cli::cli_abort(
      "subscript out of bounds"
    )
  }

  index <- as.vector(dummy_out)

  if (length(index) != length(replacement)) {
    replacement_is_not_multiple <- length(index) %% length(replacement) != 0
    if (replacement_is_not_multiple) {
      cli::cli_abort(
        "number of items to replace is not a multiple of replacement length"
      )
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
  either_are_unknowns <- is.unknowns(x_value) | is.unknowns(replacement_value)
  if (either_are_unknowns) {
    new_value <- as.unknowns(new_value)
  }

  # create operation node, passing call and dims as additional arguments
  op("replace",
    x,
    replacement,
    dim = dims,
    operation_args = list(
      index = index,
      dims = dims
    ),
    value = new_value,
    tf_operation = "tf_replace"
  )
}

#' @export
cbind.greta_array <- function(...) {
  dots <- list(...)
  dots <- lapply(dots, as.greta_array)

  dims <- lapply(dots, dim)
  ndims <- lengths(dims)
  arrays_are_2d <- all(ndims == 2)
  if (!arrays_are_2d) {
    cli::cli_abort(
      "all {.cls greta_array}s must be two-dimensional"
    )
  }

  # dimensions
  rows <- vapply(dims, `[`, 1, FUN.VALUE = 1)
  cols <- vapply(dims, `[`, 2, FUN.VALUE = 1)

  # check all the same
  same_number_of_rows <- all(rows == rows[1])
  if (!same_number_of_rows) {
    cli::cli_abort(
      "all {.cls greta_array}s must be have the same number of rows"
    )
  }

  # output dimensions
  dims <- c(rows[1], sum(cols))

  op("cbind", ...,
    dim = dims,
    tf_operation = "tf_cbind"
  )
}

#' @export
rbind.greta_array <- function(...) {
  dots <- list(...)
  dots <- lapply(dots, as.greta_array)

  dims <- lapply(dots, dim)
  ndims <- lengths(dims)
  arrays_2d <- all(ndims == 2)
  if (!arrays_2d) {
    cli::cli_abort(
      "all {.cls greta_array}s must be two-dimensional"
    )
  }

  # dimensions
  rows <- vapply(dims, `[`, 1, FUN.VALUE = 1)
  cols <- vapply(dims, `[`, 2, FUN.VALUE = 1)

  # check all the same
  array_cols_same <- all(cols == cols[1])
  if (!array_cols_same) {
    cli::cli_abort(
      "all {.cls greta_array}s must be have the same number of columns"
    )
  }

  # output dimensions
  dims <- c(sum(rows), cols[1])

  op("rbind", ...,
    dim = dims,
    tf_operation = "tf_rbind"
  )
}

# nolint start
#' @rdname overloaded
#' @export
abind <- function(...,
                  along = N, rev.along = NULL, new.names = NULL,
                  force.array = TRUE, make.names = use.anon.names,
                  use.anon.names = FALSE, use.first.dimnames = FALSE,
                  hier.names = FALSE, use.dnns = FALSE) {
  UseMethod("abind")
}
# nolint end

# nolint start
#' @export
abind.default <- function(...,
                          along = N, rev.along = NULL, new.names = NULL,
                          force.array = TRUE, make.names = use.anon.names,
                          use.anon.names = FALSE, use.first.dimnames = FALSE,
                          hier.names = FALSE, use.dnns = FALSE) {
  # nolint end

  # error nicely if they don't have abind installed
  abind_installed <- requireNamespace("abind", quietly = TRUE)
  if (!abind_installed) {
    cli::cli_abort(
      c(
        "{.fun abind} is being called on R arrays (not {.cls greta_array}s), \\
        but the {.pkg abind} package is not installed",
        "install {.pkg abind} with:",
        "{.code install.packages('abind')}"
      )
    )
  }

  call <- sys.call()
  call_list <- as.list(call)[-1]
  do.call(abind::abind, call_list)
}

# nolint start
#' @export
abind.greta_array <- function(...,
                              along = N, rev.along = NULL, new.names = NULL,
                              force.array = TRUE, make.names = use.anon.names,
                              use.anon.names = FALSE,
                              use.first.dimnames = FALSE, hier.names = FALSE,
                              use.dnns = FALSE) {
  # nolint end

  # warn if any of the arguments have been changed
  # nolint start
  user_set_args <- !is.null(rev.along) |
    !is.null(new.names) |
    !isTRUE(force.array) |
    !identical(make.names, FALSE) |
    !identical(use.anon.names, FALSE) |
    !identical(use.first.dimnames, FALSE) |
    !identical(hier.names, FALSE) |
    !identical(use.dnns, FALSE)
  # nolint end

  if (user_set_args) {
    cli::cli_warn(
      "only the argument {.arg along} is supported when using {.fun abind} \\
      with {.cls greta_array}s, any other arguments will be ignored"
    )
  }

  arg_list <- list(...)

  # drop any NULLs
  to_discard <- are_null(arg_list)
  if (any(to_discard)) {
    arg_list <- arg_list[!to_discard]
  }

  # get N first, in case they used the default value for along
  dims <- lapply(arg_list, dim)
  n <- max(lengths(dims))

  # needed to keep the same formals as abind
  N <- n # nolint
  along <- as.integer(force(along))

  # rationalise along, and pad N if we're prepending/appending a dimension
  ## TODO add explaining variable here
  if (along < 1 || along > n || (along > floor(along) &&
    along < ceiling(along))) {
    n <- n + 1
    along <- max(1, min(n + 1, ceiling(along)))
  }

  # TODO revisit checking functions here
  along_outside_0_n <- !(along %in% 0:n)
  if (along_outside_0_n) {
    cli::cli_abort(
      c(
        "{.arg along} must be between 0 and {n}",
        "Instead {.arg along} was {.val {along}}"
      )
    )
  }

  pre <- seq(from = 1, len = along - 1)
  post <- seq(to = n - 1, len = n - along)

  # loop though all elements of arg_list padding if necessary:
  arg_list <- lapply(
    arg_list,
    function(x) {
      dim <- dim(x)
      if (n_dim(x) == n - 1) {
        dim(x) <- c(dim[pre], 1, dim[post])
      }
      x
    }
  )

  # check the non-binding dimensions match
  dims <- lapply(arg_list, dim)
  dim_out <- rep(NA, n)
  for (dim in seq_len(n)[-along]) {
    this_dim <- vapply(dims, `[`, dim, FUN.VALUE = 1L)
    dim_varying <- !all(this_dim == this_dim[1])
    if (dim_varying) {
      cli::cli_abort(
        c(
          "all {.cls greta_array}s must have the same dimensions except on \\
          the {.arg along} dimension",
          "However, dimension {dim} had varying sizes: {this_dim}"
        )
      )
    } else {
      dim_out[dim] <- this_dim[1]
    }
  }

  bind_dims <- vapply(dims, `[`, along, FUN.VALUE = 1L)
  dim_out[along] <- sum(bind_dims)

  do.call(
    op,
    c(
      operation = "abind",
      arg_list,
      dim = list(dim_out),
      operation_args = list(list(axis = as.integer(along))),
      tf_operation = "tf_abind"
    )
  )
}

#' @export
c.greta_array <- function(...) {
  args <- list(...)

  # drop NULLs from the list
  is_null <- are_null(args)
  args <- args[!is_null]

  # try to coerce to greta arrays
  args <- lapply(args, as.greta_array, optional = TRUE)

  # return a list if they aren't all greta arrays
  is_greta_array <- are_greta_array(args)

  if (!all(is_greta_array)) {
    return(args)
  }

  # loop through arrays, flattening them R-style
  arrays <- lapply(args, flatten)

  # get output dimensions
  length_vec <- lengths(arrays)
  dim_out <- c(sum(length_vec), 1L)

  # create the op, expanding 'arrays' out to match op()'s dots input
  do.call(
    op,
    c(
      operation = "rbind",
      arrays,
      dim = list(dim_out),
      tf_operation = "tf_rbind"
    )
  )
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
dim.greta_array <- function(x) {
  as.integer(get_node(x)$dim)
}

#' @export
length.greta_array <- function(x) {
  prod(dim(x))
}

# reshape greta arrays
#' @export
`dim<-.greta_array` <- function(x, value) { # nolint

  dims <- value

  dims <- dims %||% length(x)

  # TODO revisit logic / checking functions here
  if (length(dims) == 0L) {
    cli::cli_abort(
      "length-0 dimension vector is invalid"
    )
  }

  if (length(dims) == 1L) {
    dims <- c(dims, 1L)
  }

  if (any(is.na(dims))) {
    cli::cli_abort(
      "the dims contain missing values"
    )
  }

  dims <- as.integer(dims)

  dims_contain_negatives <- any(dims < 0L)
  if (dims_contain_negatives) {
    cli::cli_abort(
      c(
        "the dims contain negative values:",
        "{.code dim(x)} returns {dim(x)}"
      )
    )
  }

  prod_dims <- prod(dims)
  len <- length(x)
  is_scalar <- len == 1
  # if x isn't a scalar and the numbers of elements don't match, error
  n_elem_not_match <- prod_dims != len
  if (!is_scalar && n_elem_not_match) {
    cli::cli_abort(
        "dims [product {prod_dims}] do not match the length of object [{len}]"
    )
  }

  # change the values similarly
  new_value <- get_node(x)$value()
  new_value <- array(new_value, dim = dims)

  unmatch_dim <- !identical(dim(x), dims)

  if (unmatch_dim && is_scalar) {

    # if the dims don't match, but x is a scalar, expand it to the required
    # dimension
    op("expand_dim",
      x,
      operation_args = list(dims = dims),
      tf_operation = "tf_expand_dim",
      dim = dims,
      value = new_value
    )
  } else {
    # otherwise, if the dimensions don't match, but the number of elements do,
    # just change the dimensions
    op("set_dim",
      x,
      operation_args = list(dims = dims),
      tf_operation = "tf_set_dim",
      dim = dims,
      value = new_value
    )
  }


  # otherwise just reorder them
}

# head handles matrices differently to arrays, so explicitly handle 2D greta
# arrays
#' @export
#' @importFrom utils head
#' @importFrom utils head.matrix
head.greta_array <- function(x, n = 6L, ...) { # nolint

  stopifnot(length(n) == 1L)

  ans <- head.matrix(x, n, ...)

  ans

}

#' @export
#' @importFrom utils tail
#' @importFrom utils tail.matrix
tail.greta_array <- function(x, n = 6L, ...) { # nolint

  stopifnot(length(n) == 1L)

  ans <- tail.matrix(x, n, ...)

  ans

}

#' @rdname overloaded
#' @export
diag <- function(x = 1, nrow, ncol) {
  UseMethod("diag", x)
}

# wrapper function to avoid a CRAN check warning about using a .Internal() call
#' @export
diag.default <- function(...) {
  base::diag(...)
}

#' @export
diag.greta_array <- function(x = 1, nrow, ncol) {
  dim <- dim(x)

  # check the rank isn't too high
  if (!is_2d(x)) {
    cli::cli_abort(
      "Cannot only extract the diagonal from a node with exactly two \\
      dimensions"
    )
  }

  is_square <- dim[1] != dim[2]
  if (is_square) {
    cli::cli_abort(
      "Diagonal elements can only be extracted from square matrices"
    )
  }

  # return the dimensions
  dims <- c(dim[1], 1)

  # return the extraction op
  op("diag", x,
    dim = dims,
    tf_operation = "tf$linalg$diag_part"
  )
}
