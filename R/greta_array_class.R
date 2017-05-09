# define a greta_array S3 class for the objects users manipulate

# coerce to greta_array class
as.greta_array <- function(x, ...)
  UseMethod('as.greta_array', x)

# safely handle self-coersion
#' @export
as.greta_array.greta_array <- function (x, ...)
  x

# coerce logical vectors to numerics
#' @export
as.greta_array.logical <- function (x) {
  x[] <- as.numeric(x[])
  as.greta_array.numeric(x)
}

# coerce dataframes if all columns can safely be converted to numeric, error
# otherwise
#' @export
as.greta_array.data.frame <- function (x) {
  classes <- vapply(x, class, '')
  valid <- classes %in% c('numeric', 'integer', 'logical')

  if (!all(valid)) {
    invalid_types <- unique(classes[!valid])
    stop ('cannot coerce a dataframe to a greta_array unless all columns are ',
          'numeric, integer or logical. This dataframe had columns of type: ',
          paste(invalid_types, collapse = ', '),
          call. = FALSE)
  }

  as.greta_array.numeric(as.matrix(x))

}

# coerce logical matrices to numeric matrices, and error if they aren't logical
# or numeric
#' @export
as.greta_array.matrix <- function (x) {
  if (!is.numeric(x)) {

    if (is.logical(x))
      x[] <- as.numeric(x[])
    else
      stop ('cannot convert a matrix to a greta_array unless it is numeric, ',
            'integer or logical. This matrix had type: ',
            class(as.vector(x)),
            call. = FALSE)

  }

  as.greta_array.numeric(x)

}

# coerce logical arrays to numeric arrays, and error if they aren't logical
# or numeric
#' @export
as.greta_array.array <- function (x) {
  if (!is.numeric(x)) {

    if (is.logical(x))
      x[] <- as.numeric(x[])
    else
      stop ('cannot convert an array to a greta_array unless it is numeric, ',
            'integer or logical. This array had type: ',
            class(as.vector(x)),
            call. = FALSE)

  }

  as.greta_array.numeric(x)

}

# finally, reject if there are any missing values, or set up the greta_array
#' @export
as.greta_array.numeric <- function (x) {
  if (any(!is.finite(x)))
    stop ('cannot convert objects with missing or infinite values to greta_arrays',
          call. = FALSE)
  as.greta_array.node(data_node$new(x))
}

# node method (only one that does anything)
#' @export
as.greta_array.node <- function (x, ...) {
  ga <- list(node = x)
  class(ga) <- c('greta_array', 'array')
  ga
}

# otherwise error
#' @export
as.greta_array.default <- function (x) {
  stop ('objects of class ',
        paste(class(x), collapse = ' or '),
        ' cannot be coerced to greta arrays',
        call. = FALSE)
}

# checking class status
is.greta_array <- function (x)
  inherits(x, 'greta_array')

# print method
#' @export
print.greta_array <- function (x, ...) {
  text <- sprintf('greta array (%s)\n\n',
                  x$node$type)
  cat(text)
  print(x$node$value(), ...)
}

# summary method
#' @export
summary.greta_array <- function (object, ...) {
  # array type
  type_text <- sprintf("'%s' greta array",
                       object$node$type)

  len <- length(object)
  if (len == 1) {
    shape_text <- "with 1 element"
  } else {
    dim_text <- paste(dim(object), collapse = ' x ')
    shape_text <- sprintf("with %i elements (%s)",
                          len,
                          dim_text)
  }

  # distribution info
  if (inherits(object$node, 'distribution_node')) {
    distribution_text <- sprintf("following a %s distribution",
                                 object$node$distribution_name)

  } else {
    distribution_text <- ""
  }

  cat(type_text, shape_text, distribution_text, '\n')

  values <- object$node$value()
  if (inherits(values, 'unknowns')) {
    cat('\n  (values currently unknown)')
  } else {
    cat('\n')
    print(summary(values))
  }

}

# get dimensions
#' @export
dim.greta_array <- function(x)
  as.integer(x$node$dim)

#' @export
length.greta_array <- function(x)
  prod(dim(x))

# head handles matrices differently to arrays, so explicitly handle 2D greta
# arrays
#' @export
#' @importFrom utils head
head.greta_array <- function (x, n = 6L, ...) {

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
    # otherwise, take thefirst n elements

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
tail.greta_array <- function (x, n = 6L, ...) {

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

# short hand for use in functions
ga <- function (x)
  as.greta_array(x)
