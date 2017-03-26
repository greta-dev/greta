# define a greta_array S3 class for the objects users manipulate

# coerce to greta_array class
as.greta_array <- function(x, ...) {
  UseMethod('as.greta_array')
}

# safely handle self-coersion
as.greta_array.greta_array <- function (x, ...)
  x

# node method (only one defined)
as.greta_array.node <- function (x, ...) {
  ga <- list(node = x)
  class(ga) <- c('greta_array', 'array')
  ga
}

# array method (only one defined)
as.greta_array.array <- function (x, ...)
  as.greta_array(to_node(x))

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

# get dimensions
#' @export
dim.greta_array <- function(x)
  x$node$dim

#' @export
length.greta_array <- function(x)
  prod(dim(x))

# head handles matrices differently to arrays, so explicitly handle 2D greta
# arrays
#' @export
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
