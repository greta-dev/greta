# Create objects of class 'unknowns' to nicely print ? valued arrays

as.unknowns <- function(x) {
  UseMethod('as.unknowns')
}

as.unknowns.array <- function (x) {
  class(x) <- c('unknowns', class(x))
  x
}

as.unknowns.matrix <- function (x)
  as.unknowns.array(x)

strip_unknown_class <- function (x) {
  classes <- class(x)
  classes <- classes[classes != 'unknowns']
  class(x) <- classes
  x
}

#' @export
print.unknowns <- function (x, ...) {
  # remove 'unknown' class attribute
  x <- strip_unknown_class(x)

  # set values to ? for printing
  x[] <- "  ?"

  # print with question marks
  print.default(x, quote = FALSE, ...)

}

`[.unknowns` <- function(x, ...) {

  # get ready to evaluate in environment above this
  pf <- parent.frame()

  # evaluate the call as a matrix/array
  call <- sys.call()

  # make x in parent environment not an unknowns
  pf$x <- strip_unknown_class(pf$x)

  # execute the subsetting call there, without dropping
  call[[1]] <- .Primitive("[")
  call$drop <- FALSE
  out <- eval(call, envir = pf)

  # make x in parent environment not unknowns again
  pf$x <- as.unknowns(pf$x)

  # convert out back to unknowns here, and return
  as.unknowns(out)
}

# create an unknowns array from some dimensions
unknowns <- function (dims = c(1, 1), data = NA) {
  x <- array(data = data, dim = dims)
  as.unknowns(x)
}
