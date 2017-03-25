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
  class(ga) <- 'greta_array'
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

# short hand for use in functions
ga <- function (x)
  as.greta_array(x)
