# define a greta_array S3 class for the objects users manipulate

# coerce to greta_array class
as.greta_array <- function(x, ...) {
  UseMethod('as.greta_array')
}

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
print.greta_array <- function (x, ...) {
  text <- sprintf('greta array (%s)\n\n',
                  x$node$type)
  cat(text)
  print(x$node$value(), ...)
}

# get dimensions
dim.greta_array <- function(x) x$node$dim

# need to change generics to act on greta_array S3 class, and change op to a
# function that takes greta_arrays, gets their nodes, creates an operation node,
# and wraps it in a greta_array object

# change all distribution etc. constructors to wrap in greta_array class too,
# e.g.:
# beta <- function (shape1, shape2, dim = 1)
#   ga(greta:::beta_distribution$new(shape1, shape2, dim))
#

# short hand for use in functions
ga <- function (x) as.greta_array(x)
