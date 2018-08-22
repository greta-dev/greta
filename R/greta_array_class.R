# define a greta_array S3 class for the objects users manipulate

# coerce to greta_array class
as.greta_array <- function(x, ...)
  UseMethod ("as.greta_array", x)

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
  classes <- vapply(x, class, "")
  valid <- classes %in% c("numeric", "integer", "logical")

  if (!all(valid)) {
    invalid_types <- unique(classes[!valid])
    stop ("cannot coerce a dataframe to a greta_array unless all columns are ",
          "numeric, integer or logical. This dataframe had columns of type: ",
          paste(invalid_types, collapse = ", "),
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
      stop ("cannot convert a matrix to a greta_array unless it is numeric, ",
            "integer or logical. This matrix had type: ",
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
      stop ("cannot convert an array to a greta_array unless it is numeric, ",
            "integer or logical. This array had type: ",
            class(as.vector(x)),
            call. = FALSE)

  }

  as.greta_array.numeric(x)

}

# finally, reject if there are any missing values, or set up the greta_array
#' @export
as.greta_array.numeric <- function (x) {
  if (any(!is.finite(x)))
    stop ("cannot convert objects with missing or infinite values to greta_arrays",
          call. = FALSE)
  as.greta_array.node(data_node$new(x))
}

# node method (only one that does anything)
#' @export
as.greta_array.node <- function (x, ...) {
  ga <- x$value()
  attr(ga, "node") <- x
  class(ga) <- c("greta_array", "array")
  ga
}

# otherwise error
#' @export
as.greta_array.default <- function (x) {
  stop ("objects of class ",
        paste(class(x), collapse = " or "),
        " cannot be coerced to greta arrays",
        call. = FALSE)
}

# print method
#' @export
print.greta_array <- function (x, ...) {

  node <- get_node(x)
  text <- sprintf("greta array (%s)\n\n",
                  node$description())

  cat(text)
  print(node$value(), ...)
}

# summary method
#' @export
summary.greta_array <- function (object, ...) {

  node <- get_node(object)

  # array type
  type_text <- sprintf("'%s' greta array",
                       node_type(node))

  len <- length(object)
  if (len == 1) {
    shape_text <- "with 1 element"
  } else {
    dim_text <- paste(dim(object), collapse = " x ")
    shape_text <- sprintf("with %i elements (%s)",
                          len,
                          dim_text)
  }

  # distribution info
  if (inherits(node$distribution, 'distribution_node')) {
    distribution_text <- sprintf("following a %s distribution",
                                 node$distribution$distribution_name)

  } else {
    distribution_text <- ""
  }

  cat(type_text, shape_text, distribution_text, "\n")

  values <- node$value()
  if (inherits(values, "unknowns")) {
    cat("\n  (values currently unknown)")
  } else {
    cat("\n")
    print(summary(values))
  }

}

# str method
#' @export
str.greta_array <- function (object, ...) {
  value <- get_node(object)$value()
  array <- unclass(value)
  string <- capture.output(str(array))
  string <- gsub("NA", "?", string)
  string <- paste("'greta_array'", string)
  cat(string)
}

# return the unknowns array for this greta array
#' @export
as.matrix.greta_array <- function (x, ...)
  get_node(x)$value()

# extract the node from a greta array
get_node <- function (x) {
  attr(x, "node")
}

greta_array_module <- module(as.greta_array,
                             get_node,
                             unknowns = unknowns_module)
