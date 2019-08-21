# define a greta_array S3 class for the objects users manipulate

# Begin Exclude Linting

# coerce to greta_array class if optional = TRUE don't error if we fail, just
# return original x, which we pass along explicitly
as.greta_array <- function(x, optional = FALSE, original_x = x, ...)
  UseMethod("as.greta_array", x)

# safely handle self-coercion
#' @export
as.greta_array.greta_array <- function(x, optional = FALSE, original_x = x, ...)
  x

# coerce logical vectors to numerics
#' @export
as.greta_array.logical <- function(x, optional = FALSE, original_x = x, ...) {
  x[] <- as.numeric(x[])
  as.greta_array.numeric(x,
                         optional = optional,
                         original_x = original_x,
                         ...)
}

# coerce dataframes if all columns can safely be converted to numeric, error
# otherwise
#' @export
as.greta_array.data.frame <- function(x, optional = FALSE,
                                      original_x = x, ...) {
  classes <- vapply(x, class, "")
  valid <- classes %in% c("numeric", "integer", "logical")

  if (!optional & !all(valid)) {
    invalid_types <- unique(classes[!valid])
    stop("cannot coerce a dataframe to a greta_array unless all columns are ",
         "numeric, integer or logical. This dataframe had columns of type: ",
         paste(invalid_types, collapse = ", "),
         call. = FALSE)
  }

  as.greta_array.numeric(as.matrix(x),
                         optional = optional,
                         original_x = original_x,
                         ...)

}

# coerce logical matrices to numeric matrices, and error if they aren't logical
# or numeric
#' @export
as.greta_array.matrix <- function(x, optional = FALSE, original_x = x, ...) {

  if (!is.numeric(x)) {
    if (is.logical(x)) {
      x[] <- as.numeric(x[])
    } else if (!optional) {
      stop("cannot convert a matrix to a greta_array unless it is numeric, ",
           "integer or logical. This matrix had type: ",
           class(as.vector(x)),
           call. = FALSE)
    }
  }

  as.greta_array.numeric(x,
                         optional = optional,
                         original_x = original_x,
                         ...)

}

# coerce logical arrays to numeric arrays, and error if they aren't logical
# or numeric
#' @export
as.greta_array.array <- function(x, optional = FALSE, original_x = x, ...) {

  if (!optional & !is.numeric(x)) {

    if (is.logical(x))
      x[] <- as.numeric(x[])
    else
      stop("cannot convert an array to a greta_array unless it is numeric, ",
           "integer or logical. This array had type: ",
           class(as.vector(x)),
           call. = FALSE)

  }

  as.greta_array.numeric(x,
                         optional = optional,
                         original_x = original_x,
                         ...)

}

# finally, reject if there are any missing values, or set up the greta_array
#' @export
as.greta_array.numeric <- function(x, optional = FALSE, original_x = x, ...) {
  if (!optional & any(!is.finite(x))) {
    stop("cannot convert objects with missing or infinite values ",
         "to greta_arrays", call. = FALSE)
  }
  as.greta_array.node(data_node$new(x),
                      optional = optional,
                      original_x = original_x,
                      ...)
}

# node method (only one that does anything)
#' @export
as.greta_array.node <- function(x, optional = FALSE, original_x = x, ...) {
  ga <- x$value()
  attr(ga, "node") <- x
  class(ga) <- c("greta_array", "array")
  ga
}

# otherwise error
#' @export
as.greta_array.default <- function(x, optional = FALSE, original_x = x, ...) {

  if (!optional) {
    stop("objects of class ",
         paste(class(x), collapse = " or "),
         " cannot be coerced to greta arrays",
         call. = FALSE)
  }

  # return x before we started messing with it
  original_x

}

# print method
#' @export
print.greta_array <- function(x, ...) {

  node <- get_node(x)
  text <- sprintf("greta array (%s)\n\n",
                  node$description())

  cat(text)
  print(node$value(), ...)
}

# summary method
#' @export
summary.greta_array <- function(object, ...) {

  node <- get_node(object)

  sry <- list(
    type = node_type(node),
    length = length(object),
    dim = dim(object),
    distribution_name = node$distribution$distribution_name,
    values = node$value()
  )

  class(sry) <- "summary.greta_array"
  sry

}

# summary print method
#' @export
#' @method print summary.greta_array
print.summary.greta_array <- function (x, ...) {

  # array type
  type_text <- sprintf("'%s' greta array",
                       x$type)

  if (x$length == 1) {
    shape_text <- "with 1 element"
  } else {
    dim_text <- paste(x$dim, collapse = " x ")
    shape_text <- sprintf("with %i elements (%s)",
                          x$length,
                          dim_text)
  }

  # distribution info
  if (!is.null(x$distribution_name)) {
    distribution_text <- sprintf("following a %s distribution",
                                 x$distribution_name)
  } else {
    distribution_text <- ""
  }

  if (inherits(x$values, "unknowns")) {
    values_text <- "\n  (values currently unknown)"
  } else {
    values_print <- capture.output(summary(x$values))
    values_text <- paste0("\n", paste(values_print, collapse = "\n"))
  }

  text <- paste(type_text, shape_text, distribution_text, paste0("\n",
                values_text))
  cat(text)
  invisible(x)

}

# str method
#' @export
#' @importFrom utils str
str.greta_array <- function(object, ...) {
  value <- get_node(object)$value()
  array <- unclass(value)
  string <- capture.output(str(array))
  string <- gsub("NA", "?", string)
  string <- paste("'greta_array'", string)
  cat(string)
}

# return the unknowns array for this greta array
#' @export
as.matrix.greta_array <- function(x, ...)
  get_node(x)$value()

# End Exclude Linting

# extract the node from a greta array
get_node <- function(x)
  attr(x, "node")

# check for and get representations
representation <- function(x, name, error = TRUE) {
  if (inherits(x, "greta_array")) {
    x_node <- get_node(x)
  } else {
    x_node <- x
  }
  repr <- x_node$representations[[name]]
  if (error && is.null(repr)) {
    stop("greta array has no representation '",
         name, "'", call. = FALSE)
  }
  repr
}

has_representation <- function(x, name) {
  repr <- representation(x, name, error = FALSE)
  !is.null(repr)
}

# helper function to make a copy of the greta array & tensor
copy_representation <- function(x, name) {
  repr <- representation(x, name)
  identity(repr)
}

greta_array_module <- module(as.greta_array,
                             get_node,
                             has_representation,
                             representation,
                             copy_representation,
                             unknowns = unknowns_module)
