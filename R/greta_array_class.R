# define a greta_array S3 class for the objects users manipulate

# nolint start

# coerce to greta_array class if optional = TRUE don't error if we fail, just
# return original x, which we pass along explicitly
as.greta_array <- function(x, optional = FALSE, original_x = x, ...) {
  UseMethod("as.greta_array", x)
}

# safely handle self-coercion
#' @export
as.greta_array.greta_array <- function(x, optional = FALSE, original_x = x, ...) {
  x
}

# coerce logical vectors to numerics
#' @export
as.greta_array.logical <- function(x, optional = FALSE, original_x = x, ...) {
  x[] <- as.numeric(x[])
  as.greta_array.numeric(x,
    optional = optional,
    original_x = original_x,
    ...
  )
}

# coerce dataframes if all columns can safely be converted to numeric, error
# otherwise
#' @export
as.greta_array.data.frame <- function(x, optional = FALSE,
                                      original_x = x, ...) {
  check_greta_data_frame(x, optional)

  as.greta_array.numeric(as.matrix(x),
    optional = optional,
    original_x = original_x,
    ...
  )
}

# coerce logical matrices to numeric matrices, and error if they aren't logical
# or numeric
#' @export
as.greta_array.matrix <- function(x, optional = FALSE, original_x = x, ...) {

  check_greta_array_type(x, optional)

  if (!is.numeric(x) && is.logical(x)) {
      x[] <- as.numeric(x[])
    }

  as.greta_array.numeric(x,
    optional = optional,
    original_x = original_x,
    ...
  )
}

# coerce logical arrays to numeric arrays, and error if they aren't logical
# or numeric
#' @export
as.greta_array.array <- function(x, optional = FALSE, original_x = x, ...) {

  check_greta_array_type(x, optional)

  if (!optional && !is.numeric(x) && is.logical(x)) {
      x[] <- as.numeric(x[])
  }

  as.greta_array.numeric(x,
    optional = optional,
    original_x = original_x,
    ...
  )
}

# finally, reject if there are any missing values, or set up the greta_array
#' @export
as.greta_array.numeric <- function(x, optional = FALSE, original_x = x, ...) {
  check_missing_infinite_values(x, optional)

  as.greta_array.node(data_node$new(x),
    optional = optional,
    original_x = original_x,
    ...
  )
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
    cli::cli_abort(
      c(
        "Object cannot be coerced to {.cls greta_array}",
        "Objects of class {.cls {paste(class(x), collapse = ' or ')}} cannot \\
        be coerced to a {.cls greta_array}"
      )
    )
  }

  # return x before we started messing with it
  original_x
}

# print method
#' @export
print.greta_array <- function(x, ..., n = 10) {
  node <- get_node(x)

  node_desc <- node$cli_description()

  cli::cli_text("{.pkg greta} array {.cls {node_desc}}")
  cli::cli_text("\n")

  if (is.unknowns(node$value())){
    return(print(node$value(), ..., n = n))
  }

  x_val <- node$value()
  n_print <- getOption("greta.print_max") %||% n

  n_unknowns <- length(x_val)
  x_head <- head(x_val, n = n_print)
  remaining_vals <- n_unknowns - n_print

  # print with question marks
  print.default(x_head, quote = FALSE, max = n)

  cli::cli_text("\n")

  if (remaining_vals <= 0) {
    return(invisible(x_val))
  }

  if (remaining_vals > 0 ) {
    cli::cli_alert_info(
      text = c(
        "i" = "{remaining_vals} more values\n",
        "i" = "Use {.code print(n = ...)} to see more values"
      )
    )
  }


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
print.summary.greta_array <- function(x, ...) {

  # array type
  type_text <- glue::glue(
    "'{x$type}' greta array"
  )

  if (x$length == 1) {
    shape_text <- "with 1 element"
  } else {
    dim_text <- glue::glue_collapse(x$dim, sep = "x")
    shape_text <- glue::glue(
      "with {x$length} elements ({dim_text})"
    )
  }

  # distribution info
  if (!is.null(x$distribution_name)) {
    distribution_text <- glue::glue(
      "following a {x$distribution_name} distribution"
    )
  } else {
    distribution_text <- ""
  }

  if (is.unknowns(x$values)) {
    values_text <- "\n  (values currently unknown)"
  } else {
    values_print <- capture.output(summary(x$values))
    values_text <- paste0("\n", paste(values_print, collapse = "\n"))
  }

  text <- glue::glue(
    "{type_text} {shape_text} {distribution_text} \n {values_text}"
  )
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
  string <- glue::glue("'greta_array' {string}")
  cat(string)
}

# return the unknowns array for this greta array
#' @export
as.matrix.greta_array <- function(x, ...) {
  get_node(x)$value()
}

# nolint end

# extract the node from a greta array
get_node <- function(x) {
  attr(x, "node")
}

# check for and get representations
representation <- function(x, name, error = TRUE) {
  if (is.greta_array(x)) {
    x_node <- get_node(x)
  } else {
    x_node <- x
  }
  repr <- x_node$representations[[name]]
  check_has_representation(repr, name, error)
  repr
}

has_representation <- function(x, name) {
  repr <- representation(x, name, error = FALSE)
  !is.null(repr)
}

anti_representation <- function(x, name, error = TRUE) {
  if (is.greta_array(x)) {
    x_node <- get_node(x)
  } else {
    x_node <- x
  }
  repr <- x_node$anti_representations[[name]]
  check_has_anti_representation(repr, name, error)
  repr
}


has_anti_representation <- function(x, name){
  repr <- anti_representation(x, name, error = FALSE)
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
  unknowns = unknowns_module
)
