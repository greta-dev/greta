#' @title Create objects of class 'unknowns' to nicely print ? valued arrays
#' @param x object to convert to "unknowns" class
#' @export
as.unknowns <- function(x) { # nolint
  UseMethod("as.unknowns")
}

#' @export
as.unknowns.unknowns <- function(x) { # nolint
  x
}

#' @export
as.unknowns.array <- function(x) { # nolint
  class(x) <- c("unknowns", class(x))
  x
}

#' @export
as.unknowns.matrix <- function(x) { # nolint
  as.unknowns.array(x)
}

#' @export
print.unknowns <- function(x, ..., n = 10) {
  # remove 'unknown' class attribute
  x <- unclass(x)

  # set NA values to ? for printing
  x[is.na(x)] <- " ?"

  n_print <- getOption("greta.print_max") %||% n

  n_unknowns <- length(x)
  x_head <- head(x, n = n_print)
  remaining_vals <- n_unknowns - n_print

  # print with question marks
  print.default(x_head, quote = FALSE)

  cli::cli_text("\n")

  if (remaining_vals <= 0) {
    return(invisible(x))
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

# create an unknowns array from some dimensions
unknowns <- function(dims = c(1, 1), data = as.numeric(NA)) {
  x <- array(data = data, dim = dims)
  as.unknowns(x)
}

#' @title set dims like on a matrix/array
#' @param x matrix/array to set values to
#' @param value values that are  being set set
#' @export
`dim<-.unknowns` <- function(x, value) { # nolint
  x <- unclass(x)
  dim(x) <- value
  as.unknowns(x)
}

unknowns_module <- module(
  unknowns,
  as.unknowns
)
