# create an object stash in greta's namespace, to return traces to the user when
# they abort a run
greta_stash <- new.env()
greta_stash$python_has_been_initialised <- FALSE
greta_stash$numerical_messages <- c(
  "is not invertible",
  "Cholesky decomposition was not successful"
)

# create a named list
module <- function(..., sort = TRUE) {
  dots <- list(...)
  names <- names(dots)

  # guess names from call
  cl <- match.call()
  nm <- as.character(as.list(cl)[-1])

  if (is.null(names)) {
    names(dots) <- nm
  } else {
    blank_names <- names == ""
    names[blank_names] <- nm[blank_names]
    names(dots) <- names
  }

  if (sort) {
    dots <- dots[order(names(dots))]
  }

  dots
}
