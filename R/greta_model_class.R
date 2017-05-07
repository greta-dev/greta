# greta_model objects

#' @name greta-model
#' @title Greta Model Objects
#' @description Methods to inspect \code{greta_model} objects.
NULL

# register generic method to coerce objects to a greta model
as.greta_model <- function(x, ...)
  UseMethod('as.greta_model', x)

as.greta_model.dag_class <- function (x, ...) {
  ans <- list(dag = x)
  class(ans) <- 'greta_model'
  ans
}

#' @rdname greta-model
#' @param x a \code{greta_model} object
#' @param \dots further arguments passed to or from other methods (currently ignored).
#' @export
print.greta_model <- function (x, ...) {
  cat('greta model')
}

