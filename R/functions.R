#' @name greta-functions
#'
#' @title functions for greta nodes
#'
#' @description This is a list of currently implemented functions to transform
#'  greta nodes. More will follow.
#'
#' @section Usage: \preformatted{
#'  log(x)
#'  exp(x)
#'  t(x)
#'  }
NULL

#' @export
`log.node` <- function (e1) {
  op("tf$log", e1)
}

#' @export
`exp.node` <- function (e1) {
  op("tf$exp", e1)
}

#' @export
`t.node` <- function (e1) {

  # reverse the dimensions
  dimfun <- function (node_list) {
    x <- node_list[[1]]
    if (length(x$dim) != 2)
      stop ('only 2D arrays can be transposed')
    rev(x$dim)
  }

  op("tf$transpose", e1, dimfun = dimfun)
}
