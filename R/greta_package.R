# package file

#' greta: Probabilistic Modelling with TensorFlow
#' @name greta
#'
#' @description greta lets you write probabilistic models interactively in
#'   native R code, then sample from them efficiently using Hamiltonian Monte
#'   Carlo.
#'
#'   The computational heavy lifting is done by TensorFlow, Google's automatic
#'   differentiation library. greta is particularly fast where the model
#'   contains lots of linear algebra, and greta models can be easily set up to
#'   run across CPUs or GPUs just by installing the relevant version of
#'   TensorFlow.
#'
#'   See the example below for the general set up of a greta model, and
#'   \link{greta-distributions}, \link{greta-operators}, \link{greta-functions},
#'   and \link{greta-transforms} for details of the currently implemeted
#'   distributions and how to combine them into mode.ls
#'
#' @docType package
#' @import tensorflow
#' @import R6
#' @examples
#' # define a simple model
#' mu = free()
#' sigma = lognormal(1, 0.1)
#' x = observed(rnorm(10))
#' x %~% normal(mu, sigma)
#'
#' # and sample from it
#' draws <- sample(mu, sigma,
#'                 n_samples = 100,
#'                 warmup = 10)
NULL

# unexported object to hold the list of defined nodes
node_list_object <- R6Class(
  'node_list_object',
  public = list(

    node_list = list(),

    add_node = function (node)
      self$node_list <- c(self$node_list, node),

    # return list of nodes. If `names` is provided, return only those
    nodes = function (names = NULL) {
      nodes <- self$node_list
      if (!is.null(names))
        nodes <- nodes[names]
      nodes
    }

  )
)

# crate the node list object whenever the package is loaded
.onLoad <- function(libname, pkgname)
  .nodes <<- node_list_object$new()
