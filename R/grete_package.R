# package file

#' grete: Probabilistic Modelling with TensorFlow
#' @name grete
#' @docType package
#' @import tensorflow
#' @import R6
#' @export
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

# put a node list object in the global environment
#' @export
assign('.nodes', node_list_object$new(),
       envir = parent.frame())
