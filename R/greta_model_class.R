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

#' @rdname greta-model
#' @param y not used
#' @export
plot.greta_model <- function (x, y, ...) {

  if (!requireNamespace('DiagrammeR', quietly = TRUE))
    stop ('the DiagrammeR package must be installed to plot greta models',
          call. = FALSE)

  # set up graph
  dag_mat <- x$dag$adjacency_matrix()

  gr <- DiagrammeR::from_adj_matrix(dag_mat,
                                    mode = 'directed',
                                    use_diag = FALSE)

  n_nodes <- nrow(gr$nodes_df)
  n_edges <- nrow(gr$edges_df)

  names <- names(x$dag$node_list)
  types <- x$dag$node_types
  to <- gr$edges_df$to
  from <- gr$edges_df$from

  node_shapes <- rep('square', n_nodes)
  node_shapes[types == 'variable'] <- 'circle'
  node_shapes[types == 'distribution'] <- 'diamond'
  node_shapes[types == 'operation'] <- 'circle'

  node_colours <- rep('PaleTurquoise', n_nodes)
  node_colours[types == 'variable'] <- 'orange'
  node_colours[types == 'distribution'] <- 'violet'
  node_colours[types == 'operation'] <- 'lightgray'

  node_size <- rep(1, length(types))
  node_size[types == 'variable'] <- 0.8
  node_size[types == 'data'] <- 0.5
  node_size[types == 'operation'] <- 0.3

  # get node labels
  node_labels <- vapply(x$dag$node_list, member, 'plotting_label()', FUN.VALUE = '')

  #add greta array names where available
  known_nodes <- vapply(x$visible_greta_arrays, member,
                        'node$unique_name', FUN.VALUE = '')
  known_idx <- match(known_nodes, names)
  node_labels[known_idx] <- paste(names(known_nodes),
                                  node_labels[known_idx],
                                  sep = '\n')

  # for the operation nodes, add the operation to the edges
  op_idx <- which(types == 'operation')
  op_names <- vapply(x$dag$node_list[op_idx],
                     member,
                     'operation_name',
                     FUN.VALUE = '')
  op_names <- gsub('`', '', op_names)

  ops <- rep('', length(types))
  ops[op_idx] <- op_names

  # get ops as tf operations
  edge_labels <- ops[to]

  # for distributions, put the parameter names on the edges
  distrib_to <- which(types == 'distribution')

  parameter_list <- lapply(x$dag$node_list[distrib_to], member, 'parameters')
  # parameter_names <- lapply(parameter_list, names)
  node_names <- lapply(parameter_list,
                       function (parameters) {
                         vapply(parameters, member, 'unique_name', FUN.VALUE = '')
                       })

  # for each distribution
  for (i in seq_along(node_names)) {

    from_idx <- match(node_names[[i]], names)
    to_idx <- match(names(node_names)[i], names)
    param_names <- names(node_names[[i]])

    # assign them
    for (j in seq_along(from_idx)) {
      idx <- from == from_idx[j] & to == to_idx
      edge_labels[idx] <- param_names[j]
    }

  }

  edge_style <- rep('solid', length(to))

  # put dashed line between target and distribution
  # for distributions, put the parameter names on the edges
  names <- names(x$dag$node_list)
  types <- x$dag$node_types
  distrib_idx <- which(types == 'distribution')

  target_names <- vapply(x$dag$node_list[distrib_idx], member, 'target$unique_name', FUN.VALUE = '')
  distribution_names <- names(target_names)
  distribution_idx <- match(distribution_names, names)
  target_idx <- match(target_names, names)

  # for each distribution
  for (i in seq_along(distribution_idx)) {

    idx <- which(from == target_idx[i] & to == distribution_idx[i])
    edge_style[idx] <- 'dashed'

  }


  # node options
  gr$nodes_df$type <- 'lower'
  gr$nodes_df$fontcolor <- 'black'
  gr$nodes_df$fontsize <- 14

  gr$nodes_df$shape <- node_shapes
  gr$nodes_df$fillcolor <- node_colours
  gr$nodes_df$width <- node_size
  gr$nodes_df$height <- node_size * 0.8
  gr$nodes_df$label <- node_labels

  # edge options
  gr$edges_df$color <- 'Gainsboro'
  gr$edges_df$fontname <- 'Helvetica'
  gr$edges_df$fontcolor <- 'gray'
  gr$edges_df$fontsize <- 11
  gr$edges_df$penwidth <- 3

  gr$edges_df$label <- edge_labels
  gr$edges_df$style <- edge_style

  # set the layout type
  gr$global_attrs$value[gr$global_attrs$attr == 'layout'] <- 'dot'

  DiagrammeR::render_graph(gr)

}

