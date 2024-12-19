# greta_model objects

#' @name model
#' @title greta model objects
#' @description Create a `greta_model` object representing a statistical
#'   model (using `model`), and plot a graphical representation of the
#'   model. Statistical inference can be performed on `greta_model` objects
#'   with [mcmc()]
NULL

#' @rdname model
#' @export
#'
#' @param \dots for `model`: `greta_array` objects to be tracked by
#'   the model (i.e. those for which samples will be retained during mcmc). If
#'   not provided, all of the non-data `greta_array` objects defined in the
#'   calling environment will be tracked. For `print` and
#'   `plot`:further arguments passed to or from other methods (currently
#'   ignored).
#'
#' @param precision the floating point precision to use when evaluating this
#'   model. Switching from `"double"` (the default) to `"single"` may
#'   decrease the computation time but increase the risk of numerical
#'   instability during sampling.
#'
#' @param compile whether to apply
#'   [XLA JIT compilation](https://openxla.org/xla) to
#'   the TensorFlow graph representing the model. This may slow down model
#'   definition, and speed up model evaluation.
#'
#' @details `model()` takes greta arrays as arguments, and defines a
#'   statistical model by finding all of the other greta arrays on which they
#'   depend, or which depend on them. Further arguments to `model` can be
#'   used to configure the TensorFlow graph representing the model, to tweak
#'   performance.
#'
#' @return `model` - a `greta_model` object.
#'
#' @examples
#' \dontrun{
#'
#' # define a simple model
#' mu <- variable()
#' sigma <- normal(0, 3, truncation = c(0, Inf))
#' x <- rnorm(10)
#' distribution(x) <- normal(mu, sigma)
#'
#' m <- model(mu, sigma)
#'
#' plot(m)
#' }
model <- function(...,
                  precision = c("double", "single"),
                  compile = TRUE) {
  check_tf_version("error")

  # get the floating point precision
  # TODO
  # what does it choose as default if both double and single are listed
  # as default?
  tf_float <- switch(match.arg(precision),
    double = "float64",
    single = "float32"
  )

  # nodes required
  target_greta_arrays <- list(...)

  # if no arrays were specified, find all of the non-data arrays
  no_arrays_specified <- identical(target_greta_arrays, list())
  if (no_arrays_specified) {
    target_greta_arrays <- all_greta_arrays(parent.frame(),
      include_data = FALSE
    )
  } else {

    # otherwise, find variable names for the provided nodes
    names <- substitute(list(...))[-1]
    names <- vapply(names, deparse, "")
    names(target_greta_arrays) <- names
  }

  target_greta_arrays <- check_greta_arrays(target_greta_arrays, "model")

  # get the dag containing the target nodes
  # TF1/2 check
  # I don't think we need to use the `compile` flag in TF2 anymore
  # Well, it will be passed onto the tf_function creation step
  dag <- dag_class$new(target_greta_arrays,
    tf_float = tf_float,
    compile = compile
  )

  # the user might pass greta arrays with groups of nodes that are unconnected
  # to one another. Need to check there are densities in each graph
  check_subgraphs(dag)
  check_unfixed_discrete_distributions(dag)

  # define the TF graph
  # dag$define_tf()

  # create the model object and add details
  model <- as.greta_model(dag)
  model$target_greta_arrays <- target_greta_arrays
  model$visible_greta_arrays <- all_greta_arrays(parent.frame())

  model
}

# register generic method to coerce objects to a greta model
#' @title Convert object to a "greta_model" object
#' @param x object to convert to greta model
#' @param ... extra arguments - not used.
#'
#' @export
as.greta_model <- function(x, ...) { # nolint
  UseMethod("as.greta_model", x)
}

#' @export
as.greta_model.dag_class <- function(x, ...) { # nolint
  ans <- list(dag = x)
  class(ans) <- "greta_model"
  ans
}

#' @rdname model
#' @param x a `greta_model` object
#' @export
print.greta_model <- function(x, ...) {
  cat("greta model")
}

#' @rdname model
#' @param y unused default argument
#' @param colour base colour used for plotting. Defaults to `greta` colours
#'   in violet.
#'
#' @details The plot method produces a visual representation of the defined
#'   model. It uses the `DiagrammeR` package, which must be installed
#'   first. Here's a key to the plots:
#'   \if{html}{\figure{plotlegend.png}{options: width="100\%"}}
#'   \if{latex}{\figure{plotlegend.pdf}{options: width=7cm}}
#'
#' @return `plot` - a [DiagrammeR::grViz()]
#'   object, with the
#'   [`DiagrammeR::dgr_graph()`][DiagrammeR::create_graph] object used to
#'   create it as an attribute `"dgr_graph"`.
#'
#' @export
plot.greta_model <- function(x,
                             y,
                             colour = "#996bc7",
                             ...) {
  check_diagrammer_installed()

  # set up graph
  dag_mat <- x$dag$adjacency_matrix

  gr <- DiagrammeR::from_adj_matrix(dag_mat,
    mode = "directed",
    use_diag = FALSE
  )

  n_nodes <- nrow(gr$nodes_df)

  names <- names(x$dag$node_list)
  types <- x$dag$node_types
  to <- gr$edges_df$to
  from <- gr$edges_df$from

  node_shapes <- rep("square", n_nodes)
  node_shapes[types == "variable"] <- "circle"
  node_shapes[types == "distribution"] <- "diamond"
  node_shapes[types == "operation"] <- "circle"

  node_edge_colours <- rep(greta_col("lighter", colour), n_nodes)
  node_edge_colours[types == "distribution"] <- greta_col("light", colour)
  node_edge_colours[types == "operation"] <- "lightgray"

  node_colours <- rep(greta_col("super_light", colour), n_nodes)
  node_colours[types == "distribution"] <- greta_col("lighter", colour)
  node_colours[types == "operation"] <- "lightgray"
  node_colours[types == "data"] <- "white"

  node_size <- rep(1, length(types))
  node_size[types == "variable"] <- 0.6
  node_size[types == "data"] <- 0.5
  node_size[types == "operation"] <- 0.2

  # get node labels
  node_labels <- vapply(x$dag$node_list,
    member,
    "plotting_label()",
    FUN.VALUE = ""
  )

  # add greta array names where available
  visible_nodes <- lapply(x$visible_greta_arrays, get_node)
  known_nodes <- extract_unique_names(visible_nodes)

  known_nodes <- known_nodes[known_nodes %in% names]
  known_idx <- match(known_nodes, names)
  node_labels[known_idx] <- paste(names(known_nodes),
    node_labels[known_idx],
    sep = "\n"
  )

  # for the operation nodes, add the operation to the edges
  op_idx <- which(types == "operation")
  op_names <- vapply(x$dag$node_list[op_idx],
    member,
    "operation_name",
    FUN.VALUE = ""
  )
  op_names <- gsub("`", "", op_names)

  ops <- rep("", length(types))
  ops[op_idx] <- op_names

  # get ops as tf operations
  edge_labels <- ops[to]

  # for distributions, put the parameter names on the edges
  distrib_to <- which(types == "distribution")

  parameter_list <- lapply(
    x$dag$node_list[distrib_to],
    member,
    "parameters"
  )

  node_names <- lapply(
    parameter_list,
    extract_unique_names
  )

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

  edge_style <- rep("solid", length(to))

  # put dashed line between target and distribution
  # for distributions, put the parameter names on the edges
  names <- names(x$dag$node_list)
  types <- x$dag$node_types
  distrib_idx <- which(types == "distribution")

  # find those with targets
  targets <- lapply(
    x$dag$node_list[distrib_idx],
    member,
    "target"
  )

  keep <- !are_null(targets)
  distrib_idx <- distrib_idx[keep]


  target_names <- vapply(x$dag$node_list[distrib_idx],
    member,
    "target$unique_name",
    FUN.VALUE = ""
  )
  distribution_names <- names(target_names)
  distribution_idx <- match(distribution_names, names)
  target_idx <- match(target_names, names)

  # for each distribution
  for (i in seq_along(distribution_idx)) {
    idx <- which(to == target_idx[i] & from == distribution_idx[i])
    edge_style[idx] <- "dashed"
  }

  # node options
  gr$nodes_df$type <- "lower"
  gr$nodes_df$fontcolor <- greta_col("dark", colour)
  gr$nodes_df$fontsize <- 12
  gr$nodes_df$penwidth <- 2

  gr$nodes_df$shape <- node_shapes
  gr$nodes_df$color <- node_edge_colours
  gr$nodes_df$fillcolor <- node_colours
  gr$nodes_df$width <- node_size
  gr$nodes_df$height <- node_size * 0.8
  gr$nodes_df$label <- node_labels

  # edge options
  gr$edges_df$color <- "Gainsboro"
  gr$edges_df$fontname <- "Helvetica"
  gr$edges_df$fontcolor <- "gray"
  gr$edges_df$fontsize <- 11
  gr$edges_df$penwidth <- 3

  gr$edges_df$label <- edge_labels
  gr$edges_df$style <- edge_style

  # set the layout type
  gr$global_attrs$value[gr$global_attrs$attr == "layout"] <- "dot"
  # make it horizontal
  gr$global_attrs <- rbind(
    gr$global_attrs,
    data.frame(
      attr = "rankdir",
      value = "LR",
      attr_type = "graph"
    )
  )


  widget <- DiagrammeR::render_graph(gr)
  attr(widget, "dgr_graph") <- gr
  widget
}
