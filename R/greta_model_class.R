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
#'   [XLA JIT compilation](https://www.tensorflow.org/xla) to
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
  if (identical(target_greta_arrays, list())) {
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

  # get and check the types
  types <- dag$node_types

  # the user might pass greta arrays with groups of nodes that are unconnected
  # to one another. Need to check there are densities in each graph

  # so find the subgraph to which each node belongs
  graph_id <- dag$subgraph_membership()

  graphs <- unique(graph_id)
  n_graphs <- length(graphs)

  # separate messages to avoid the subgraphs issue for beginners
  if (n_graphs == 1) {
    density_message <- cli::format_error(
      c(
        "none of the {.cls greta_array}s in the model are associated with a \\
        probability density, so a model cannot be defined"
        )
    )
    variable_message <- cli::format_error(
      c(
        "none of the {.cls greta_array}s in the model are unknown, so a model \\
        cannot be defined"
        )
      )
  } else {
    density_message <- cli::format_error(
      c(
        "the model contains {n_graphs} disjoint graphs",
        "one or more of these sub-graphs does not contain any \\
        {.cls greta_array}s that are associated with a probability density, \\
        so a model cannot be defined"
      )
    )
    variable_message <- cli::format_error(
        c(
          "the model contains {n_graphs} disjoint graphs",
          "one or more of these sub-graphs does not contain any \\
          {.cls greta_array}s that are unknown, so a model cannot be defined"
        )
      )
  }

  for (graph in graphs) {
    types_sub <- types[graph_id == graph]

    # check they have a density among them
    if (!("distribution" %in% types_sub)) {
      stop(
        density_message,
        call. = FALSE
        )
    }

    # check they have a variable node among them
    if (!("variable" %in% types_sub)) {
      stop(
        variable_message,
        call. = FALSE
        )
    }
  }

  # check for unfixed discrete distributions
  distributions <- dag$node_list[dag$node_types == "distribution"]
  bad_nodes <- vapply(
    distributions,
    function(x) {
      valid_target <- is.null(x$target) ||
        inherits(x$target, "data_node")
      x$discrete && !valid_target
    },
    FALSE
  )

  if (any(bad_nodes)) {
    msg <- cli::format_error(
      "model contains a discrete random variable that doesn't have a fixed \\
      value, so inference cannot be carried out"
        )
    stop(
      msg,
      call. = FALSE
    )
  }

  # define the TF graph
  # dag$define_tf()

  # create the model object and add details
  model <- as.greta_model(dag)
  model$target_greta_arrays <- target_greta_arrays
  model$visible_greta_arrays <- all_greta_arrays(parent.frame())

  model
}

# register generic method to coerce objects to a greta model
as.greta_model <- function(x, ...) { # nolint
  UseMethod("as.greta_model", x)
}

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
  if (!is_DiagrammeR_installed()) {
    msg <- cli::format_error(
      c(
        "the {.pkg DiagrammeR} package must be installed to plot greta models",
        "install {.pkg DiagrammeR} with:",
        "{.code install.packages('DiagrammeR')}"
        )
      )
    stop(
      msg,
      call. = FALSE
    )
  }

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
  known_nodes <- vapply(visible_nodes,
    member,
    "unique_name",
    FUN.VALUE = ""
  )
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
    function(parameters) {
      vapply(parameters,
        member,
        "unique_name",
        FUN.VALUE = ""
      )
    }
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

  keep <- !vapply(targets, is.null, TRUE)
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
