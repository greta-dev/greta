#' @importFrom reticulate py_set_attr
#' @importFrom tensorflow dict
#' @importFrom R6 R6Class

# create dag class
dag_class <- R6Class(
  "dag_class",
  public = list(

    node_list = list(),
    node_types = NA,
    node_tf_names = NA,
    tf_environment = NA,
    tf_graph = NA,
    target_nodes = NA,
    tf_float = NA,
    n_cores = 0L,
    compile = NA,
    adjacency_matrix = NULL,
    trace_names = NULL,

    # create a dag from some target nodes
    initialize = function(target_greta_arrays,
                          tf_float = "float32",
                          compile = FALSE) {

      # build the dag
      self$build_dag(target_greta_arrays)

      # find the nodes we care about
      self$target_nodes <- lapply(target_greta_arrays, get_node)

      # create an adjacency matrix
      self$build_adjacency_matrix()

      # set up the tf environment, with a graph
      self$new_tf_environment()

      # store the performance control info
      self$tf_float <- tf_float
      self$compile <- compile

    },

    new_tf_environment = function() {

      self$tf_environment <- new.env()
      self$tf_graph <- tf$Graph()
      self$tf_environment$data_list <- list()

    },

    # execute an expression on this dag's tensorflow graph, with the correct
    # float type
    on_graph = function(expr) {

      # temporarily pass float type info to options, so it can be accessed by
      # nodes on definition, without cluncky explicit passing
      old_float_type <- options()$greta_tf_float
      on.exit(options(greta_tf_float = old_float_type))
      options(greta_tf_float = self$tf_float)

      with(self$tf_graph$as_default(), expr)
    },

    # execute an expression in the tensorflow environment
    tf_run = function(expr, as_text = FALSE) {

      tfe <- self$tf_environment

      if (as_text)
        tfe$expr <- parse(text = expr)
      else
        tfe$expr <- substitute(expr)

      on.exit(rm("expr", envir = tfe))

      self$on_graph(with(tfe, eval(expr)))

    },

    # sess$run() an expression in the tensorflow environment, with the feed dict
    tf_sess_run = function(expr, as_text = FALSE) {

      if (!as_text)
        expr <- deparse(substitute(expr))

      expr <- paste0("sess$run(", expr, ", feed_dict = feed_dict)")

      self$tf_run(expr, as_text = TRUE)

    },

    # return a list of nodes connected to those in the target node list
    build_dag = function(greta_array_list) {

      target_node_list <- lapply(greta_array_list, get_node)

      # loop through the target nodes, recursively registering them to this dag
      for (node in target_node_list) {
        node$register_family(self)
      }

      # stash the node names, types, and tf names
      self$node_types <- vapply(self$node_list, node_type, FUN.VALUE = "")
      self$node_tf_names <- self$make_names()

    },

    # create human-readable names for TF tensors
    make_names = function() {

      types <- self$node_types

      for (type in c("variable", "data", "operation", "distribution")) {
        idx <- which(types == type)
        types[idx] <- paste(type, seq_along(idx), sep = "_")
      }

      self$node_tf_names <- types

    },

    # get the TF names for different node types
    get_tf_names = function(types = NULL) {
      names <- self$node_tf_names
      if (!is.null(types))
        names <- names[which(self$node_types %in% types)]
      names
    },

    # look up the TF name for a single node
    tf_name = function(node) {
      name <- self$node_tf_names[node$unique_name]
      if (length(name) == 0) {
        name <- ""
      }
      name
    },

    define_free_state = function(type = c("variable", "placeholder"),
                                 name = "free_state") {

      type <- match.arg(type)

      tfe <- self$tf_environment

      vals <- self$example_parameters(free = TRUE)
      vals <- unlist_tf(vals)

      if (type == "variable") {

        # tf$Variable seems to have trouble assigning values, if created with
        # numeric (rather than logical) NAs
        vals <- as.logical(vals)
        vals <- t(as.matrix(vals))

        self$on_graph(free_state <- tf$Variable(initial_value = vals,
                                                dtype = tf_float()))
      } else {

        shape <- shape(NULL, length(vals))
        self$on_graph(free_state <- tf$compat$v1$placeholder(dtype = tf_float(),
                                                             shape = shape))

      }

      assign(name,
             free_state,
             envir = tfe)

    },

    # define the body of the tensorflow graph in the environment env; without
    # defining the free_state, or the densities etc.
    define_tf_body = function(target_nodes = self$node_list) {

      tfe <- self$tf_environment

      # split up into separate free state variables and assign
      free_state <- get("free_state", envir = tfe)

      params <- self$example_parameters(free = TRUE)
      lengths <- vapply(params,
                        function(x) length(x),
                        FUN.VALUE = 1L)
      if (length(lengths) > 1) {
        args <- self$on_graph(tf$split(free_state, lengths, axis = 1L))
      } else {
        args <- list(free_state)
      }

      names <- paste0(names(params), "_free")
      for (i in seq_along(names))
        assign(names[i], args[[i]], envir = tfe)

      # define all nodes, node densities and free states in the environment, and
      # on the graph
      self$on_graph(lapply(target_nodes,
                           function(x) x$define_tf(self)))

      invisible(NULL)

    },

    # use core and compilation options to set up a session in this environment
    define_tf_session = function() {

      tfe <- self$tf_environment
      tfe$n_cores <- self$n_cores
      # Begin Exclude Linting
      self$tf_run(
        config <- tf$compat$v1$ConfigProto(inter_op_parallelism_threads = n_cores,
                                 intra_op_parallelism_threads = n_cores))

      if (self$compile) {
        self$tf_run(py_set_attr(config$graph_options$optimizer_options,
                                "global_jit_level",
                                tf$compat$v1$OptimizerOptions$ON_1))
      }
      # End Exclude Linting

      # start a session and initialise all variables
      self$tf_run(sess <- tf$compat$v1$Session(config = config))
      self$tf_run(sess$run(tf$compat$v1$global_variables_initializer()))

    },

    # define tf graph in environment
    define_tf = function() {

      # define the free state variable, rest of the graph, and the session
      self$define_free_state("placeholder", "free_state")
      self$define_tf_body()
      self$define_tf_session()

    },

    # define tensor for overall log density and gradients
    define_joint_density = function() {

      tfe <- self$tf_environment

      # get all distribution nodes that have a target
      distribution_nodes <- self$node_list[self$node_types == "distribution"]
      target_nodes <- lapply(distribution_nodes, member, "get_tf_target_node()")
      has_target <- !vapply(target_nodes, is.null, FUN.VALUE = TRUE)
      distribution_nodes <- distribution_nodes[has_target]
      target_nodes <- target_nodes[has_target]

      # get the densities, evaluated at these targets
      densities <- mapply(self$evaluate_density,
                          distribution_nodes,
                          target_nodes,
                          SIMPLIFY = FALSE)

      # reduce_sum each of them (skipping the batch dimension)
      self$on_graph(summed_densities <- lapply(densities, tf_sum, drop = TRUE))

      # sum them together
      names(summed_densities) <- NULL
      self$on_graph(joint_density <- tf$add_n(summed_densities))

      # assign overall density to environment
      assign("joint_density",
             joint_density,
             envir = self$tf_environment)

      # define adjusted joint density

      # get names of Jacobian adjustment tensors for all variable nodes
      adj_names <- paste0(self$get_tf_names(types = "variable"), "_adj")

      # get TF density tensors for all distribution
      adj <- lapply(adj_names, get, envir = self$tf_environment)

      # remove their names and sum them together (accounting for tfp bijectors
      # sometimes returning a scalar tensor)
      names(adj) <- NULL
      adj <- match_batches(adj)
      self$on_graph(total_adj <- tf$add_n(adj))

      # assign overall density to environment
      assign("joint_density_adj",
             joint_density + total_adj,
             envir = self$tf_environment)

    },

    # evaluate the (truncation-corrected) density of a tfp distribution on its
    # target tensor
    evaluate_density = function(distribution_node, target_node) {

      tfe <- self$tf_environment

      parameter_nodes <- distribution_node$parameters

      # get the tensorflow objects for these
      distrib_constructor <- self$get_tf_object(distribution_node)
      tf_target <- self$get_tf_object(target_node)
      tf_parameter_list <- lapply(parameter_nodes, self$get_tf_object)

      # execute the distribution constructor functions to return a tfp
      # distribution object
      tfp_distribution <- distrib_constructor(tf_parameter_list, dag = self)

      self$tf_evaluate_density(tfp_distribution,
                               tf_target,
                               truncation = distribution_node$truncation,
                               bounds = distribution_node$bounds)

    },

    tf_evaluate_density = function(tfp_distribution,
                                   tf_target,
                                   truncation = NULL,
                                   bounds = NULL) {

      # get the uncorrected log density
      ld <- tfp_distribution$log_prob(tf_target)

      # if required, calculate the log-adjustment to the truncation term of
      # the density function i.e. the density of a distribution, truncated
      # between a and b, is the non truncated density, divided by the integral
      # of the density function between the truncation bounds. This can be
      # calculated from the distribution's CDF
      if (!is.null(truncation)) {

        lower <- truncation[[1]]
        upper <- truncation[[2]]

        if (all(lower == bounds[1])) {

          # if only upper is constrained, just need the cdf at the upper
          offset <- tfp_distribution$log_cdf(fl(upper))

        } else if (all(upper == bounds[2])) {

          # if only lower is constrained, get the log of the integral above it
          offset <- tf$math$log(fl(1) - tfp_distribution$cdf(fl(lower)))

        } else {

          # if both are constrained, get the log of the integral between them
          offset <- tf$math$log(tfp_distribution$cdf(fl(upper)) -
                                  tfp_distribution$cdf(fl(lower)))

        }

        ld <- ld - offset

      }


      ld


    },

    # get the tf object in envir correpsonding to 'node'
    get_tf_object = function(node) {
      get(self$tf_name(node), envir = self$tf_environment)
    },

    # return a function to obtain the model log probability from a tensor for
    # the free state
    generate_log_prob_function = function(which = c("adjusted",
                                                    "unadjusted",
                                                    "both")) {

      which <- match.arg(which)

      function(free_state) {

        # temporarily define a new environment
        tfe_old <- self$tf_environment
        on.exit(self$tf_environment <- tfe_old)
        tfe <- self$tf_environment <- new.env()

        # copy the placeholders over here, so they aren't recreated
        data_names <- self$get_tf_names(types = "data")
        for (name in data_names)
          tfe[[name]] <- tfe_old[[name]]

        # put the free state in the environment, and build out the tf graph
        tfe$free_state <- free_state
        self$define_tf_body()

        # define the densities
        self$define_joint_density()

        objectives <- list(adjusted = tfe$joint_density_adj,
                           unadjusted = tfe$joint_density)

        # return either of the densities, or a list of both
        result <- switch(which,
                         adjusted = objectives$adjusted,
                         unadjusted = objectives$unadjusted,
                         both = objectives)

        result

      }

    },

    # return the expected parameter format either in free state vector form, or
    # list of transformed parameters
    example_parameters = function(free = TRUE) {

      # find all variable nodes in the graph
      nodes <- self$node_list[self$node_types == "variable"]
      names(nodes) <- self$get_tf_names(types = "variable")

      # get their values in either free of non-free form
      if (free) {
        parameters <- lapply(nodes, member, "value(free = TRUE)")
      } else {
        parameters <- lapply(nodes, member, "value()")
      }

      parameters

    },

    build_feed_dict = function(dict_list = list(),
                               data_list = self$tf_environment$data_list) {

      tfe <- self$tf_environment

      # put the list in the environment temporarily
      tfe$dict_list <- c(dict_list, data_list)
      on.exit(rm("dict_list", envir = tfe))

      # roll into a dict in the tf environment
      self$tf_run(feed_dict <- do.call(dict, dict_list))

    },

    send_parameters = function(parameters) {

      # reshape to a row vector if needed
      if (is.null(dim(parameters))) {
        parameters <- array(parameters, dim = c(1, length(parameters)))
      }

      # create a feed dict in the TF environment
      parameter_list <- list(free_state = parameters)

      self$build_feed_dict(parameter_list)

    },

    # get adjusted joint log density across the whole dag
    log_density = function() {

      res <- cleanly(self$tf_sess_run(joint_density_adj))

      if (inherits(res, "error"))
        res <- NA

      res

    },

    hessians = function() {

      tfe <- self$tf_environment
      nodes <- self$target_nodes

      # get names and dimensions of target greta arrays
      ga_names <- names(nodes)
      ga_dims <- lapply(nodes, member, "dim")

      # build the hessian tensors if needed
      if (!exists("hessian_list", envir = tfe)) {

        tf_names <- vapply(nodes, self$tf_name, FUN.VALUE = "")
        y <- tfe$joint_density
        xs <- lapply(tf_names, get, tfe)
        names(xs) <- NULL
        tfe$hessian_list <- self$on_graph(tf$hessians(y, xs))

      }

      # evaluate at the current free state and assign
      hessian_list <- self$tf_sess_run(hessian_list)

      # reshape from tensor to R dimensions
      dims <- lapply(ga_dims, hessian_dims)
      hessian_list <- mapply(array, hessian_list, dims, SIMPLIFY = FALSE)

      # assign names and return
      names(hessian_list) <- ga_names
      hessian_list

    },

    trace_values_batch = function(free_state_batch) {

      # update the parameters & build the feed dict
      self$send_parameters(free_state_batch)

      tfe <- self$tf_environment

      target_tf_names <- lapply(self$target_nodes,
                                self$tf_name)

      target_tensors <- lapply(target_tf_names,
                               get,
                               envir = tfe)

      # evaluate them in the tensorflow environment
      trace_list <- tfe$sess$run(target_tensors,
                                 feed_dict = tfe$feed_dict)

      trace_list

    },

    # return the current values of the traced nodes, as a named vector
    trace_values = function(free_state,
                            flatten = TRUE,
                            trace_batch_size = Inf) {

      # get the number of samples to trace
      n_samples <- nrow(free_state)
      indices <- seq_len(n_samples)
      splits <- split(indices, (indices - 1) %/% trace_batch_size)
      names(splits) <- NULL

      # split the free state up into batches
      get_rows <- function(rows, x) x[rows, , drop = FALSE]
      free_state_batches <- lapply(splits, get_rows, free_state)

      # loop through them
      trace_list_batches <- lapply(free_state_batches, self$trace_values_batch)

      # loop through each of the elements in the lists, and stack them
      stack_elements <- function(name, list) {
        elems <- lapply(trace_list_batches, `[[`, name)
        do.call(abind::abind, c(elems, list(along = 1)))
      }
      elements <- seq_along(trace_list_batches[[1]])
      trace_list <- lapply(elements, stack_elements, trace_list_batches)
      names(trace_list) <- names(trace_list_batches[[1]])

      # if they are flattened, e.g. for MCMC tracing
      if (flatten) {
        # loop through elements flattening these arrays to vectors and giving
        # the elements better names
        trace_list_flat <- lapply(seq_along(trace_list),
                                  flatten_trace,
                                  trace_list)

        out <- do.call(cbind, trace_list_flat)
        self$trace_names <- colnames(out)

      } else {

        # prepare for return to R
        trace_list <- lapply(trace_list, drop_first_dim)
        trace_list <- lapply(trace_list, drop_column_dim)

        out <- trace_list

      }


      out

    },

    # for all the nodes in this dag, return a vector of membership to sub-graphs
    subgraph_membership = function() {

      # convert adjacency matrix into absolute connectedness matrix using matrix
      # powers. Inspired by Method 2 here:
      # http://raphael.candelier.fr/?blog=Adj2cluster

      # convert adjacency to a symmetric, logical matrix
      adj <- self$adjacency_matrix
      sym <- (adj + t(adj)) > 0

      # loop through to build a block diagonal matrix of connected components
      # (usually only takes a few iterations)
      maxit <- 1000
      it <- 0
      p <- r <- sym
      while (it < maxit) {
        p <- p %*% sym
        t <- (r + p) > 0
        if (any(t != r)) {
          r <- t
          it <- it + 1
        } else {
          break ()
        }
      }

      # check we didn't time out
      if (it == maxit) {
        stop("could not determine the number of independent models ",
             "in a reasonable amount of time",
             call. = FALSE)
      }

      # find the cluster IDs
      n <- nrow(r)
      neighbours <- lapply(seq_len(n), function(i) which(r[i, ]))
      cluster_names <- vapply(neighbours, paste, collapse = "_", FUN.VALUE = "")
      cluster_id <- match(cluster_names, unique(cluster_names))

      # name them
      names(cluster_id) <- rownames(adj)
      cluster_id

    },

    build_adjacency_matrix = function() {

      # make dag matrix
      n_node <- length(self$node_list)
      node_names <- names(self$node_list)
      node_types <- self$node_types
      dag_mat <- matrix(0, nrow = n_node, ncol = n_node)
      rownames(dag_mat) <- colnames(dag_mat) <- node_names

      children <- lapply(self$node_list,
                        member,
                        "child_names()")
      parents <- lapply(self$node_list,
                         member,
                         "parent_names(recursive = FALSE)")

      # for distribution nodes, remove target nodes from parents, and put them
      # in children to send the arrow in the opposite direction when plotting
      distribs <- which(node_types == "distribution")
      for (i in distribs) {

        own_name <- node_names[i]
        target_name <- self$node_list[[i]]$target$unique_name

        if (!is.null(target_name)) {

          # switch the target from child to parent of the distribution
          parents[[i]] <- parents[[i]][parents[[i]] != target_name]
          children[[i]] <- c(children[[i]], target_name)

          # switch the distribution from parent to child of the target
          idx <- match(target_name, node_names)
          children[[idx]] <- children[[idx]][children[[idx]] != own_name]
          parents[[idx]] <- c(parents[[idx]], own_name)

        }

      }

      # parents in the lower left, children in the upper right
      for (i in seq_len(n_node)) {
        dag_mat[i, children[[i]]] <- 1
        dag_mat[parents[[i]], i] <- 1
      }

      self$adjacency_matrix <- dag_mat

    }

  )
)
