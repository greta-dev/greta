#' @importFrom reticulate py_set_attr

# create dag class
dag_class <- R6Class(
  'dag_class',
  public = list (

    node_list = list(),
    node_types = NA,
    node_tf_names = NA,
    tf_environment = NA,
    tf_graph = NA,
    target_nodes = NA,
    parameters_example = NA,
    tf_float = NA,
    n_cores = NA,
    compile = NA,

    # create a dag from some target nodes
    initialize = function (target_greta_arrays,
                           tf_float = tf$float32,
                           n_cores = 2L,
                           compile = FALSE) {

      # build the dag
      self$build_dag(target_greta_arrays)

      # find the nodes we care about
      self$target_nodes <- lapply(target_greta_arrays, member, 'node')

      # set up the tf environment, with a graph
      self$tf_environment <- new.env()
      self$tf_graph <- tf$Graph()

      # stash an example list to relist parameters
      self$parameters_example <- self$example_parameters(flat = FALSE)

      # store the performance control info
      self$tf_float <- tf_float
      self$n_cores <- n_cores
      self$compile <- compile

    },

    # execute an expression on this dag's tensorflow graph
    on_graph = function (expr) {
      with(self$tf_graph$as_default(), expr)
    },

    # execute an exporession in the tensorflow environment
    tf_run = function (expr) {
      self$tf_environment$expr <- substitute(expr)
      self$on_graph(with(self$tf_environment, eval(expr)))
    },

    # return a list of nodes connected to those in the target node list
    build_dag = function (greta_array_list) {

      target_node_list <- lapply(greta_array_list, member, "node")

      # loop through the target nodes, recursively registering them to this dag
      for (node in target_node_list) {
        node$register_family(self)
      }

      # stash the node names, types, and tf names
      self$node_types <- vapply(self$node_list, node_type, FUN.VALUE = '')
      self$node_tf_names <- self$make_names()

    },

    make_names = function () {
      # given a vector of node types, create names for TF tensors, sequentially
      # within each type

      types <- self$node_types

      for (type in c('variable', 'data', 'operation', 'distribution')) {
        idx <- which(types == type)
        types[idx] <- paste(type, seq_along(idx), sep = '_')
      }

      self$node_tf_names <- types

    },

    # get the TF names for different node types
    get_tf_names = function (types = NULL) {
      names <- self$node_tf_names
      if (!is.null(types))
        names <- names[which(self$node_types %in% types)]
      names
    },

    # look up the TF name for a singe node
    tf_name = function (node) {
      self$node_tf_names[node$unique_name]
    },

    # define tf graph in environment
    define_tf = function (log_density = TRUE, gradients = TRUE) {

      # temporarily pass float type info to options, so it can be accessed by
      # nodes on definition, without cluncky explicit passing
      old_float_type <- options()$greta_tf_float
      on.exit(options(greta_tf_float = old_float_type))
      options(greta_tf_float = self$tf_float)

      # check for unfixed discrete distributions
      distributions <- self$node_list[self$node_types == 'distribution']
      bad_nodes <- vapply(distributions,
                          function(x) {
                            x$discrete && !inherits(x$target, 'data_node')
                          },
                          FALSE)

      if (any(bad_nodes)) {
        stop ("model contains a discrete random variable that doesn't have a ",
              "fixed value, so cannot be sampled from",
              call. = FALSE)
      }

      # define all nodes, node densities and free states in the environment, and
      # on the graph
      self$on_graph(lapply(self$node_list,
                      function (x) x$define_tf(self)))


      # define an overall log density and gradients, plus adjusted versions
      if (log_density)
        self$on_graph(self$define_joint_density())

      if (gradients)
        self$on_graph(self$define_gradients())

      # use core and compilation options to create a config object
      self$tf_environment$n_cores <- self$n_cores
      self$tf_run(config <- tf$ConfigProto(inter_op_parallelism_threads = n_cores,
                                           intra_op_parallelism_threads = n_cores))

      if (self$compile) {
        self$tf_run(py_set_attr(config$graph_options$optimizer_options,
                                 'global_jit_level',
                                 tf$OptimizerOptions$ON_1))
      }

      # start a session and initialise all variables
      self$tf_run(sess <- tf$Session(config = config))
      self$tf_run(sess$run(tf$global_variables_initializer()))

    },

    # define tensor for overall log density and gradients
    define_joint_density = function () {

      # get names of densities for all distribution nodes
      density_names <- self$get_tf_names(types = 'distribution')

      # get TF density tensors for all distribution
      densities <- lapply(density_names, get, envir = self$tf_environment)

      # reduce_sum them
      summed_densities <- lapply(densities, tf$reduce_sum)

      # remove their names and sum them together
      names(summed_densities) <- NULL
      joint_density <- tf$add_n(summed_densities)

      # assign overall density to environment
      assign('joint_density', joint_density, envir = self$tf_environment)

      # define adjusted joint density

      # get names of adjustment tensors for all variable nodes
      adj_names <- paste0(self$get_tf_names(types = 'variable'), '_adj')

      # get TF density tensors for all distribution
      adj <- lapply(adj_names, get, envir = self$tf_environment)

      # remove their names and sum them together
      names(adj) <- NULL
      total_adj <- tf$add_n(adj)

      # assign overall density to environment
      assign('joint_density_adj', joint_density + total_adj, envir = self$tf_environment)

    },

    define_gradients = function () {

      # get names of free states for all variable nodes
      variable_tf_names <- self$get_tf_names(types = 'variable')

      # loop through them, defining the gradient of the joint density w.r.t. the
      # free state
      for (name in variable_tf_names) {

        # names of tensors
        free_name <- paste0(name, '_free')
        gradient_name <- paste0(name, '_gradient')
        gradient_adj_name <- paste0(name, '_gradient_adj')

        # raw gradients
        gradient <- tf$gradients(self$tf_environment$joint_density,
                                 self$tf_environment[[free_name]])
        gradient_reshape <- tf$reshape(gradient, shape(-1))
        self$tf_environment[[gradient_name]] <- gradient_reshape

        # adjusted gradients
        gradient_adj <- tf$gradients(self$tf_environment$joint_density_adj,
                                 self$tf_environment[[free_name]])
        gradient_adj_reshape <- tf$reshape(gradient_adj, shape(-1))
        self$tf_environment[[gradient_adj_name]] <- gradient_adj_reshape

      }

      # combine the gradients into one tensor
      gradient_names <- paste0(variable_tf_names, '_gradient')
      gradient_list <- lapply(gradient_names,
                              get,
                              envir = self$tf_environment)
      self$tf_environment$gradients <- tf$concat(gradient_list, 0L)

      # same for adjusted gradients
      gradient_adj_names <- paste0(variable_tf_names, '_gradient_adj')
      gradient_adj_list <- lapply(gradient_adj_names,
                                  get,
                                  envir = self$tf_environment)

      self$tf_environment$gradients_adj <- tf$concat(gradient_adj_list, 0L)

    },

    # return the expected free parameter format either in list or vector form
    example_parameters = function (flat = TRUE) {

      # find all variable nodes in the graph and get their values
      nodes <- self$node_list[self$node_types == 'variable']
      names(nodes) <- self$get_tf_names(types = 'variable')
      current_parameters <- lapply(nodes, member, 'value()')

      # optionally flatten them
      if (flat)
        current_parameters <- unlist_tf(current_parameters)

      current_parameters

    },

    send_parameters = function (parameters, flat = TRUE) {

      # convert parameters to a named list and change TF names to free versions
      parameters <- relist_tf(parameters, self$parameters_example)
      names(parameters) <- paste0(names(parameters), '_free')

      # create a feed dict in the TF environment
      self$tf_environment$parameters <- parameters
      self$tf_run(parameter_dict <- do.call(dict, parameters))

    },

    # get log density and gradient of joint density w.r.t. free states of all
    # variable nodes, with or without applying the jacobian adjustment
    log_density = function(adjusted = TRUE) {

      cleanly(self$tf_run(sess$run(joint_density_adj,
                                   feed_dict = parameter_dict)))

    },

    gradients = function (adjusted = TRUE) {

      cleanly(self$tf_run(sess$run(gradients_adj,
                                   feed_dict = parameter_dict)))

    },

    # return the current values of the traced nodes, as a named vector
    trace_values = function () {

      target_tf_names <- lapply(self$target_nodes,
                                self$tf_name)

      target_tensors <- lapply(target_tf_names,
                               get,
                               envir = self$tf_environment)

      # evaluate them in the tensorflow environment
      trace_list <- lapply(target_tensors,
                           self$tf_environment$sess$run,
                           feed_dict = self$tf_environment$parameter_dict)

      # flatten and return
      unlist(trace_list)

    },

    # for all the nodes in this dag, return a vector of membership to sub-graphs
    subgraph_membership = function () {

      neighbour_list <- self$find_node_neighbours()

      n_node <- length(neighbour_list)
      no_of_clusters <- 1
      registered <- rep(FALSE, n_node)
      membership <- rep(NA, n_node)

      # loop through all nodes
      for (node in seq_len(n_node)) {

        # if they haven't been registered
        if (!registered[node]) {

          # add this one to the register, with cluster membership
          registered[node] <- TRUE
          membership[node] <- no_of_clusters

          # get this node's neighbours (numeric vector, indexing nodes)
          neighbours <- neighbour_list[[node]]

          # loop through the neighbours
          for (neighbour in neighbours) {
            if (!registered[neighbour]) {
              registered[neighbour] <- TRUE;
              membership[neighbour] <- no_of_clusters
            }
          }

          # increment cluster ID
          no_of_clusters <- no_of_clusters + 1

        }
      }

      names(membership) <- names(neighbour_list)
      membership

    },

    find_node_neighbours = function () {

      # loop through each node in this dag, finding all members of its sub-graph
      sub_dags <- lapply(self$node_list,
                         function (x) dag_class$new(list(as.greta_array(x))))
      sub_node_lists <- lapply(sub_dags, member, 'node_list')

      # get their names
      names <- lapply(sub_node_lists, function (node_list) {
        vapply(node_list, member, 'unique_name', FUN.VALUE = '')})

      # convert these to indices against this dag's node list
      original_names <- names(self$node_list)
      lapply(names, match, original_names)

    },

    adjacency_matrix = function () {

      # make dag matrix
      n_node <- length(self$node_list)
      node_names <- names(self$node_list)
      node_types <- self$node_types
      dag_mat <- matrix(0, nrow = n_node, ncol = n_node)
      rownames(dag_mat) <- colnames(dag_mat) <- node_names

      parents <- lapply(self$node_list, member, 'parent_names(recursive = FALSE)')
      children <- lapply(self$node_list, member, 'child_names(recursive = FALSE)')

      # for distribution nodes, remove target nodes from children, and put them
      # in parents to send the arrow in the opposite direction when plotting
      distribs <- which(node_types == 'distribution')
      for (i in distribs) {

        own_name <- node_names[i]
        target_name <- self$node_list[[i]]$target$unique_name

        # switch the target from child to parent of the distribution
        children[[i]] <- children[[i]][children[[i]] != target_name]
        parents[[i]] <- c(parents[[i]], target_name)

        # switch the distribution from parent to child of the target
        idx <- match(target_name, node_names)
        parents[[idx]] <- parents[[idx]][parents[[idx]] != own_name]
        children[[idx]] <- c(children[[idx]], own_name)

      }

      # children in the lower left, parents in the upper right
      for (i in seq_len(n_node)) {
        dag_mat[i, parents[[i]]] <- 1
        dag_mat[children[[i]], i] <- 1
      }

      dag_mat

    }

  )
)
