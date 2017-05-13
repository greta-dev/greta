# create dag class
dag_class <- R6Class(
  'dag_class',
  public = list (

    node_list = list(),
    node_types = NA,
    node_tf_names = NA,
    tf_environment = NA,
    target_nodes = NA,
    parameters_example = NA,

    # create a dag from some target nodes
    initialize = function (target_greta_arrays) {

      # build the dag
      self$build_dag(target_greta_arrays)

      # find the nodes we care about
      self$target_nodes <- lapply(target_greta_arrays, member, 'node')

      # stash the node names, types, and tf names
      self$node_types <- vapply(self$node_list, member, 'type', FUN.VALUE = '')
      self$node_tf_names <- self$make_names()

      # set up the tf environment
      self$tf_environment <- new.env()

      # stash an example list to relist parameters
      self$parameters_example <- self$example_parameters(flat = FALSE)

    },

    # return a list of nodes connected to those in the target node list
    build_dag = function (greta_array_list) {

      target_node_list <- lapply(greta_array_list, member, "node")

      # loop through the target nodes, recursively registering them to this dag
      for (node in target_node_list) {
        node$register_family(self)
      }

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
    define_tf = function () {

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

      # define all nodes, node densities and free states in the environment
      lapply(self$node_list, function (x) x$define_tf(self))

      # define an overall log density and relevant gradients there
      self$define_joint_density()
      self$define_gradients()

      # start a session and initialise all variables
      with(self$tf_environment,
           {sess <- tf$Session(); sess$run(tf$global_variables_initializer())})

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

        gradient <- tf$gradients(self$tf_environment$joint_density,
                                 self$tf_environment[[free_name]])
        gradient_reshape <- tf$reshape(gradient, shape(-1))

        self$tf_environment[[gradient_name]] <- gradient_reshape

      }

      # combine the gradients into one tensor
      gradient_names <- paste0(variable_tf_names, '_gradient')

      gradient_list <- lapply(gradient_names,
                              get,
                              envir = self$tf_environment)

      self$tf_environment$gradients <- tf$concat(gradient_list, 0L)

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
      sum_total <- tf$add_n(summed_densities)

      # assign overall density to environment
      assign('joint_density', sum_total, envir = self$tf_environment)

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
      with(self$tf_environment,
           parameter_dict <- do.call(dict, parameters))

    },

    log_density = function() {

      with(self$tf_environment,
           sess$run(joint_density, feed_dict = parameter_dict))

    },

    # get gradient of joint density w.r.t. free states of all variable nodes
    gradients = function () {
      with(self$tf_environment,
           sess$run(gradients, feed_dict = parameter_dict))
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

    }

  )
)
