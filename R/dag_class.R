# create dag class
dag_class <- R6Class(
  'dag_class',
  public = list (

    node_list = list(),
    node_names = NA,
    node_types = NA,
    node_tf_names = NA,
    tf_environment = NA,
    defined = FALSE,
    target_nodes = NA,
    parameters_example = NA,

    # create a dag from some target nodes
    initialize = function (target_greta_arrays) {

      # build the dag
      self$build_dag(target_greta_arrays)

      # find the nodes we care about
      self$target_nodes <- lapply(target_greta_arrays, member, 'node')

      # stash the node names, types, and tf names
      self$node_names <- names(self$node_list)
      self$node_types <- vapply(self$node_list, member, 'type', FUN.VALUE = '')
      self$node_tf_names <- self$build_names()

      # set up the tf environment
      self$tf_environment <- new.env()

      # stash an example list to relist parameters
      self$parameters_example <- self$example_parameters(flat = FALSE)

    },

    # return list of nodes. If `names` is provided, return only those
    nodes = function (types = NULL) {
      nodes <- self$node_list
      if (!is.null(types))
        nodes <- nodes[which(self$node_types %in% types)]
      nodes
    },

    # get tf names for different node types
    get_tf_names = function (types = NULL) {
      names <- self$node_tf_names
      if (!is.null(types))
        names <- names[which(self$node_types %in% types)]
      names
    },

    tf_name = function (node) {
      self$node_tf_names[node$unique_name]
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

        # define and evaluate the command
        command <- sprintf('%s <- tf$reshape(tf$gradients(joint_density, %s), shape(-1))',
                           gradient_name,
                           free_name)
        eval(parse(text = command),
             envir = self$tf_environment)

      }

      # combine the gradients into one tensor
      gradient_names <- paste0(variable_tf_names, '_gradient')

      # define and evaluate the command
      command <- sprintf('gradients <- tf$concat(list(%s), 0L)',
                         paste(gradient_names, collapse = ','))

      eval(parse(text = command),
           envir = self$tf_environment)

    },

    # define tensor for overall log density and gradients
    define_joint_density = function () {

      # get names of densities for all distribution nodes
      density_names <- self$get_tf_names(types = 'distribution')

      # get TF density tensors for all distribution
      densities <- lapply(density_names,
                          function(x) get(x, envir = self$tf_environment))

      # reduce_sum them
      summed_densities <- lapply(densities, tf$reduce_sum)

      # remoe their names and sum them together
      names(summed_densities) <- NULL
      sum_total <- tf$add_n(summed_densities)

      # assign overall density to environment
      assign('joint_density', sum_total, envir = self$tf_environment)

    },

    # define tf graph in environment
    define_tf = function () {

      # check for unfixed discrete distributions
      bad_nodes <- vapply(self$nodes('distribution'),
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
      lapply(self$nodes(), function (x) x$define_tf(self))

      # define an overall log density and relevant gradients there
      self$define_joint_density()
      self$define_gradients()

      # start a session and initialise all variables
      self$run_tf(  sess <- tf$Session()  )
      self$run_tf(  sess$run(tf$global_variables_initializer())  )

    },

    # run a command in the environment in which the TF graph and session are defined
    run_tf = function (call)
      eval(substitute(call), envir = self$tf_environment),

    # return the expected free parameter format either in list or vector form
    example_parameters = function (flat = TRUE) {

      # get example parameter list for all non-fixed  parameters for the dag
      current_parameters <- self$all_values(types = 'variable')

      # optionally flatten them
      if (flat)
        current_parameters <- unlist_tf(current_parameters)

      current_parameters

    },

    send_parameters = function (parameters, flat = TRUE) {

      # convert parameters to a named list
      parameters <- relist_tf(parameters, self$parameters_example)

      # change node names to free versions
      names(parameters) <- paste0(names(parameters), '_free')

      # coerce to dict *in tf_environment* so that keys are linked to tensors
      assign('parameters', parameters, envir = self$tf_environment)

      # create a feed dict
      ex <- expression(parameter_dict <- do.call(dict, parameters))
      eval(ex,
           envir = self$tf_environment)

    },

    log_density = function() {

      ex <- expression(sess$run(joint_density, feed_dict = parameter_dict))
      eval(ex,
           envir = self$tf_environment)

    },

    # return a list of nodes connected to those in the target node list
    build_dag = function (greta_array_list) {

      target_node_list <- lapply(greta_array_list, member, "node")

      # loop through the target nodes, recursively registering them to this dag
      for (node in target_node_list) {
        node$register_family(self)
      }

    },

    # get values in all descendents as a named list, only for nodes of
    # the named type (if type != NULL), and if omit_fixed = TRUE, omit the
    # fixed values when reporting (ignored when setting)
    all_values = function (types = NULL, omit_fixed = TRUE) {

      # find all nodes of this type in the graph
      nodes <- self$nodes(types = types)
      names <- self$get_tf_names(types = types)

      if (omit_fixed) {
        fixed <- vapply(nodes, member, '.fixed_value', FUN.VALUE = TRUE)
        nodes <- nodes[which(!fixed)]
        names <- names[which(!fixed)]
      }

      # get all values in a list
      values <- lapply(nodes, member, 'value()')
      names(values) <- names

      values

    },

    # return the current values of the traced nodes, as a named vector
    trace_values = function () {

      command <- 'sess$run(%s, feed_dict = parameter_dict)'

      # evaluate the nodes corresponding to each of the target variables
      trace_list <- lapply(self$target_nodes,
                           function (node) {
                             eval(parse(text = sprintf(command, self$tf_name(node))),
                                  envir = self$tf_environment)
                           })

      # flatten and return
      unlist(trace_list)

    },

    # get gradient of joint density w.r.t. free states of all variable nodes
    gradients = function () {
      ex <- expression(sess$run(gradients, feed_dict = parameter_dict))
      eval(ex, envir = self$tf_environment)
    },

    build_names = function () {
      # given a vector of node types, create names for TF tensors, sequentially
      # within each type

      types <- self$node_types

      for (type in c('variable', 'data', 'operation', 'distribution')) {
        idx <- which(types == type)
        types[idx] <- paste(type, seq_along(idx), sep = '_')
      }

      self$node_tf_names <- types

    }


  )
)
