# create dag class
dag_class <- R6Class(
  'dag_class',
  public = list (

    children = list(),
    tf_environment = NA,
    defined = FALSE,
    target_nodes = NA,
    parameters_example = NA,

    # create a dag from some target nodes
    initialize = function (target_nodes) {
      self$build_dag(target_nodes)
      self$tf_environment <- new.env()
      self$parameters_example <- self$example_parameters(flat = FALSE)
    },

    define_gradients = function () {

      # get names of free states for all non-fixed stochastic nodes
      stoch_names <- self$child_names(type = 'stochastic',
                                      omit_fixed = TRUE)

      # loop through them, defining the gradient of the joint density w.r.t. the
      # free state
      for (name in stoch_names) {

        # names of tensors
        free_name <- paste0(stoch_names, '_free')
        gradient_name <- paste0(stoch_names, '_gradient')

        # define and evaluate the command
        command <- sprintf('%s <- tf$reshape(tf$gradients(joint_density, %s), shape(-1))',
                           gradient_name,
                           free_name)
        eval(parse(text = command),
             envir = self$tf_environment)

      }

      # combine the gradients into one tensor
      gradient_names <- paste0(stoch_names, '_gradient')

      # define and evaluate the command
      command <- sprintf('gradients <- tf$concat(0L, list(%s))',
                         paste(gradient_names, collapse = ','))
      eval(parse(text = command),
           envir = self$tf_environment)




    },

    # define tensor for overall log density and gradients
    define_joint_density = function () {

      # get names of densities for all stochastic nodes
      stoch_names <- self$child_names(type = 'stochastic')
      density_names <- paste0(stoch_names, '_density')

      # get TF density tensors for all stochastic nodes
      densities <- lapply(density_names,
                          function(x) get(x, envir = self$tf_environment))

      # reduce_sum them
      summed_densities <- lapply(densities, tf$reduce_sum)

      # sum them together
      sum_total <- tf$add_n(summed_densities)

      # assign overall density to environment
      assign('joint_density', sum_total, envir = self$tf_environment)

    },

    # define tf graph in environment
    define_tf = function () {

      # define all nodes, node densities and free states in the environment
      lapply(self$children,
             function (x) x$define_tf(self$tf_environment))

      # define an overall log density and relevant gradients there
      self$define_joint_density()
      self$define_gradients()

      # start a session and initialise all variables
      self$run_tf(  sess <- tf$Session()  )
      self$run_tf(  sess$run(tf$initialize_all_variables())  )

    },

    # run a command in the environment in which the TF graph and session are defined
    run_tf = function (call)
      eval(substitute(call), envir = self$tf_environment),

    # return the expected parameter format either in list or vector form
    example_parameters = function (flat = TRUE) {

      # get example parameter list for all non-fixed  parameters for the dag
      current_parameters <- self$all_values(type = 'stochastic',
                                            omit_fixed = TRUE)

      # optionally flatten them
      if (flat)
        current_parameters <- unlist_tf(current_parameters)

      current_parameters

    },

    # check proposed model parameters 'parameters' as a named list (if flat =
    # FALSE), or a named vector (if flat = TRUE). If they are valid, return as a
    # named list.
    check_parameters = function (parameters, flat = TRUE) {

      # get example parameter list for all non-fixed parameters for the dag
      current_parameters <- self$all_values(type = 'stochastic',
                                            omit_fixed = TRUE)

      # unflatten the new parameters if required, and convert each element to an array
      if (flat)
        parameters <- relist_tf(parameters, current_parameters)

      # and check they match
      if (!length(parameters) == length(current_parameters))
        stop ('length of new parameters does not match dag')

      classes <- vapply(parameters, class, '')
      current_classes <- vapply(current_parameters, class, '')
      if (!all(classes == current_classes))
        stop ('classes of new parameters do not match dag')

      dims <- vapply(parameters, dim, 1)
      current_dims <- vapply(current_parameters, dim, 1)
      if (!all(dims == current_dims))
        stop ('dimensions of new parameters do not match dag')

      if (!all(names(parameters) == names(current_parameters)))
        stop ('names of new parameters do not match dag')

      # otherwise return as a list
      parameters

    },

    send_parameters = function (parameters, flat = TRUE) {

      # convert parameters to a named list
      parameters <- relist_tf(parameters, self$parameters_example)

      # change node names to free versions
      # names_new <- vapply(names(parameters), paste0, '_free', FUN.VALUE = '')
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
    build_dag = function (node_list) {

      # get all registered nodes, and their descendents
      all_nodes <- .nodes$nodes()
      all_plus_children <- lapply(all_nodes, function (x) c(x$name, x$child_names()))

      # find target nodes and remove from all_nodes those that don't contain the targets
      target_names <- vapply(node_list,
                             member,
                             'name',
                             FUN.VALUE = '')

      self$target_nodes <- target_names

      contain_targets <- vapply(all_plus_children,
                                function(x) any(target_names %in% x),
                                FALSE)

      # get the unique set
      dag_node_names <- unique(unlist(all_plus_children[which(contain_targets)]))
      idx <- match(dag_node_names, names(all_nodes))

      # get dependency graph and add as an attribute?

      # add nodes as children
      self$children <- all_nodes[idx]

    },

    child_names = function (recursive = TRUE,
                            type = NULL,
                            omit_fixed = FALSE) {

      children <- self$children

      if (length(children) > 0) {

        names <- vapply(children,
                        function(x) x$name,
                        '')

        if (recursive) {
          names <- c(names,
                     unlist(lapply(children,
                                   function(x) x$child_names(recursive = TRUE))))
        }

        # account for multiple nodes depending on the same nodes
        names <- unique(names)

      } else {
        # otherwise return own name (to make sure at least something is returned
        # on recursion)
        names <- self$node_name()
      }

      # optionally filter to a specific type
      if (!is.null(type) && is.character(type)) {

        named_nodes <- .nodes$nodes(names)
        types <- vapply(named_nodes, function(x) x$type, '')
        idx <- which(types == type)
        names <- names[idx]

      }

      # optionally omit those that have fixed values
      if (omit_fixed) {

        named_nodes <- .nodes$nodes(names)
        fixed <- vapply(named_nodes, function(x) x$.fixed_value, FUN.VALUE = TRUE)
        names <- names[which(!fixed)]

      }

      names

    },

    # get or set values in all descendents as a named list, only for nodes of
    # the named type (if type != NULL), and if omit_fixed = TRUE, omit the
    # fixed values when reporting (ignored when setting)
    all_values = function (new_values = NULL, type = NULL, omit_fixed = TRUE) {

      # find all nodes of this type in the graph
      node_names <- self$child_names(type = type)
      node_names <- c(self$name, node_names)
      nodes <- .nodes$nodes(node_names)

      if (omit_fixed) {
        fixed <- vapply(nodes, function(x) x$.fixed_value, FUN.VALUE = TRUE)
        nodes <- nodes[which(!fixed)]
        node_names <- node_names[which(!fixed)]
      }

      if (is.null(new_values)) {

        # get all values in a list
        values <- lapply(nodes, function(x) x$value())
        names(values) <- node_names
        return (values)

      } else {

        # or check the new values have the right dimension ()
        current_values <- self$all_values(type = type, omit_fixed = TRUE)
        current_shape <- vapply(current_values, length, 1)
        new_shape <- vapply(new_values, length, 1)

        if (!identical(current_shape, new_shape))
          stop ('new values have different shape to current values')

        # then assign them
        for (i in seq_along(nodes))
          nodes[[i]]$value(new_values[[i]])

      }

    },

    # return the current values of the traced nodes, as a named vector
    trace_values = function () {

      command <- 'sess$run(%s, feed_dict = parameter_dict)'

      # evaluate the nodes corresponding to each of the target variables
      trace_list <- lapply(self$target_nodes,
                           function (name) {
                             eval(parse(text = sprintf(command, name)),
                                  envir = self$tf_environment)
                           })

      # flatten and return
      unlist(trace_list)

    },

    # get gradient of joint density w.r.t. free states of all unfixed
    # stochastic nodes
    gradients = function () {
      ex <- expression(sess$run(gradients, feed_dict = parameter_dict))
      eval(ex,
           envir = self$tf_environment)
    }

  )
)
