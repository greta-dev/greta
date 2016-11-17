# base node class
node <- R6Class(
  'node',
  public = list(

    type = 'undefined',
    name = 'nameless',
    registered = FALSE,
    children = list(),
    .value = array(NA),
    .fixed_value = FALSE,
    dim = NA,

    register = function () {
      # register this node in the global list, as soon as it is assigned a name
      name <- 'node'
      current_nodes <- .nodes$nodes()
      node_names <- names(current_nodes)
      if (name %in% node_names) {
        # find a unique name for it (adding numbers to the end)
        good_name <- FALSE
        counter <- 1
        while (!good_name) {
          counter <- counter + 1
          proposed_name <- paste0(name, counter)
          if (!proposed_name %in% node_names)
            good_name <- TRUE
        }
        name <- proposed_name
      }

      # assign name and add to register
      self$node_name(name)
      current_nodes[[name]] <- self
      .nodes$node_list <- current_nodes
      self$registered <- TRUE

    },

    add_child = function (node) {

      # # register the node if needed
      # if (!node$registered)
      #   node$register()

      # add to list of children
      self$children = c(self$children, node)
    },

    # get or set the name of this node
    node_name = function (name = NULL) {
      if (!is.null(name))
        self$name <- name
      self$name
    },

    # return the names of all child nodes, and if recursive = TRUE, all nodes
    # lower in this graph. If type is a character, only nodes with that type
    # (from the type public object)  will  be listed
    child_names = function (recursive = TRUE,
                            type = NULL) {

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
        # otherwise return own name (to make sure at least somethign is returned
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

      names

    },

    as_node = function (object) {

      # if it isn't a node already, try to make it a constant node
      if (!is_node(object))
        object <- constant_node$new(object)

      object

    },

    # define objects on tensorflow graph in environment env
    tf = function (env)
      stop ('no method to evaluate this node in TensorFlow'),

    # define this and all descendent objects on tensorflow graph in environment env
    define_tf = function (env) {

      # if defined already, skip
      if (!self$name %in% ls(env)) {

        # make sure children are defined
        children_defined <- vapply(self$children,
                                   function(x) x$name %in% (ls(env)),
                                   FUN.VALUE = FALSE)

        if (any(!children_defined)) {
          lapply(self$children[which(!children_defined)],
                 function(x) x$define_tf(env))
        }

        # then define self
        self$tf(env)

      }

    },

    # get or set this nodes' current value
    value = function (new_value = NULL) {

      if (is.null(new_value)) {

        self$.value

      } else {

        if (self$.fixed_value)
          stop ('this node has a fixed value; it cannot be reset')

        # get the dimension of the new value
        dim <- dim(new_value)

        # coerce 1D arrays to column vectors
        if (length(dim) == 1)
          dim <- c(dim, 1L)

        # update value and store
        dim(new_value) <- dim
        self$dim <- dim

        self$.value <- new_value
      }
    },

    # log density function for just this node
    log_density_function = function (val)
      stop ('log density function not implemented for this node'),

    # evaluate this node's density, given it's value
    log_density = function ()
      self$log_density_function(self$value()),

    # basic print method
    print = function() {
      msg <- sprintf('%s node',
                     self$type)
      cat(msg)
    }

  ))
