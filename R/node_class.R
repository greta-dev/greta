# base node class
node <- R6Class(
  'node',
  public = list(

    type = 'undefined',
    unique_name = '',
    name = 'nameless',
    registered = FALSE,
    children = list(),
    parents = list(),
    .value = array(NA),
    .fixed_value = FALSE,
    dim = NA,
    distribution = NULL,

    initialize = function (dim = NULL, value = NULL) {

      if (is.null(dim))
        dim <- c(1, 1)

      # coerce dim to integer
      dim <- as.integer(dim)

      # store array (updates dim)
      if (is.null(value))
        value <- unknowns(dim = dim)

      self$value(value)
      self$register()
      self$unique_name <- capture.output(self$.__enclos_env__)

    },

    register = function () {

      # register this node in the global list, as soon as it is assigned a name
      .nodes <- options()$nodes
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
      options(nodes = .nodes)
      self$registered <- TRUE

    },

    add_child = function (node) {

      # if the node is already listed as a child, clone and re-register it to a
      # new name
      if (node$name %in% self$child_names()) {
        node <- node$clone()
        node$register()
      }

      # add to list of children
      self$children = c(self$children, node)
      node$add_parent(self)

    },

    remove_child = function (node) {

      # remove node from list of children
      rem_idx <- self$child_names() == node$name
      self$children <- self$children[-rem_idx]
      node$remove_parent(self)

    },


    add_parent = function (node) {

      # if the node is already listed as a parent, clone and re-register it to a
      # new name
      if (node$name %in% self$parent_names()) {
        node <- node$clone()
        node$register()
      }

      # add to list of children
      self$parents = c(self$parents, node)

    },

    remove_parent = function (node) {

      # remove node from list of children
      rem_idx <- self$parent_names() == node$name
      self$parents <- self$parents[-rem_idx]

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
    child_names = function (recursive = TRUE) {

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

      names

    },

    parent_names = function (recursive = TRUE) {

      parents <- self$parents

      if (length(parents) > 0) {

        names <- vapply(parents,
                        function(x) x$name,
                        '')

        if (recursive) {
          names <- c(names,
                     unlist(lapply(parents,
                                   function(x) x$parent_names(recursive = TRUE))))
        }

        # account for multiple nodes depending on the same nodes
        names <- unique(names)

      } else {
        # otherwise return own name (to make sure at least somethign is returned
        # on recursion)
        names <- self$node_name()
      }

      names

    },

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
    value = function (new_value = NULL, ...) {

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

    set_distribution = function (distribution) {

      # check it
      if (!inherits(distribution, 'distribution_node'))
        stop ('invalid distribution')

      # add it
      self$distribution <- distribution

    },

    # return a string describing this node, for use in print and summary etc.
    description = function () {

      text <- self$type

      if (!is.null(self$distribution)) {
        text <- paste(text,
                      'following a',
                      self$distribution$distribution_name,
                      'distribution')
      }

      text

    }


  ))

# generic to grab dimensions
dim.node <- function (x)
  x$dim

# is this object of class node
is.node <- function (x)
  inherits(x, 'node')

# coerce an object to a node
to_node <- function (x) {
  if (!is.node(x)) {
    if (is.numeric(x))
      x <- data_node$new(x)
    else if (is.greta_array(x))
      x <- x$node
    else
      stop ('cannot coerce object to a node')
  }
  x
}
