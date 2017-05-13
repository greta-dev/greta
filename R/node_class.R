# base node class
node <- R6Class(
  'node',
  public = list(
    unique_name = '',
    children = list(),
    parents = list(),
    .value = array(NA),
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
      self$get_unique_name()

    },

    register = function (dag) {
      if (! (self$unique_name %in% names(dag$node_list)))
        dag$node_list[[self$unique_name]] <- self
    },

    # recursively register self and family
    register_family = function (dag) {

      if (! (self$unique_name %in% names(dag$node_list))) {

        # add self to list
        self$register(dag)

        # find my immediate family (not including self)
        family <- c(self$parents, self$children)

        # get and assign their names
        family_names <- vapply(family, member, 'unique_name', FUN.VALUE = '')
        names(family) <- family_names

        # find the unregistered ones
        unregistered_idx <- which(! (family_names %in% names(dag$node_list)))
        unregistered <- family[unregistered_idx]

        # add them to the node list
        for (relative in unregistered)
          relative$register_family(dag)

      }

    },

    add_child = function (node) {

      # if the node is already listed as a child, clone and re-register it to a
      # new name
      if (node$unique_name %in% self$child_names()) {
        node <- node$clone()
      }

      # add to list of children
      self$children = c(self$children, node)
      node$add_parent(self)

    },

    remove_child = function (node) {

      # remove node from list of children
      rem_idx <- which(self$child_names() == node$unique_name)
      self$children <- self$children[-rem_idx]
      node$remove_parent(self)

    },

    add_parent = function (node) {

      # if the node is already listed as a parent, clone and re-register it to a
      # new name
      if (node$unique_name %in% self$parent_names()) {
        node <- node$clone()
      }

      # add to list of children
      self$parents = c(self$parents, node)

    },

    remove_parent = function (node) {

      # remove node from list of children
      rem_idx <- which(self$parent_names() == node$unique_name)
      self$parents <- self$parents[-rem_idx]

    },

    # return the names of all child nodes, and if recursive = TRUE, all nodes
    # lower in this graph. If type is a character, only nodes with that type
    # (from the type public object)  will  be listed
    child_names = function (recursive = TRUE) {

      children <- self$children

      if (length(children) > 0) {

        names <- vapply(children,
                        function(x) x$unique_name,
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
        names <- self$unique_name
      }

      names

    },

    parent_names = function (recursive = TRUE) {

      parents <- self$parents

      if (length(parents) > 0) {

        names <- vapply(parents,
                        function(x) x$unique_name,
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
        names <- self$unique_name
      }

      names

    },

    defined = function (dag) {
      tf_name <- dag$tf_name(self)
      tf_name %in% ls(dag$tf_environment)
    },

    # define this and all descendent objects on tensorflow graph in environment env
    define_tf = function (dag) {

      # if defined already, skip
      if (!self$defined(dag)) {

        # make sure children are defined
        children_defined <- vapply(self$children,
                                   function(x) x$defined(dag),
                                   FUN.VALUE = FALSE)

        if (any(!children_defined)) {
          lapply(self$children[which(!children_defined)],
                 function(x) x$define_tf(dag))
        }

        # then define self
        self$tf(dag)

      }

    },

    # get or set this nodes' current value
    value = function (new_value = NULL, ...) {

      if (is.null(new_value)) {

        self$.value

      } else {

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

      text <- node_type(self)

      if (!is.null(self$distribution)) {
        text <- paste(text,
                      'following a',
                      self$distribution$distribution_name,
                      'distribution')
      }

      text

    },

    get_unique_name = function () {

      name <- capture.output(self$.__enclos_env__)
      name <- gsub('<environment: ', 'node_', name)
      name <- gsub('>', '', name)
      self$unique_name <- name

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
