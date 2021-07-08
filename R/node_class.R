# base node class
node <- R6Class(
  "node",
  public = list(
    unique_name = "",
    parents = list(),
    children = list(),
    representations = list(),
    .value = array(NA),
    dim = NA,
    distribution = NULL,
    initialize = function(dim = NULL, value = NULL) {
      if (is.null(dim)) {
        dim <- c(1, 1)
      }

      # coerce dim to integer
      dim <- as.integer(dim)

      # store array (updates dim)
      if (is.null(value)) {
        value <- unknowns(dim = dim)
      }

      self$value(value)
      self$get_unique_name()
    },
    register = function(dag) {
      if (!(self$unique_name %in% names(dag$node_list))) {
        dag$node_list[[self$unique_name]] <- self
      }
    },

    # recursively register self and family
    register_family = function(dag) {
      if (!(self$unique_name %in% names(dag$node_list))) {

        # add self to list
        self$register(dag)

        # find my immediate family (not including self)
        family <- c(self$list_children(dag), self$list_parents(dag))

        # get and assign their names
        family_names <- vapply(family, member, "unique_name", FUN.VALUE = "")
        names(family) <- family_names

        # find the unregistered ones
        unregistered_idx <- which(!(family_names %in% names(dag$node_list)))
        unregistered <- family[unregistered_idx]

        # add them to the node list
        for (relative in unregistered) {
          relative$register_family(dag)
        }
      }
    },
    add_parent = function(node) {

      # add to list of parents
      self$parents <- c(self$parents, node)
      node$add_child(self)
    },
    remove_parent = function(node) {

      # remove node from list of parents
      rem_idx <- which(self$parent_names(recursive = FALSE) == node$unique_name)
      self$parents <- self$parents[-rem_idx]
      node$remove_child(self)
    },
    list_parents = function(dag) {
      parents <- self$parents

      # if this node is being sampled and has a distribution, consider
      # that a parent node too
      mode <- dag$how_to_define(self)
      if (mode == "sampling" & has_distribution(self)) {
        parents <- c(parents, list(self$distribution))
      }

      parents
    },
    add_child = function(node) {

      # add to list of children
      self$children <- c(self$children, node)
    },
    remove_child = function(node) {

      # remove node from list of parents
      rem_idx <- which(self$child_names() == node$unique_name)
      self$children <- self$children[-rem_idx]
    },
    list_children = function(dag) {
      children <- self$children

      # if this node is being sampled and has a distribution, do not consider
      # that a child node
      mode <- dag$how_to_define(self)
      if (mode == "sampling" & has_distribution(self)) {
        child_names <- vapply(children,
          member,
          "unique_name",
          FUN.VALUE = character(1)
        )
        keep <- child_names != self$distribution$unique_name
        children <- children[keep]
      }

      children
    },

    # return the names of all parent nodes, and if recursive = TRUE, all nodes
    # lower in this graph. If type is a character, only nodes with that type
    # (from the type public object)  will  be listed
    # NB. this is the true, forward parentage and is unaffected by the dag mode!
    parent_names = function(recursive = FALSE) {
      parents <- self$parents

      if (length(parents) > 0) {
        names <- vapply(parents,
          member,
          "unique_name",
          FUN.VALUE = character(1)
        )

        if (recursive) {
          their_parents <- function(x) {
            x$parent_names(recursive = TRUE)
          }

          grandparents <- lapply(parents, their_parents)
          names <- c(names, unlist(grandparents))
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
    child_names = function() {
      children <- self$children

      if (length(children) > 0) {
        names <- vapply(
          children,
          function(x) x$unique_name,
          ""
        )

        # account for multiple nodes depending on the same nodes
        names <- unique(names)
      } else {
        # otherwise return own name (to make sure at least somethign is returned
        # on recursion)
        names <- self$unique_name
      }

      names
    },
    defined = function(dag) {
      tf_name <- dag$tf_name(self)
      tf_name %in% ls(dag$tf_environment)
    },

    # define this and all descendent objects on tensorflow graph in environment
    # env
    define_tf = function(dag) {

      # if defined already, skip
      if (!self$defined(dag)) {

        # make sure parents are defined
        parents_defined <- vapply(self$list_parents(dag),
          function(x) x$defined(dag),
          FUN.VALUE = FALSE
        )

        if (any(!parents_defined)) {
          parents <- self$list_parents(dag)
          lapply(
            parents[which(!parents_defined)],
            function(x) x$define_tf(dag)
          )
        }

        # then define self
        self$tf(dag)
      }
    },

    # get or set this nodes' current value
    value = function(new_value = NULL, ...) {
      if (is.null(new_value)) {
        self$.value
      } else {

        # get the dimension of the new value
        dim <- dim(new_value)

        # coerce 1D arrays to column vectors
        if (length(dim) == 1) {
          dim <- c(dim, 1L)
        }

        # update value and store
        dim(new_value) <- dim
        self$dim <- dim

        self$.value <- new_value
      }
    },
    set_distribution = function(distribution) {

      # check it
      if (!inherits(distribution, "distribution_node")) {
        msg <- cli::format_error(
          "invalid distribution"
        )
        stop(
          msg,
          call. = FALSE
        )
      }

      # add it
      self$distribution <- distribution
    },

    # return a string describing this node, for use in print and summary etc.
    description = function() {
      text <- node_type(self)

      if (has_distribution(self)) {
        text <- paste(
          text,
          "following a",
          self$distribution$distribution_name,
          "distribution"
        )
      }

      text
    },
    get_unique_name = function() {
      self$unique_name <- paste0("node_", rhex())
    },
    plotting_label = function() {
      label <- ""
      type <- node_type(self)

      # replace distributions with more info
      if (type == "distribution") {
        label <- self$distribution_name
      }

      # if it's data and scalar, just put the value
      if (type == "data" & is_scalar(self)) {
        val <- as.numeric(self$value())
        val <- round(val, 2)
        label <- prettyNum(val)
      }

      label
    }
  )
)

# generic to grab dimensions
dim.node <- function(x) {
  x$dim
}

# coerce an object to a node
to_node <- function(x) {
  if (!inherits(x, "node")) {
    if (inherits(x, "greta_array")) {
      x <- get_node(x)
    } else if (is.numeric(x)) {
      x <- data_node$new(x)
    } else {
      msg <- cli::format_error(
        "cannot coerce object to a node"
      )
      stop(
        msg,
        call. = FALSE
      )
    }
  }
  x
}
