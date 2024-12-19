# base node class
node <- R6Class(
  "node",
  public = list(
    unique_name = "",
    parents = list(),
    children = list(),
    # named greta arrays giving different representations of the greta array
    # represented by this node that have already been calculated, to be used for
    # computational speedups or numerical stability. E.g. a logarithm or a
    # cholesky factor
    representations = list(),
    anti_representations = list(),
    .value = array(NA),
    dim = NA,
    distribution = NULL,
    initialize = function(dim = NULL, value = NULL) {
      dim <- dim %||% c(1,1)

      # coerce dim to integer
      dim <- as.integer(dim)

      # store array (updates dim)
      value <- value %||% unknowns(dim = dim)

      self$value(value)
      self$create_unique_name()
    },
    register = function(dag) {
      ## TODO add explaining variable
      if (!(self$unique_name %in% names(dag$node_list))) {
        dag$node_list[[self$unique_name]] <- self
      }
    },

    # recursively register self and family
    register_family = function(dag) {
      ## TODO add explaining variable
      if (!(self$unique_name %in% names(dag$node_list))) {

        # add self to list
        self$register(dag)

        # find my immediate family (not including self)
        family <- c(self$list_children(dag), self$list_parents(dag))

        # get and assign their names
        family_names <- extract_unique_names(family)
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

      if (mode == "sampling" & has_representation(self, "cholesky")){
        # remove cholesky representation node from parents
        parent_names <- extract_unique_names(parents)
        antirep_name <- get_node(self$representations$cholesky)$unique_name
        parent_names_keep <- setdiff(parent_names, antirep_name)
        parents <- parents[match(parent_names_keep, parent_names)]
      }

      if (mode == "sampling" & has_anti_representation(self, "chol2symm")){
        chol2symm_node <- get_node(self$anti_representations$chol2symm)
        parents <- c(parents, list(chol2symm_node))
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
        child_names <- extract_unique_names(children)
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
        names <- extract_unique_names(parents)

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

    # define this and all descendent objects on TF graph in environment
    define_tf = function(dag) {
      if (Sys.getenv("GRETA_DEBUG") == "true") {
        browser()
      }
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
            function(x){
              x$define_tf(dag)
            }
          )
        }

        # then define self
          # stop("hi from the future ... parents are of class:", str(parents))
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

      check_is_distribution_node(distribution)

      # add it
      self$distribution <- distribution
    },

    # return a string describing this node, for use in print and summary etc.
    description = function() {
      text <- node_type(self)

      if (has_distribution(self)) {
        text <- glue::glue(
          "{text} following a ",
          "{self$distribution$distribution_name} distribution"
        )
      }

      text
    },
    cli_description = function() {
      text <- node_type(self)
      text <- node_type_colour(text)

      dist_txt <- glue::glue("{self$distribution$distribution_name} distribution")
      if (has_distribution(self)) {
        text <- cli::cli_fmt(
          cli::cli_text(
            # "{text} following a {.strong {dist_txt}}"
            "{text} following a {cli::col_yellow({dist_txt})}"
          )
        )
      }

      text
    },

    create_unique_name = function() {
      self$unique_name <- glue::glue("node_{rhex()}")
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
    },
    make_antirepresentations = function(representations){
      mapply(
        FUN = self$make_one_anti_representation,
        representations,
        names(representations)
        )
    },
    make_one_anti_representation = function(ga, name){
      node <- get_node(ga)
      anti_name <- self$find_anti_name(name)
      node$anti_representations[[anti_name]] <- as.greta_array(self)
      node
    },
    find_anti_name = function(name){
      switch(name,
             cholesky = "chol2symm",
             chol2symm = "chol",
             exp = "log",
             log = "exp",
             probit = "iprobit",
             iprobit = "probit",
             logit = "ilogit",
             ilogit = "logit"
      )
    }
  )
)

#' @title generic to grab dimensions of nodes
#' @param x greta node class
#' @export
dim.node <- function(x) {
  x$dim
}

# coerce an object to a node
to_node <- function(x) {
  # TODO: clean up this logic
  if (!is.node(x)) {
    if (is.greta_array(x)) {
      x <- get_node(x)
    } else if (is.numeric(x)) {
      x <- data_node$new(x)
    } else {
      cli::cli_abort(
        "cannot coerce object to a node"
      )
    }
  }
  x
}
