#' @importFrom reticulate py_set_attr
#' @importFrom tensorflow dict
#' @importFrom R6 R6Class

# create dag class
dag_class <- R6Class(
  "dag_class",
  public = list(
    mode = "all_forward",
    node_list = list(),
    target_nodes = list(),
    variables_without_free_state = list(),
    tf_environment = NA,
    tf_graph = NA,
    tf_float = NA,
    n_cores = 0L,
    compile = NA,
    trace_names = NULL,

    # create a dag from some target nodes
    initialize = function(target_greta_arrays,
                          tf_float = "float32",
                          compile = FALSE) {
      # build the dag
      self$build_dag(target_greta_arrays)

      # find the nodes we care about
      self$target_nodes <- lapply(target_greta_arrays, get_node)

      # set up the tf environment, with a graph
      # TF1/2 check
      # not sure if we need to build the new environment in eager mode?
      self$new_tf_environment()

      # store the performance control info
      self$tf_float <- tf_float
      self$compile <- compile
      self$define_tf_trace_values_batch()
      self$define_tf_log_prob_function()
    },

    define_tf_trace_values_batch = function(){
      self$tf_trace_values_batch <- tensorflow::tf_function(
        f = self$define_trace_values_batch
      )
    },

    define_tf_log_prob_function = function(){
      self$tf_log_prob_function <- tensorflow::tf_function(
        # TF1/2 check
        # need to check in on all cases of `tensorflow::tf_function()`
        # as we are getting lots of warnings about retracting
        f = self$generate_log_prob_function()
      )
    },

    tf_log_prob_function = NULL,

    tf_log_prob_function_adjusted = function(free_state){
      self$tf_log_prob_function(free_state)$adjusted
    },

    tf_log_prob_function_unadjusted = function(free_state){
      self$tf_log_prob_function(free_state)$unadjusted
    },

    # TF1/2 check
    # built with TF
    # Not sure if we need tensorflow environments in TF2, given that
    # everything will be passed as functions?
    new_tf_environment = function() {
      self$tf_environment <- new.env()
      self$tf_graph <- tf$Graph()
      self$tf_environment$all_forward_data_list <- list()
      self$tf_environment$all_sampling_data_list <- list()
      self$tf_environment$hybrid_data_list <- list()
    },

    # return a list of nodes connected to those in the target node list
    build_dag = function(greta_array_list) {
      target_node_list <- lapply(greta_array_list, get_node)

      # loop through the target nodes, recursively registering them to this dag
      for (node in target_node_list) {
        node$register_family(self)
      }
    },

    # get the TF names for different node types
    get_tf_names = function(types = NULL) {

      # get tf basenames
      names <- self$node_tf_names
      if (!is.null(types)) {
        names <- names[which(self$node_types %in% types)]
      }

      # prepend mode
      if (length(names) > 0) {
        names <- paste(self$mode, names, sep = "_")
      }
    },

    # look up the TF name for a single node
    tf_name = function(node) {

      # get tf basename from node name
      name <- self$node_tf_names[node$unique_name]
      if (length(name) == 0) {
        name <- ""
      }

      # prepend mode
      if (!is.na(name)) {
        name <- paste(self$mode, name, sep = "_")
      }

      name
    },

    # how to define a node if the sampling mode is hybrid (this is quite knotty,
    # so gets its own function)
    how_to_define_hybrid = function(node) {
      node_type <- node_type(node)

      # names of variable nodes not connected to the free state in this dag
      stateless_names <- names(self$variables_without_free_state)

      # if the node is data, use sampling mode if it has a distribution and
      # forward mode if not
      if (node_type == "data") {
        node_mode <- ifelse(has_distribution(node), "sampling", "forward")
      }

      # if the node is a variable, use forward mode if it has a free state,
      # and sampling mode if not
      if (node_type == "variable") {
        to_sample <- node$unique_name %in% stateless_names
        node_mode <- ifelse(to_sample, "sampling", "forward")
      }

      # if it's an operation, see if it has a distribution (for lkj and
      # wishart) and get mode based on whether the parent has a free state
      if (node_type == "operation") {
        parent_name <- node$parents[[1]]$unique_name
        parent_stateless <- parent_name %in% stateless_names
        to_sample <- has_distribution(node) & parent_stateless
        node_mode <- ifelse(to_sample, "sampling", "forward")
      }

      # if the node is a distribution, decide based on its target
      if (node_type == "distribution") {
        target <- node$target
        target_type <- node_type(target)

        # if it has no target (e.g. for a mixture distribution), define it in
        # sampling mode (so it defines before the things that depend on it)
        if (is.null(target)) {
          node_mode <- "sampling"
        }

        # if the target is data, use sampling mode
        if (target_type == "data") {
          node_mode <- "sampling"
        }

        # if the target is a variable, use forward mode if it has a free
        # state, and sampling mode if not
        if (target_type == "variable") {
          to_sample <- target$unique_name %in% stateless_names
          node_mode <- ifelse(to_sample, "sampling", "forward")
        }

        # if the target is an operation, see if that operation has a single
        # parent that is a variable, and see if that has a free state
        if (target_type == "operation") {
          target_parent_name <- target$parents[[1]]$unique_name
          target_parent_stateless <- target_parent_name %in% stateless_names
          node_mode <- ifelse(target_parent_stateless, "sampling", "forward")
        }
      }

      node_mode
    },

    # how to define the node if we're sampling everything (no free state)
    how_to_define_all_sampling = function(node) {
      switch(node_type(node),
             data = ifelse(has_distribution(node), "sampling", "forward"),
             operation = ifelse(has_distribution(node), "sampling", "forward"),
             "sampling"
      )
    },

    # tell a node whether to define itself in forward mode (deterministically
    # from an existing free state), or in sampling mode (generate a random
    # version of itself)
    how_to_define = function(node) {
      switch(self$mode,

             # if doing inference, everything is push-forward
             all_forward = "forward",

             # sampling from prior most nodes are in sampling mode
             all_sampling = self$how_to_define_all_sampling(node),

             # sampling from posterior some nodes defined forward, others sampled
             hybrid = self$how_to_define_hybrid(node)
      )
    },
    define_batch_size = function() {
      # TF1/2 check?
      # pretty sure `batch_size` just now needs to be the input of a function
      # I'm not even sure that batch_size needs to be a function, it might
      # just need to be the input to wherever it is used next?

      ## NOTE: when calling `model` there is no `free_state` in `tf_environment`
      ## Trying out something where the free state is set if there isn't one?

      if (!exists("batch_size", envir = self$tf_environment)) {
        with(
          data = self$tf_environment,
          batch_size <- tf$shape(self$tf_environment$free_state)[0]
        )
      }

      # put this in the greta stash, so it can be accessed by other (sub-)dags
      # if needed, e.g. when using as_tf_function()
      assign("batch_size", self$tf_environment$batch_size, envir = greta_stash)

    },

    define_free_state = function(type = c("variable", "placeholder"),
                                 name = "free_state") {
      type <- match.arg(type)
      tfe <- self$tf_environment

      vals <- self$example_parameters(free = TRUE)
      vals <- unlist_tf(vals)

      if (type == "variable") {

        # TF1/2 check
        # tf$Variable seems to have trouble assigning values, if created with
        # numeric (rather than logical) NAs
        vals <- as.logical(vals)
        vals <- t(as.matrix(vals))

        free_state <- tf$Variable(
          initial_value = vals,
          dtype = tf_float()
        )
      } else {
        shape <- shape(NULL, length(vals))
        # TF1/2 check
        # instead?
        # free_state <- tensorflow::as_tensor(
        #   dtype = tf_float(),
        #   shape = shape
        # )
      }

      assign(name,
             free_state,
             envir = tfe
      )
    },

    # split the overall free state vector into free versions of variables
    split_free_state = function() {
      tfe <- self$tf_environment

      free_state <- get("free_state", envir = tfe)

      params <- self$example_parameters(free = TRUE)

      lengths <- lengths(params)

      if (length(lengths) > 1) {
        args <- tf$split(free_state, lengths, axis = 1L)
      } else {
        args <- list(free_state)
      }

      names <- glue::glue("{names(params)}_free")

      for (i in seq_along(names)) {
        assign(names[i], args[[i]], envir = tfe)
      }
    },

    # define the body of the tensorflow graph in the environment env; without
    # defining the free_state, or the densities etc.
    define_tf_body = function(target_nodes = self$node_list) {

      # if in forward or hybrid mode, split up the free state
      if (self$mode %in% c("all_forward", "hybrid")) {
        self$split_free_state()
      }

      # define all nodes in the environment and on the graph
      lapply(target_nodes, function(x){
        x$define_tf(self)
      })

      invisible(NULL)
    },

    # TF1/2 check?
    # I think we can probably remove this part of things? However I'm not sure
    # if the "mode" part is going to be imporatnt here?
    # define tf graph in environment; either for forward-mode computation from a
    # free state variable, or for sampling
    define_tf = function(target_nodes = self$node_list) {
      # define the free state variable
      # TF1/2 check?
      # pretty sure define_batch_size needs to be passed as an argument to
      # whatever is above here...if define_tf even needs to exist?
      # and I think we can remove define_batch_size since
      # this should just be passed as an argument later?

      if (self$mode != "all_sampling") {
        self$define_batch_size()
      }

      self$define_tf_body(target_nodes = target_nodes)

    },

    # define tensor for overall log density and gradients
    define_joint_density = function() {
      tfe <- self$tf_environment

      # get all distribution nodes that have a target
      distribution_nodes <- self$node_list[self$node_types == "distribution"]
      target_nodes <- lapply(distribution_nodes, member, "get_tf_target_node()")
      has_target <- !are_null(target_nodes)
      distribution_nodes <- distribution_nodes[has_target]
      target_nodes <- target_nodes[has_target]

      # get the densities, evaluated at these targets
      densities <- mapply(self$evaluate_density,
                          distribution_nodes,
                          target_nodes,
                          SIMPLIFY = FALSE
      )

      # reduce_sum each of them (skipping the batch dimension)
      summed_densities <- lapply(densities, tf_sum, drop = TRUE)

      # sum them together
      names(summed_densities) <- NULL
      joint_density <- tf$add_n(summed_densities)

      # assign overall density to environment
      assign("joint_density",
             joint_density,
             envir = self$tf_environment
      )

      # define adjusted joint density

      # get names of Jacobian adjustment tensors for all variable nodes
      adj_names <- glue::glue("{self$get_tf_names(types = 'variable')}_adj")

      # get TF density tensors for all distribution
      adj <- lapply(adj_names, get, envir = self$tf_environment)

      # remove their names and sum them together (accounting for tfp bijectors
      # sometimes returning a scalar tensor)
      names(adj) <- NULL
      adj <- match_batches(adj)
      total_adj <- tf$add_n(adj)

      # assign overall density to environment
      assign("joint_density_adj",
             joint_density + total_adj,
             envir = self$tf_environment
      )
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
      self$tf_evaluate_density(
        tfp_distribution,
        tf_target,
        truncation = distribution_node$truncation,
        bounds = distribution_node$bounds
      )
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

        ## TODO add explaining variables
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

    ###<<<
    hessians = function(
      free_state,
      nodes = self$target_nodes,
      which_objective = c(
        "adjusted",
        "unadjusted"
      )
    ){
      which_objective <- match.arg(which_objective)

      ga_names <- names(nodes)

      ## TF1/2 retracing
      ## This is a location where retracting happens in `opt`
      hessian_list <- lapply(X = nodes,
             self$calculate_one_hessian,
             free_state = free_state,
             which_objective = which_objective)
      # assign names and return
      names(hessian_list) <- ga_names
      hessian_list
    },

    calculate_one_hessian = function(
    node,
    free_state,
    which_objective = c(
      "adjusted",
      "unadjusted"
    )
    ) {
      which_objective <- match.arg(which_objective)

      # temporarily define a new environment
      tfe_old <- self$tf_environment
      on.exit(self$tf_environment <- tfe_old)
      tfe <- self$tf_environment <- new.env()

      # put the free state in the environment, and build out the tf graph
      tfe$free_state <- free_state

      # get names and dimensions of target greta arrays
      ga_dim <- node$dim
      tf_name <- self$tf_name(node)

      # we now make all of the operations define themselves now
      with(tf$GradientTape() %as% tape_1, {
        with(tf$GradientTape() %as% tape_2, {
          self$define_tf()
          # define the densities
          self$define_joint_density()

          xs <- get(tf_name, tfe)

          objectives <- list(
            adjusted = tfe$joint_density_adj,
            unadjusted = tfe$joint_density
          )

          # return either of the densities, or a list of both
          y <- switch(which_objective,
                      adjusted = objectives$adjusted,
                      unadjusted = objectives$unadjusted
          )
        })
        g <- tape_2$gradient(y, xs)
      })
      h <- tape_1$jacobian(g, xs)

      # reshape from tensor to R dimensions
      hessian <- array(h$numpy(), dim = hessian_dims(ga_dim))

      hessian

    },

    ###<<<

    # return a function to obtain the model log probability from a tensor for
    # the free state
    generate_log_prob_function = function(which = c(
      "both",
      "adjusted",
      "unadjusted"
    )) {
      which <- match.arg(which)

      # we can only pass the free_state parameter through
      # we need some way to lexically scope the
      # batch size and the data
      function(free_state) {
        # temporarily define a new environment
        tfe_old <- self$tf_environment
        on.exit(self$tf_environment <- tfe_old)
        tfe <- self$tf_environment <- new.env()

        # put the free state in the environment, and build out the tf graph
        tfe$free_state <- free_state

        # we now make all of the operations define themselves now
        self$define_tf()
        # define the densities
        self$define_joint_density()

        objectives <- list(
          adjusted = tfe$joint_density_adj,
          unadjusted = tfe$joint_density
        )

        # return either of the densities, or a list of both
        result <- switch(which,
                         adjusted = objectives$adjusted,
                         unadjusted = objectives$unadjusted,
                         both = objectives
        )

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

      # remove any of these that don't need a free state here (for calculate())
      stateless_names <- vapply(self$variables_without_free_state,
                                self$tf_name,
                                FUN.VALUE = character(1)
      )
      keep <- !names(parameters) %in% stateless_names
      parameters <- parameters[keep]

      parameters
    },
    get_tf_data_list = function() {
      data_list_name <- glue::glue("{self$mode}_data_list")
      self$tf_environment[[data_list_name]]
    },
    set_tf_data_list = function(element_name, value) {
      data_list_name <- glue::glue("{self$mode}_data_list")
      self$tf_environment[[data_list_name]][[element_name]] <- value
    },

    # get adjusted joint log density across the whole dag
    log_density = function() {
      res <- cleanly(self$tf_environment$joint_density_adj)

      if (inherits(res, "error")) {
        res <- NA
      }

      res
    },

    tf_trace_values_batch = NULL,
    trace_values_batch = function(free_state_batch){
      lapply(
        X = self$tf_trace_values_batch(free_state_batch),
        FUN = as.array
      )
    },

    define_trace_values_batch = function(free_state_batch) {

      # update the parameters & build the feed dict
      target_tf_names <- lapply(
        self$target_nodes,
        self$tf_name
      )

      # TF1/2 check
      # maybe remove onexit stuff?
      tfe_old <- self$tf_environment
      on.exit(self$tf_environment <- tfe_old)
      tfe <- self$tf_environment <- new.env()

      # put the free state in the environment, and build out the tf graph
      tfe$free_state <- free_state_batch

      # we now make all of the operations define themselves now
      self$define_tf()

      target_tensors <- lapply(target_tf_names,
                               get,
                               envir = tfe
      )

      return(target_tensors)
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
        trace_list_flat <- lapply(
          seq_along(trace_list),
          flatten_trace,
          trace_list
        )

        out <- do.call(cbind, trace_list_flat)
        self$trace_names <- colnames(out)
      } else {
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
          break()
        }
      }

      check_timeout(it, maxit)

      # find the cluster IDs
      n <- nrow(r)
      neighbours <- lapply(seq_len(n), function(i) which(r[i, ]))
      cluster_names <- vapply(X = neighbours,
                              FUN = paste,
                              FUN.VALUE = character(1),
                              collapse = "_")
      cluster_id <- match(cluster_names, unique(cluster_names))

      # name them
      names(cluster_id) <- rownames(adj)
      cluster_id
    },

    # get the tfp distribution object for a distribution node
    get_tfp_distribution = function(distrib_node) {

      # build the tfp distribution object for the distribution, and use it
      # to get the tensor for the sample
      distrib_constructor <- self$get_tf_object(distrib_node)
      parameter_nodes <- distrib_node$parameters
      tf_parameter_list <- lapply(parameter_nodes, self$get_tf_object)

      # execute the distribution constructor functions to return a tfp
      # distribution object
      tfp_distribution <- distrib_constructor(tf_parameter_list, dag = self)
    },

    # try to draw a random sample from a distribution node
    draw_sample = function(distribution_node) {
      # self$check_sampling_implemented(distribution_node)
      tfp_distribution <- self$get_tfp_distribution(distribution_node)

      sample <- tfp_distribution$sample

      check_sampling_implemented(sample, distribution_node)

      truncation <- distribution_node$truncation

      if (is.null(truncation)) {

        # if we're not dealing with truncation, sample directly
        tensor <- sample(seed = get_seed())
      } else {

        # if we're dealing with truncation (therefore univariate and continuous)
        # sample a random uniform (tensor), and pass through the truncated
        # quantile (inverse cdf) function

        cdf <- tfp_distribution$cdf
        quantile <- tfp_distribution$quantile
        check_truncation_implemented(tfp_distribution, distribution_node)

        # generate a random uniform sample of the correct shape and transform
        # through truncated inverse CDF to get draws on truncated scale
        u <- tf_randu(distribution_node$dim, self)

        lower <- cdf(fl(truncation[1]))
        upper <- cdf(fl(truncation[2]))
        range <- upper - lower

        tensor <- quantile(lower + u * range)
      }

      tensor
    }
  ),
  active = list(
    node_types = function(value) {
      vapply(self$node_list, node_type, FUN.VALUE = "")
    },

    # create human-readable base names for TF tensors. these will actually be
    # defined prepended with "all_forward_" or "all_sampling" or "hybrid_
    node_tf_names = function(value) {
      types <- self$node_types

      for (type in c("variable", "data", "operation", "distribution")) {
        idx <- which(types == type)
        types[idx] <- paste(type, seq_along(idx), sep = "_")
      }

      types
    },
    adjacency_matrix = function(value) {

      # make dag matrix
      n_node <- length(self$node_list)
      node_names <- names(self$node_list)
      node_types <- self$node_types
      dag_mat <- matrix(0, nrow = n_node, ncol = n_node)
      rownames(dag_mat) <- colnames(dag_mat) <- node_names

      children <- lapply(
        self$node_list,
        member,
        "child_names()"
      )
      parents <- lapply(
        self$node_list,
        member,
        "parent_names(recursive = FALSE)"
      )

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

      dag_mat
    }
  )
)
