optimiser <- R6Class(
  "optimiser",
  inherit = inference,
  public = list(

    # optimiser information
    name = "",
    method = "method",
    parameters = list(),
    other_args = list(),
    max_iterations = 100L,
    tolerance = 1e-6,
    uses_callbacks = TRUE,
    adjust = TRUE,

    # modified during optimisation
    it = 0,
    old_obj = Inf,
    diff = Inf,

    # set up the model
    initialize = function(initial_values,
                          model,
                          name,
                          method,
                          parameters,
                          other_args,
                          max_iterations,
                          tolerance,
                          adjust) {
      super$initialize(initial_values,
                       model,
                       parameters = list(),
                       seed = get_seed()
      )

      self$name <- name
      self$method <- method
      self$parameters <- parameters
      self$other_args <- other_args
      self$max_iterations <- as.integer(max_iterations)
      self$tolerance <- tolerance
      self$adjust <- adjust

      if ("uses_callbacks" %in% names(other_args)) {
        self$uses_callbacks <- other_args$uses_callbacks
      }

      self$create_optimiser_objective()
      self$create_tf_minimiser()
    },
    parameter_names = function() {
      names(self$parameters)
    },
    # TF1/2
    # Not sure if the dtype needs to be set anymore, since I think TF2
    # will guess what it should be based on function input?
    # I see this is used inside of optimizers to ensure the right dtype is used
    # perhaps it will be OK to have this set...it's just that `on_graph()`
    # might not work in TF2?
    set_dtype = function(parameter_name, dtype) {
      params <- self$parameters
      param_names <- self$parameter_names()

      if (parameter_name %in% param_names) {
        param <- params[[parameter_name]]
        # self$model$dag$on_graph(
        tf_param <- tf$constant(param, dtype = dtype)
        # )
        params[[parameter_name]] <- tf_param
      }

      self$parameters <- params
    },

    # initialize the variables, then set the ones we care about
    # TF1/2
    # I think we can skip out on initializing the variables, since this is
    # done by default in TF2 now.
    set_inits = function() {
      dag <- self$model$dag
      tfe <- dag$tf_environment

      dag$tf_sess_run(tf$compat$v1$global_variables_initializer())

      shape <- tfe$optimiser_free_state$shape
      # dag$on_graph(
      tfe$optimiser_init <- tf$constant(self$free_state,
                                        shape = shape,
                                        dtype = tf_float()
      )
      # )

      . <- dag$tf_sess_run(optimiser_free_state$assign(optimiser_init))
    },

    # create a separate free state variable and objective, since optimisers must
    # use variables
    create_optimiser_objective = function() {
      dag <- self$model$dag
      tfe <- dag$tf_environment

      # TF 1/2 - do we care about live pointers anymore?
      # define a *variable* free state object
      # if (!live_pointer("optimiser_free_state", envir = tfe)) {
      if (!exists("optimiser_free_state", envir = tfe)) {
        dag$define_free_state("variable", name = "optimiser_free_state")
      }


      # use the log prob function to define objectives from the variable
      # TF 1/2 - probably going to delete this code as we don't care about
      # pointers to TF graph things
      # if (!live_pointer("optimiser_objective_adj", envir = tfe)) {
      if (!exists("optimiser_objective_adj", envir = tfe)) {
        # browser() <<
        # dag$on_graph(
        # TF 1/2 - this will not work at the moment, we need ot be able to
        # switch between using the adjusted or unadjusted log prob function
        # depending on what the user asked for
        objectives <- dag$tf_log_prob_function(tfe$optimiser_free_state)
        # )

        assign("optimiser_objective_adj",
               -objectives$adjusted,
               envir = tfe
        )

        assign("optimiser_objective",
               -objectives$unadjusted,
               envir = tfe
        )
      }
    },
    run = function() {
      self$model$dag$build_feed_dict()
      self$set_inits()
      self$run_minimiser()
      self$fetch_free_state()
    },
    fetch_free_state = function() {

      # get the free state as a vector
      self$free_state <- self$model$dag$tf_sess_run(optimiser_free_state)
    },
    return_outputs = function() {
      dag <- self$model$dag

      # if the optimiser was ignoring the callbacks, we have no idea about the
      # number of iterations or convergence
      if (!self$uses_callbacks) {
        self$it <- NA
      }

      converged <- self$it < (self$max_iterations - 1)

      par <- dag$trace_values(self$free_state, flatten = FALSE)
      par <- lapply(par, drop_first_dim)
      par <- lapply(par, drop_column_dim)

      list(
        par = par,
        value = -dag$tf_sess_run(joint_density),
        iterations = self$it,
        convergence = ifelse(converged, 0, 1)
      )
    }
  )
)

tf_optimiser <- R6Class(
  "tf_optimiser",
  inherit = optimiser,
  public = list(

    # some of the optimisers are very fussy about dtypes, so convert them now
    sanitise_dtypes = function() {
      self$set_dtype("global_step", tf$int64)

      if (self$name == "proximal_gradient_descent") {
        lapply(self$parameter_names(), self$set_dtype, tf$float64)
      }

      if (self$name == "proximal_adagrad") {
        fussy_params <- c(
          "learning_rate",
          "l1_regularization_strength",
          "l2_regularization_strength"
        )

        lapply(fussy_params, self$set_dtype, tf$float64)
      }
    },

    # create an op to minimise the objective
    create_tf_minimiser = function() {
      # browser()
      dag <- self$model$dag
      tfe <- dag$tf_environment

      self$sanitise_dtypes()

      optimise_fun <- eval(parse(text = self$method))
      # dag$on_graph(
      tfe$tf_optimiser <- do.call(
        optimise_fun,
        self$parameters
      )
      # )

      # browser()
      if (self$adjust) {
        dag$tf_run(train <- tf_optimiser$minimize(optimiser_objective_adj))
      } else {
        dag$tf_run(train <- tf_optimiser$minimize(optimiser_objective))
      }
    },

    # minimise the objective function
    run_minimiser = function() {
      self$set_inits()

      while (self$it < self$max_iterations &
             self$diff > self$tolerance) {
        self$it <- self$it + 1
        self$model$dag$tf_sess_run(train)
        if (self$adjust) {
          obj <- self$model$dag$tf_sess_run(optimiser_objective_adj)
        } else {
          obj <- self$model$dag$tf_sess_run(optimiser_objective)
        }
        self$diff <- abs(self$old_obj - obj)
        self$old_obj <- obj
      }
    }
  )
)

tfp_optimiser <- R6Class(
  "tfp_optimiser",
  inherit = optimiser,
  public = list(

    # some of the optimisers are very fussy about dtypes, so convert them now
    sanitise_dtypes = function() {
      self$set_dtype("global_step", tf$int64)

      if (self$name == "proximal_gradient_descent") {
        lapply(self$parameter_names(), self$set_dtype, tf$float64)
      }

      if (self$name == "proximal_adagrad") {
        fussy_params <- c(
          "learning_rate",
          "l1_regularization_strength",
          "l2_regularization_strength"
        )

        lapply(fussy_params, self$set_dtype, tf$float64)
      }
    },

    # create an op to minimise the objective
    create_tfp_minimiser = function() {
      # browser()
      dag <- self$model$dag
      tfe <- dag$tf_environment

      self$sanitise_dtypes()

      optimise_fun <- eval(parse(text = self$method))
      # dag$on_graph(
      tfe$tf_optimiser <- do.call(
        optimise_fun,
        self$parameters
      )
      # )

      if (self$adjust) {
        dag$tf_run(train <- tf_optimiser$minimize(optimiser_objective_adj))
      } else {
        dag$tf_run(train <- tf_optimiser$minimize(optimiser_objective))
      }
    },

    # minimise the objective function
    run_minimiser = function() {
      self$set_inits()

      while (self$it < self$max_iterations &
             self$diff > self$tolerance) {
        self$it <- self$it + 1
        self$model$dag$tf_sess_run(train)
        if (self$adjust) {
          obj <- self$model$dag$tf_sess_run(optimiser_objective_adj)
        } else {
          obj <- self$model$dag$tf_sess_run(optimiser_objective)
        }
        self$diff <- abs(self$old_obj - obj)
        self$old_obj <- obj
      }
    }
  )
)

# TF1/2
# I think gien that the scipy optimisers have been removed, then this code
# could be removed as well? Might be worthwhile to explore/understand if this
# output is different to the tensorflow output?
scipy_optimiser <- R6Class(
  "scipy_optimiser",
  inherit = optimiser,
  public = list(
    create_tf_minimiser = function() {
      dag <- self$model$dag
      tfe <- dag$tf_environment

      opt_fun <- eval(parse(text = "tf$contrib$opt$ScipyOptimizerInterface"))

      if (self$adjust) {
        loss <- tfe$optimiser_objective_adj
      } else {
        loss <- tfe$optimiser_objective
      }

      args <- list(
        loss = loss,
        method = self$method,
        options = c(self$parameters,
                    maxiter = self$max_iterations
        ),
        tol = self$tolerance
      )

      # dag$on_graph(
      tfe$tf_optimiser <- do.call(opt_fun, args)
      # )
    },
    obj_progress = function(obj) {
      self$diff <- abs(self$old_obj - obj)
      self$old_obj <- obj
    },
    it_progress = function(...) {
      self$it <- self$it + 1
    },
    run_minimiser = function() {
      dag <- self$model$dag
      tfe <- dag$tf_environment
      tfe$it_progress <- self$it_progress
      tfe$obj_progress <- self$obj_progress

      self$set_inits()

      # run the optimiser, suppressing python's yammering
      quietly(
        dag$tf_run(
          tf_optimiser$minimize(sess,
                                feed_dict = feed_dict,
                                step_callback = it_progress,
                                loss_callback = obj_progress,
                                fetches = list(optimiser_objective_adj)
          )
        )
      )
    }
  )
)
