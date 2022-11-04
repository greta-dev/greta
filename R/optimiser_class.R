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
    minimiser_function = NULL,

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

      # browser()
      # self$create_optimiser_objective()
      ## TF1/2 create_tf_minimiser needs to be renamed to reflect that it
      ## is actually running the optimisation
      # self$create_tf_minimiser()
      run_optimiser(self)
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

      # dag$tf_sess_run(tf$compat$v1$global_variables_initializer())

      shape <- tfe$optimiser_free_state$shape
      # dag$on_graph(
      tfe$optimiser_init <- tf$constant(self$free_state,
                                        shape = shape,
                                        dtype = tf_float()
      )
      # )

      # . <- dag$tf_sess_run(optimiser_free_state$assign(optimiser_init))
      tfe$optimiser_free_state$assign(tfe$optimiser_init)
    },

    # TF1/2 - current interface might actually change this somewhat
    # as we end up just redefining the optimiser /tf log prob function
    # again later on

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

        assign(
          x = "optimiser_objective_adj",
          value = function() -dag$tf_log_prob_function(free_state)$adjusted,
          envir = tfe
        )

        assign(
          x = "optimiser_objective",
          value = function() -dag$tf_log_prob_function(free_state)$unadjusted,
          envir = tfe
        )

      }
    },
    run = function() {
      # TF1/2 - don't need to build a feed_dict anymore as it isn't getting built?
      # self$model$dag$build_feed_dict()
      # self$set_inits()
      # self$run_minimiser()
      self$minimiser_function(self$free_state)
      self$fetch_free_state()

    },
    fetch_free_state = function() {

      tfe <- self$model$dag$tf_environment
      # get the free state as a vector
      # self$free_state <- self$model$dag$tf_sess_run(optimiser_free_state)
      # self$free_state <- tfe$optimiser_free_state
      self$free_state <- tfe$free_state

    },
    return_outputs = function() {
      dag <- self$model$dag
      # browser()

      # if the optimiser was ignoring the callbacks, we have no idea about the
      # number of iterations or convergence
      if (!self$uses_callbacks) {
        self$it <- NA
      }

      # TF1/2 need to note some way of the number of iterations
      converged <- self$it < (self$max_iterations - 1)
      # because we need to resolve an issue with indexing of TF object
      r_free_state <- as.array(self$free_state)
      par <- dag$trace_values(r_free_state, flatten = FALSE)
      par <- lapply(par, drop_first_dim)
      par <- lapply(par, drop_column_dim)

      if (self$adjust) {
        value <- dag$tf_log_prob_function(self$free_state)$adjusted
      } else {
        value <- dag$tf_log_prob_function(self$free_state)$unadjusted
      }

      value <- as.array(value) * -1

      list(
        par = par,
        #TF1/2 - need to remove uses of tf_sess_run
        # value = -dag$tf_sess_run(joint_density),
        value = value,
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

      # TF1/2 - unsure if we need this any more
      # self$sanitise_dtypes()

      optimise_fun <- eval(parse(text = self$method))

      tfe$tf_optimiser <- do.call(
        optimise_fun,
        self$parameters
      )

      self$minimiser_function <- function(inits) {

        free_state <- tf$Variable(inits)

        objective_adjusted <- function(){
          -dag$tf_log_prob_function(free_state)$adjusted
        }

        objective_unadjusted <- function(){
          -dag$tf_log_prob_function(free_state)$unadjusted
        }

        if (self$adjust) {
          tfe$train <- tfe$tf_optimiser$minimize(
            objective_adjusted,
            var_list = list(free_state)
          )

        } else {
          tfe$train <- tfe$tf_optimiser$minimize(
            objective_unadjusted,
            var_list = list(free_state)
          )
        }
        browser()
        tfe$free_state <- free_state
      }

      #
      #       free_state <- tf$Variable(self$free_state)
      #
      #       objective_adjusted <- function(){
      #         -dag$tf_log_prob_function(free_state)$adjusted
      #       }
      #
      #       objective_unadjusted <- function(){
      #         -dag$tf_log_prob_function(free_state)$adjusted
      #       }
      #
      #       if (self$adjust) {
      #           browser()
      #         # dag$tf_run(train <- tf_optimiser$minimize(optimiser_objective_adj))
      #
      #         tfe$train <- tfe$tf_optimiser$minimize(
      #           objective_adjusted,
      #           var_list = list(free_state)
      #         )
      #
      #         } else {
      #         # dag$tf_run(train <- tf_optimiser$minimize(optimiser_objective))
      #
      #           tfe$train <- tfe$tf_optimiser$minimize(
      #             objective_unadjusted,
      #             var_list = list(free_state)
      #           )
      #         }
      #
      #       # add the updated free state into TFE
      #       tfe$free_state <- free_state
    },

    # minimise the objective function
    run_minimiser = function() {
      self$set_inits()
      tfe <- self$model$dag$tf_environment

      # browser() <<
      ## TF1/2 - need to get have some way of getting the number of iterations
      ## out from the train
      while (self$it < self$max_iterations &
             self$diff > self$tolerance) {
        self$it <- self$it + 1
        # self$model$dag$tf_sess_run(train)
        tfe$train
        if (self$adjust) {
          # obj <- self$model$dag$tf_sess_run(optimiser_objective_adj)
          obj <- tfe$optimiser_objective_adj
        } else {
          # obj <- self$model$dag$tf_sess_run(optimiser_objective)
          obj <- tfe$optimiser_objective
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

    create_tfp_minimiser = function() {
      # browser() <<
      dag <- self$model$dag
      tfe <- dag$tf_environment

      self$sanitise_dtypes()

      optimise_fun <- eval(parse(text = self$method))
      # dag$on_graph(
      # TF1/2 - need to work out how to appropriately call the TFP function
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

      # create an op to minimise the objective
      self$minimiser_function <- function(inits) {

        free_state <- tf$Variable(inits)

        objective_adjusted <- function(){
          -dag$tf_log_prob_function(free_state)$adjusted
        }

        objective_unadjusted <- function(){
          -dag$tf_log_prob_function(free_state)$adjusted
        }

        if (self$adjust) {
          tfe$train <- tfe$tf_optimiser$minimize(
            objective_adjusted,
            var_list = list(free_state)
          )

        } else {
          tfe$train <- tfe$tf_optimiser$minimize(
            objective_unadjusted,
            var_list = list(free_state)
          )
        }

        tfe$free_state <- free_state
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

# implement an S3 method to handle dispatching the optimisation method based
# on the class. Should also allow for building other methods in the future
run_optimiser <- function(self){
  UseMethod("run_optimiser")
}

## TF1/2 Change from `create_tf_minimiser` as it runs the minimisation
run_optimiser.tf_optimiser <- function(self){
  self$create_tf_minimiser()
}

run_optimiser.tfp_optimiser <- function(self){
  self$create_tfp_minimiser()
}

