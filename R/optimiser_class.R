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
    run_minimiser = NULL,

    # modified during optimisation
    it = 0,
    old_obj = Inf,
    diff = Inf,

    # set up the model
    initialize = function(
    initial_values,
    model,
    name,
    method,
    parameters,
    other_args,
    max_iterations,
    tolerance,
    adjust
    ) {
      super$initialize(
        initial_values,
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
      run_optimiser(self)
    },
    parameter_names = function() {
      names(self$parameters)
    },

    run = function() {
      self$run_minimiser(self$free_state)
      self$fetch_free_state()
    },
    fetch_free_state = function() {
      self$free_state <- self$model$dag$tf_environment$free_state
    },
    return_outputs = function() {
      dag <- self$model$dag

      # if the optimiser was ignoring the callbacks, we have no idea about the
      # number of iterations or convergence
      if (!self$uses_callbacks) {
        self$it <- NA
      }

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

    # create an op to minimise the objective
    run_tf_minimiser = function() {
      dag <- self$model$dag
      tfe <- dag$tf_environment

      optimise_fun <- eval(parse(text = self$method))

      tfe$tf_optimiser <- do.call(
        optimise_fun,
        self$parameters
      )

      self$run_minimiser <- function(inits) {
        free_state <- tf$Variable(inits)

        objective_adjusted <- function() {
          -dag$tf_log_prob_function(free_state)$adjusted
        }

        objective_unadjusted <- function() {
          -dag$tf_log_prob_function(free_state)$unadjusted
        }

        # TF1/2 - get this to work inside TF with TF while loop
        while (self$it < self$max_iterations &
               all(self$diff > self$tolerance)) {
          # add 1 because python indexing
          self$it <- as.numeric(tfe$tf_optimiser$iterations) + 1

          if (self$adjust) {
            tfe$tf_optimiser$minimize(
              objective_adjusted,
              var_list = list(free_state)
            )
            obj_numeric <- objective_adjusted()$numpy()
          } else {
            tfe$tf_optimiser$minimize(
              objective_unadjusted,
              var_list = list(free_state)
            )
            obj_numeric <- objective_unadjusted()$numpy()
          }

          self$diff <- abs(self$old_obj - obj_numeric)
          self$old_obj <- obj_numeric
        }
        tfe$free_state <- free_state
      }
    }
  )
)

tfp_optimiser <- R6Class(
  "tfp_optimiser",
  inherit = optimiser,
  public = list(

    run_tfp_minimiser = function() {
      # browser()
      dag <- self$model$dag
      tfe <- dag$tf_environment

      optimise_fun <- eval(parse(text = self$method))

      if (self$adjust) {
        objective <- function(x) {
          -dag$tf_log_prob_function(x)$adjusted
        }
      } else {
        objective <- function(x) {
          -dag$tf_log_prob_function(x)$adjusted
        }
      }

      # bfgs uses value_and_gradient
      value_and_gradient <- function(x){
        tfp$math$value_and_gradient(
          function(x) objective(x),
          x
        )
      }

      self$run_minimiser <- function(inits) {

        # TF1/2 - will be better in the long run to have some kind of constructor
        # function or similar to implement this
        if (self$name == "bfgs") {
          self$parameters$value_and_gradients_function <- value_and_gradient
          self$parameters$initial_position <- inits
        } else if (self$name == "nelder_mead") {
          # because nelder_mead uses initial_vertex and not initial_position
          # this is a quick hack changing the argument in place by argument
          # position instead of by name ... for reasons
          # <<< browser()
          self$parameters$objective_function <- objective
          self$parameters$initial_vertex <- tf$constant(c(inits))
          self$parameters$batch_evaluate_objective <- TRUE
          # self$parameters$batch_evaluate_objective <- FALSE
        }

        tfe$tf_optimiser <- do.call(
          optimise_fun,
          self$parameters
        )

        self$it <- as.numeric(tfe$tf_optimiser$num_iterations)
        tfe$free_state <- tfe$tf_optimiser$position

      }

    }
  )
)

# implement an S3 method to handle dispatching the optimisation method based
# on the class. Should also allow for building other methods in the future
run_optimiser <- function(self) {
  UseMethod("run_optimiser")
}

run_optimiser.tf_optimiser <- function(self) {
  self$run_tf_minimiser()
}

run_optimiser.tfp_optimiser <- function(self) {
  self$run_tfp_minimiser()
}
