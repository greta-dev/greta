# R6 inference class objects

# base node class
inference <- R6Class(
  "inference",
  public = list(
    model = NULL,

    # compute information
    compute_options = NULL,

    # RNG seed
    seed = 1,

    # size and current value of the free state
    n_free = 1L,
    free_state = 0,

    # and of the traced values
    n_traced = 1L,

    # where to write the traced values to
    trace_log_file = NULL,
    parameters = list(),
    tuning_periods = list(),

    # free state values for the last burst
    last_burst_free_states = list(),
    # all recorded free state values
    traced_free_state = list(),
    # all recorded greta array values
    traced_values = list(),
    initialize = function(
      initial_values,
      model,
      parameters = list(),
      seed = get_seed()
    ) {
      self$parameters <- parameters
      self$model <- model
      free_parameters <- model$dag$example_parameters(free = TRUE)
      free_parameters <- unlist_tf(free_parameters)
      self$n_free <- length(free_parameters)
      self$set_initial_values(initial_values)
      # maybe inefficient-  potentially speed up?

      self$n_traced <- length(model$dag$trace_values(self$free_state))
      self$seed <- seed
    },

    # Write burst values to log file; appends if the file exists, creates it if
    # not.
    write_trace_to_log_file = function(last_burst_values) {
      if (file.exists(self$trace_log_file)) {
        # Append
        write.table(
          last_burst_values,
          self$trace_log_file,
          append = TRUE,
          row.names = FALSE,
          col.names = FALSE
        )
      } else {
        # Create file
        write.table(
          last_burst_values,
          self$trace_log_file,
          append = FALSE,
          row.names = FALSE,
          col.names = TRUE
        )
      }
    },

    # write the percentage progress to a file
    write_percentage_log = function(total, completed, stage) {
      if (!is.null(self$percentage_file)) {
        percentage <- round(100 * completed / total)
        msg <- glue::glue(
          "{stage} {percentage}%"
        )
        writeLines(msg, self$percentage_file)
      }
    },

    # set RNG seed for a tensorflow graph. Must be done before definition of a
    # random tensor
    set_tf_seed = function() {
      dag <- self$model$dag
      dag$tf_environment$rng_seed <- self$seed
    },

    # check and try to autofill a single set of initial values (single vector on
    # free state scale)
    check_initial_values = function(inits, call = rlang::caller_env()) {
      undefined <- is.na(inits)

      # try to fill in any that weren't specified
      if (any(undefined)) {
        n_missing <- sum(undefined)

        valid <- FALSE
        attempts <- 1
        while (!valid & attempts < 20) {
          inits[undefined] <- rnorm(n_missing, 0, 0.1)

          # test validity of values
          valid <- self$valid_parameters(inits)
          attempts <- attempts + 1
        }

        self$check_reasonable_starting_values(valid, attempts)
      } else {
        # if they were all provided, check they can be be used
        valid <- self$valid_parameters(inits)
        self$check_valid_parameters(valid)
      }

      inits
    },

    check_reasonable_starting_values = function(valid, attempts) {
      if (!valid) {
        cli::cli_abort(
          message = c(
            "Could not find reasonable starting values after \\
              {attempts} attempts.",
            "Please specify initial values manually via the \\
              {.arg initial_values} argument"
          )
        )
      }
    },

    check_valid_parameters = function(valid) {
      if (!valid) {
        cli::cli_abort(
          c(
            "The log density could not be evaluated at these initial values",
            "Try using these initials as the {.arg values} argument in \\
              {.fun calculate} to see what values of subsequent \\
              {.cls greta_array}s these initial values lead to."
          )
        )
      }
    },

    # check and set a list of initial values
    set_initial_values = function(init_list) {
      # check/autofill them
      init_list <- lapply(init_list, self$check_initial_values)

      # combine into a matrix
      inits <- do.call(rbind, init_list)

      # set them as the state
      self$free_state <- inits
    },

    # check whether the model can be evaluated at these parameters
    valid_parameters = function(parameters) {
      dag <- self$model$dag
      tf_parameters <- fl(array(
        data = parameters,
        dim = c(1, length(parameters))
      ))
      ld <- lapply(
        dag$tf_log_prob_function(tf_parameters),
        as.numeric
      )
      is.finite(ld$adjusted) && is.finite(ld$unadjusted)
    },

    # run a burst of sampling, and put the resulting free state values in
    # last_burst_free_states
    run_burst = function() {
      cli::cli_abort(
        "no method to run a burst in the base inference class"
      )
      self$last_burst_free_states <- free_states
    },

    # store the free state, and/or corresponding values of the target greta
    # arrays for the latest batch of raw draws
    trace = function(free_state = TRUE, values = FALSE) {
      if (free_state) {
        # append the free state trace for each chain
        self$traced_free_state <- mapply(
          rbind,
          self$traced_free_state,
          self$last_burst_free_states,
          SIMPLIFY = FALSE
        )
      }

      if (values) {
        # calculate the observed values
        last_burst_values <- self$trace_burst_values()
        if (!is.null(self$trace_log_file)) {
          self$write_trace_to_log_file(last_burst_values)
        }
        self$traced_values <- mapply(
          rbind,
          self$traced_values,
          last_burst_values,
          SIMPLIFY = FALSE
        )
      }
    },

    # given a matrix of free state values, get a matrix of values of the target
    # greta arrays
    trace_burst_values = function(free_states = self$last_burst_free_states) {
      # can't use apply directly, as it will drop the variable name if there's
      # only one parameter being traced
      values_trace <- lapply(
        free_states,
        self$model$dag$trace_values
      )

      values_trace
    },

    # is the sampler in one of the tuning periods for a given parameter
    in_periods = function(periods, i, n_samples) {
      within <- function(period, fraction) {
        fraction > period[1] & fraction <= period[2]
      }

      fraction <- i / n_samples
      in_period <- vapply(periods, within, fraction, FUN.VALUE = FALSE)
      any(in_period)
    }
  )
)
