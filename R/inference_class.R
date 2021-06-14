# R6 inference class objects

# base node class
inference <- R6Class(
  "inference",
  public = list(
    model = NULL,

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
    initialize = function(initial_values,
                          model,
                          parameters = list(),
                          seed = get_seed()) {

      # flush the environment and redefine the tensorflow graph if needed
      if (is.null(model$dag$tf_graph$unique_name)) {
        model$dag$new_tf_environment()
        model$dag$define_tf()
      }

      self$parameters <- parameters
      self$model <- model
      free_parameters <- model$dag$example_parameters(free = TRUE)
      free_parameters <- unlist_tf(free_parameters)
      self$n_free <- length(free_parameters)
      self$set_initial_values(initial_values)
      self$n_traced <- length(model$dag$trace_values(self$free_state))
      self$seed <- seed
    },

    # Write burst values to log file; appends if the file exists, creates it if
    # not.
    write_trace_to_log_file = function(last_burst_values) {
      if (file.exists(self$trace_log_file)) {
        # Append
        write.table(last_burst_values, self$trace_log_file,
          append = TRUE,
          row.names = FALSE, col.names = FALSE
        )
      } else {
        # Create file
        write.table(last_burst_values, self$trace_log_file,
          append = FALSE,
          row.names = FALSE, col.names = TRUE
        )
      }
    },

    # write the percentage progress to a file
    write_percentage_log = function(total, completed, stage) {
      if (!is.null(self$percentage_file)) {
        percentage <- round(100 * completed / total)
        msg <- sprintf(
          "%s %i%%",
          stage,
          percentage
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
    check_initial_values = function(inits) {
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

        if (!valid) {
          msg <- cli::format_error(
            c(
              "Could not find reasonable starting values after \\
              {attempts}, attempts.",
              "Please specify initial values manually via the \\
              {.arg initial_values} argument"
            )
          )
          stop(
            msg,
            call. = FALSE
          )
        }
      } else {

        # if they were all provided, check they can be be used
        valid <- self$valid_parameters(inits)
        if (!valid) {
          msg <- cli::format_error(
            c(
              "The log density could not be evaluated at these initial values",
              "Try using {.fun calculate} to see whether they lead to values \\
              of other greta arrays in the model."
            )
          )
          stop(
            msg,
            call. = FALSE
          )
        }
      }

      inits
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
      tfe <- dag$tf_environment

      if (!live_pointer("joint_density_adj", envir = tfe)) {
        dag$on_graph(dag$define_joint_density())
      }

      dag$send_parameters(parameters)
      ld <- self$model$dag$log_density()
      is.finite(ld)
    },

    # run a burst of sampling, and put the resulting free state values in
    # last_burst_free_states
    run_burst = function() {
      msg <- cli::format_error(
        "no method to run a burst in the base inference class"
      )
      stop(
        msg,
        call. = FALSE
      )
      self$last_burst_free_states <- free_states
    },

    # store the free state, and/or corresponding values of the target greta
    # arrays for the latest batch of raw draws
    trace = function(free_state = TRUE, values = FALSE) {
      if (free_state) {

        # append the free state trace for each chain
        self$traced_free_state <- mapply(rbind,
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
        self$traced_values <- mapply(rbind,
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

#' @importFrom coda mcmc mcmc.list
sampler <- R6Class(
  "sampler",
  inherit = inference,
  public = list(

    # sampler information
    sampler_number = 1,
    n_samplers = 1,
    n_chains = 1,
    numerical_rejections = 0,
    thin = 1,

    # tuning information
    mean_accept_stat = 0.5,
    sum_epsilon_trace = NULL,
    hbar = 0,
    log_epsilon_bar = 0,
    tuning_interval = 3,
    uses_metropolis = TRUE,
    welford_state = list(
      count = 0,
      mean = 0,
      m2 = 0
    ),
    accept_target = 0.5,
    accept_history = NULL,

    # sampler kernel information
    parameters = list(
      epsilon = 0.1,
      diag_sd = 1
    ),

    # parallel progress reporting
    percentage_file = NULL,
    pb_file = NULL,
    pb_width = options()$width,

    # batch sizes for tracing
    trace_batch_size = 100,
    initialize = function(initial_values,
                          model,
                          parameters = list(),
                          seed) {

      # initialize the inference method
      super$initialize(
        initial_values = initial_values,
        model = model,
        parameters = parameters,
        seed = seed
      )

      self$n_chains <- nrow(self$free_state)

      # duplicate diag_sd if needed
      n_diag <- length(self$parameters$diag_sd)
      n_parameters <- self$n_free
      if (n_diag != n_parameters && n_parameters > 1) {
        diag_sd <- rep(self$parameters$diag_sd[1], n_parameters)
        self$parameters$diag_sd <- diag_sd
      }

      # define the draws tensor on the tf graph
      self$define_tf_draws()
    },
    run_chain = function(n_samples, thin, warmup,
                         verbose, pb_update,
                         one_by_one, plan_is, n_cores, float_type,
                         trace_batch_size,
                         from_scratch = TRUE) {
      self$thin <- thin
      dag <- self$model$dag

      # set the number of cores
      dag$n_cores <- n_cores

      if (!plan_is$parallel & verbose) {
        self$print_sampler_number()
      }

      if (plan_is$parallel) {

        # flush the environment
        dag$new_tf_environment()

        # set the batch size for multiple chains
        dag$set_tf_data_list("batch_size", self$n_chains)

        # rebuild the TF graph
        dag$define_tf()

        # rebuild the TF draws tensor
        self$define_tf_draws()
      }

      # create these objects if needed
      if (from_scratch) {
        self$traced_free_state <- replicate(self$n_chains,
          matrix(NA, 0, self$n_free),
          simplify = FALSE
        )

        self$traced_values <- replicate(self$n_chains,
          matrix(NA, 0, self$n_traced),
          simplify = FALSE
        )
      }

      # how big would we like the bursts to be
      ideal_burst_size <- ifelse(one_by_one, 1L, pb_update)

      # if warmup is required, do that now
      if (warmup > 0) {
        if (verbose) {
          pb_warmup <- create_progress_bar(
            "warmup",
            c(warmup, n_samples),
            pb_update,
            self$pb_width
          )

          iterate_progress_bar(pb_warmup, 0, 0, self$n_chains, self$pb_file)
        } else {
          pb_warmup <- NULL
        }

        # split up warmup iterations into bursts of sampling
        burst_lengths <- self$burst_lengths(warmup,
          ideal_burst_size,
          warmup = TRUE
        )
        completed_iterations <- cumsum(burst_lengths)

        for (burst in seq_along(burst_lengths)) {
          self$run_burst(burst_lengths[burst])
          self$trace()
          self$update_welford()
          self$tune(completed_iterations[burst], warmup)

          if (verbose) {

            # update the progress bar/percentage log
            iterate_progress_bar(pb_warmup,
              it = completed_iterations[burst],
              rejects = self$numerical_rejections,
              chains = self$n_chains,
              file = self$pb_file
            )

            self$write_percentage_log(warmup,
              completed_iterations[burst],
              stage = "warmup"
            )
          }
        }

        # scrub the free state trace and numerical rejections
        self$traced_free_state <- replicate(self$n_chains,
          matrix(NA, 0, self$n_free),
          simplify = FALSE
        )
        self$numerical_rejections <- 0
      }

      if (n_samples > 0) {

        # on exiting during the main sampling period (even if killed by the
        # user) trace the free state values

        on.exit(self$trace_values(trace_batch_size), add = TRUE)

        # main sampling
        if (verbose) {
          pb_sampling <- create_progress_bar(
            "sampling",
            c(warmup, n_samples),
            pb_update,
            self$pb_width
          )
          iterate_progress_bar(pb_sampling, 0, 0, self$n_chains, self$pb_file)
        } else {
          pb_sampling <- NULL
        }

        # split up warmup iterations into bursts of sampling
        burst_lengths <- self$burst_lengths(n_samples, ideal_burst_size)
        completed_iterations <- cumsum(burst_lengths)

        for (burst in seq_along(burst_lengths)) {
          self$run_burst(burst_lengths[burst], thin = thin)
          self$trace()

          if (verbose) {

            # update the progress bar/percentage log
            iterate_progress_bar(pb_sampling,
              it = completed_iterations[burst],
              rejects = self$numerical_rejections,
              chains = self$n_chains,
              file = self$pb_file
            )

            self$write_percentage_log(n_samples,
              completed_iterations[burst],
              stage = "sampling"
            )
          }
        }
      }

      # return self, to send results back when running in parallel
      self
    },

    # update the welford accumulator for summary statistics of the posterior,
    # used for tuning
    update_welford = function() {

      # unlist the states into a matrix
      trace_matrix <- do.call(rbind, self$last_burst_free_states)

      count <- self$welford_state$count
      mean <- self$welford_state$mean
      m2 <- self$welford_state$m2

      for (i in seq_len(nrow(trace_matrix))) {
        new_value <- trace_matrix[i, ]

        count <- count + 1
        delta <- new_value - mean
        mean <- mean + delta / count
        delta2 <- new_value - mean
        m2 <- m2 + delta * delta2
      }

      self$welford_state <- list(
        count = count,
        mean = mean,
        m2 = m2
      )
    },
    sample_variance = function() {
      count <- self$welford_state$count
      m2 <- self$welford_state$m2
      m2 / (count - 1)
    },

    # convert traced free state to the traced values, accounting for
    # chain dimension
    trace_values = function(trace_batch_size) {
      self$traced_values <- lapply(self$traced_free_state,
        self$model$dag$trace_values,
        trace_batch_size = trace_batch_size
      )
    },

    # print the sampler number (if relevant)
    print_sampler_number = function() {
      msg <- ""

      if (self$n_samplers > 1) {
        msg <- sprintf(
          "\nsampler %i/%i",
          self$sampler_number,
          self$n_samplers
        )
      }

      if (self$n_chains > 1) {
        n_cores <- self$model$dag$n_cores

        cores_text <- ifelse(n_cores == 1,
          "1 core",
          sprintf("up to %i cores", n_cores)
        )

        msg <- sprintf(
          "\nrunning %i chains simultaneously on %s",
          self$n_chains,
          cores_text
        )
      }

      if (!identical(msg, "")) {
        cli::cli_alert_info(msg)
        cat("\n")
      }
    },

    # split the number of samples up into bursts of running the sampler,
    # considering the progress bar update frequency and the parameter tuning
    # schedule during warmup
    burst_lengths = function(n_samples, pb_update, warmup = FALSE) {

      # when to stop for progress bar updates
      changepoints <- c(seq(0, n_samples, by = pb_update), n_samples)

      if (warmup) {

        # when to break to update tuning
        tuning_points <- seq(0, n_samples, by = self$tuning_interval)

        # handle infinite tuning interval (for non-tuned mcmc)
        if (all(is.na(tuning_points))) {
          tuning_points <- c(0, n_samples)
        }

        changepoints <- c(changepoints, tuning_points)
      }

      changepoints <- sort(unique(changepoints))
      diff(changepoints)
    },

    # overall tuning method
    tune = function(iterations_completed, total_iterations) {
      self$tune_epsilon(iterations_completed, total_iterations)
      self$tune_diag_sd(iterations_completed, total_iterations)
    },
    tune_epsilon = function(iter, total) {

      # tuning periods for the tunable parameters (first 10%, last 60%)
      tuning_periods <- list(c(0, 0.1), c(0.4, 1))

      # whether we're tuning now
      tuning_now <- self$in_periods(
        tuning_periods,
        iter,
        total
      )

      if (tuning_now) {

        # epsilon & tuning parameters
        kappa <- 0.75
        gamma <- 0.1
        t0 <- 10
        mu <- log(t0 * 0.05)

        hbar <- self$hbar
        log_epsilon_bar <- self$log_epsilon_bar
        mean_accept_stat <- self$mean_accept_stat

        w1 <- 1 / (iter + t0)
        hbar <- (1 - w1) * hbar + w1 * (self$accept_target - mean_accept_stat)
        log_epsilon <- mu - hbar * sqrt(iter) / gamma
        w2 <- iter^-kappa
        log_epsilon_bar <- w2 * log_epsilon + (1 - w2) * log_epsilon_bar

        self$hbar <- hbar
        self$log_epsilon_bar <- log_epsilon_bar
        self$parameters$epsilon <- exp(log_epsilon)

        # if this is the end of the warmup, put the averaged epsilon back in for
        # the parameter
        if (iter == total) {
          self$parameters$epsilon <- exp(log_epsilon_bar)
        }
      }
    },
    tune_diag_sd = function(iterations_completed, total_iterations) {

      # when, during warmup, to tune this parameter (after epsilon, but stopping
      # before halfway through)
      tuning_periods <- list(c(0.1, 0.4))

      tuning_now <- self$in_periods(
        tuning_periods,
        iterations_completed,
        total_iterations
      )

      if (tuning_now) {
        n_accepted <- sum(!self$accept_history)

        # provided there have been at least 5 acceptances in the warmup so far
        if (n_accepted > 5) {

          # get the sample posterior variance and shrink it
          sample_var <- self$sample_variance()
          shrinkage <- 1 / (n_accepted + 5)
          var_shrunk <- n_accepted * shrinkage * sample_var + 5e-3 * shrinkage
          self$parameters$diag_sd <- sqrt(var_shrunk)
        }
      }
    },
    define_tf_draws = function() {
      dag <- self$model$dag
      tfe <- dag$tf_environment

      self$set_tf_seed()

      self$define_tf_kernel()

      # and the sampler info
      dag$tf_run(
        sampler_burst_length <- tf$compat$v1$placeholder(dtype = tf$int32)
      )
      dag$tf_run(
        sampler_thin <- tf$compat$v1$placeholder(dtype = tf$int32)
      )

      # define the whole draws tensor
      dag$tf_run(
        sampler_batch <- tfp$mcmc$sample_chain(
          num_results = tf$math$floordiv(sampler_burst_length, sampler_thin),
          current_state = free_state,
          kernel = sampler_kernel,
          trace_fn = function(current_state, kernel_results) {
            kernel_results
          },
          num_burnin_steps = tf$constant(0L, dtype = tf$int32),
          num_steps_between_results = sampler_thin,
          parallel_iterations = 1L
        )
      )
    },

    # run a burst of the sampler
    run_burst = function(n_samples, thin = 1L) {
      dag <- self$model$dag
      tfe <- dag$tf_environment

      # combine the sampler information with information on the sampler's tuning
      # parameters, and make into a dict
      sampler_values <- list(
        free_state = self$free_state,
        sampler_burst_length = as.integer(n_samples),
        sampler_thin = as.integer(thin)
      )

      sampler_dict_list <- c(
        sampler_values,
        self$sampler_parameter_values()
      )

      dag$set_tf_data_list("batch_size", nrow(self$free_state))
      dag$build_feed_dict(sampler_dict_list)

      # run the sampler, handling numerical errors
      batch_results <- self$sample_carefully(n_samples)

      # get trace of free state and drop the null dimension
      free_state_draws <- batch_results$all_states

      # if there is one sample at a time, and it's rejected, conversion from
      # python back to R can drop a dimension, so handle that here. Ugh.
      if (length(dim(free_state_draws)) != 3) {
        dim(free_state_draws) <- c(1, dim(free_state_draws))
      }

      self$last_burst_free_states <- split_chains(free_state_draws)

      n_draws <- nrow(free_state_draws)
      if (n_draws > 0) {
        free_state <- free_state_draws[n_draws, , , drop = FALSE]
        dim(free_state) <- dim(free_state)[-1]
        self$free_state <- free_state
      }

      if (self$uses_metropolis) {

        # log acceptance probability
        log_accept_stats <- batch_results$trace$log_accept_ratio
        is_accepted <- batch_results$trace$is_accepted
        self$accept_history <- rbind(self$accept_history, is_accepted)
        accept_stats_batch <- pmin(1, exp(log_accept_stats))
        self$mean_accept_stat <- mean(accept_stats_batch, na.rm = TRUE)

        # numerical rejections parameter sets
        bad <- sum(!is.finite(log_accept_stats))
        self$numerical_rejections <- self$numerical_rejections + bad
      }
    },
    sample_carefully = function(n_samples) {

      # tryCatch handling for numerical errors
      dag <- self$model$dag
      tfe <- dag$tf_environment

      # don't use dag$tf_sess_run, to avoid the overhead on parsing expressions
      result <- cleanly(tfe$sess$run(tfe$sampler_batch,
        feed_dict = tfe$feed_dict
      ))

      # if it's fine, batch_results is the output
      # if it's a non-numerical error, it will error
      # if it's a numerical error, batch_results will be an error object
      if (inherits(result, "error")) {

        # simple case that this is a single bad sample. Mock up a result and
        # pass it back
        if (n_samples == 1L) {
          result <- list(
            all_states = self$free_state,
            trace = list(
              log_accept_ratio = rep(-Inf, self$n_chains),
              is_accepted = rep(FALSE, self$n_chains)
            )
          )
        } else {

          # otherwise, *one* of these multiple samples was bad. The sampler
          # won't be valid if we just restart, so we need to error here,
          # informing the user how to run one sample at a time
          msg <- cli::format_error(
            c(
              "TensorFlow hit a numerical problem that caused it to error",
              "{.pkg greta} can handle these as bad proposals if you rerun \\
              {.fun mcmc} with the argument {.code one_by_one = TRUE}.",
              "This will slow down the sampler slightly."
            )
          )

          stop(
            msg,
            result,
            call. = FALSE
          )
        }
      }

      result
    },
    sampler_parameter_values = function() {

      # random number of integration steps
      self$parameters
    }
  )
)

hmc_sampler <- R6Class(
  "hmc_sampler",
  inherit = sampler,
  public = list(
    parameters = list(
      Lmin = 10,
      Lmax = 20,
      epsilon = 0.005,
      diag_sd = 1
    ),
    accept_target = 0.651,
    define_tf_kernel = function() {
      dag <- self$model$dag
      tfe <- dag$tf_environment

      # tensors for sampler parameters
      dag$tf_run(
        hmc_epsilon <- tf$compat$v1$placeholder(dtype = tf_float())
      )
      dag$tf_run(
        hmc_l <- tf$compat$v1$placeholder(dtype = tf$int64)
      )

      # need to pass in the value for this placeholder as a matrix (shape(n, 1))
      dag$tf_run(
        hmc_diag_sd <- tf$compat$v1$placeholder(
          dtype = tf_float(),
          shape = shape(dim(free_state)[[2]], 1)
        )
      )

      # but it step_sizes must be a vector (shape(n, )), so reshape it
      dag$tf_run(
        hmc_step_sizes <- tf$reshape(
          hmc_epsilon * (hmc_diag_sd / tf$reduce_sum(hmc_diag_sd)),
          shape = shape(dim(free_state)[[2]])
        )
      )

      # log probability function
      tfe$log_prob_fun <- dag$generate_log_prob_function()

      # build the kernel
      # nolint start
      dag$tf_run(
        sampler_kernel <- tfp$mcmc$HamiltonianMonteCarlo(
          target_log_prob_fn = log_prob_fun,
          step_size = hmc_step_sizes,
          num_leapfrog_steps = hmc_l,
          seed = rng_seed
        )
      )
      # nolint end
    },
    sampler_parameter_values = function() {

      # random number of integration steps
      l_min <- self$parameters$Lmin
      l_max <- self$parameters$Lmax
      l <- sample(seq(l_min, l_max), 1)

      epsilon <- self$parameters$epsilon
      diag_sd <- matrix(self$parameters$diag_sd)

      # return named list for replacing tensors
      list(
        hmc_l = l,
        hmc_epsilon = epsilon,
        hmc_diag_sd = diag_sd
      )
    }
  )
)

rwmh_sampler <- R6Class(
  "rwmh_sampler",
  inherit = sampler,
  public = list(
    parameters = list(
      proposal = "normal",
      epsilon = 0.1,
      diag_sd = 1
    ),
    accept_target = 0.44,
    define_tf_kernel = function() {
      dag <- self$model$dag
      tfe <- dag$tf_environment
      tfe$rwmh_proposal <- switch(self$parameters$proposal,
        normal = tfp$mcmc$random_walk_normal_fn,
        uniform = tfp$mcmc$random_walk_uniform_fn
      )

      tfe$log_prob_fun <- dag$generate_log_prob_function()

      # tensors for sampler parameters
      dag$tf_run(
        rwmh_epsilon <- tf$compat$v1$placeholder(dtype = tf_float())
      )

      # need to pass in the value for this placeholder as a matrix (shape(n, 1))
      dag$tf_run(
        rwmh_diag_sd <- tf$compat$v1$placeholder(
          dtype = tf_float(),
          shape = shape(dim(free_state)[[2]], 1)
        )
      )

      # but it step_sizes must be a vector (shape(n, )), so reshape it
      dag$tf_run(
        rwmh_step_sizes <- tf$reshape(
          rwmh_epsilon * (rwmh_diag_sd / tf$reduce_sum(rwmh_diag_sd)),
          shape = shape(dim(free_state)[[2]])
        )
      )

      dag$tf_run(
        new_state_fn <- rwmh_proposal(scale = rwmh_step_sizes)
      )

      # build the kernel
      # nolint start
      dag$tf_run(
        sampler_kernel <- tfp$mcmc$RandomWalkMetropolis(
          target_log_prob_fn = log_prob_fun,
          new_state_fn = new_state_fn,
          seed = rng_seed
        )
      )
      # nolint end
    },
    sampler_parameter_values = function() {
      epsilon <- self$parameters$epsilon
      diag_sd <- matrix(self$parameters$diag_sd)

      # return named list for replacing tensors
      list(
        rwmh_epsilon = epsilon,
        rwmh_diag_sd = diag_sd
      )
    }
  )
)

slice_sampler <- R6Class(
  "slice_sampler",
  inherit = sampler,
  public = list(
    parameters = list(
      max_doublings = NA
    ),
    tuning_interval = Inf,
    uses_metropolis = FALSE,
    define_tf_kernel = function() {
      dag <- self$model$dag
      tfe <- dag$tf_environment

      if (dag$tf_float != "float32") {
        msg <- cli::format_error(
          c(
            "slice sampler can only currently be used for models defined with \\
            single precision",
            "set {.code model(..., precision = 'single')} instead"
          )
        )
        stop(
          msg,
          call. = FALSE
        )
      }

      tfe$log_prob_fun <- dag$generate_log_prob_function()
      dag$tf_run(
        slice_max_doublings <- tf$compat$v1$placeholder(dtype = tf$int32)
      )

      # build the kernel
      # nolint start
      dag$tf_run(
        sampler_kernel <- tfp$mcmc$SliceSampler(
          target_log_prob_fn = log_prob_fun,
          step_size = fl(1),
          max_doublings = slice_max_doublings,
          seed = rng_seed
        )
      )
      # nolint end
    },
    sampler_parameter_values = function() {
      max_doublings <- as.integer(self$parameters$max_doublings)

      # return named list for replacing tensors
      list(slice_max_doublings = max_doublings)
    },

    # no additional here tuning
    tune = function(iterations_completed, total_iterations) {

    }
  )
)

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
    set_dtype = function(parameter_name, dtype) {
      params <- self$parameters
      param_names <- self$parameter_names()

      if (parameter_name %in% param_names) {
        param <- params[[parameter_name]]
        self$model$dag$on_graph(
          tf_param <- tf$constant(param, dtype = dtype)
        )
        params[[parameter_name]] <- tf_param
      }

      self$parameters <- params
    },

    # initialize the variables, then set the ones we care about
    set_inits = function() {
      dag <- self$model$dag
      tfe <- dag$tf_environment

      dag$tf_sess_run(tf$compat$v1$global_variables_initializer())

      shape <- tfe$optimiser_free_state$shape
      dag$on_graph(
        tfe$optimiser_init <- tf$constant(self$free_state,
          shape = shape,
          dtype = tf_float()
        )
      )

      . <- dag$tf_sess_run(optimiser_free_state$assign(optimiser_init))
    },

    # create a separate free state variable and objective, since optimisers must
    # use variables
    create_optimiser_objective = function() {
      dag <- self$model$dag
      tfe <- dag$tf_environment

      # define a *variable* free state object
      if (!live_pointer("optimiser_free_state", envir = tfe)) {
        dag$define_free_state("variable", name = "optimiser_free_state")
      }

      # use the log prob function to define objectives from the variable
      if (!live_pointer("optimiser_objective_adj", envir = tfe)) {
        log_prob_fun <- dag$generate_log_prob_function(which = "both")
        dag$on_graph(objectives <- log_prob_fun(tfe$optimiser_free_state))

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
      dag <- self$model$dag
      tfe <- dag$tf_environment

      self$sanitise_dtypes()

      optimise_fun <- eval(parse(text = self$method))
      dag$on_graph(tfe$tf_optimiser <- do.call(
        optimise_fun,
        self$parameters
      ))

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

      dag$on_graph(tfe$tf_optimiser <- do.call(opt_fun, args))
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
