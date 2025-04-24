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
    warmup = 1,

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
    initialize = function(
      initial_values,
      model,
      parameters = list(),
      seed,
      compute_options
    ) {
      # initialize the inference method
      super$initialize(
        initial_values = initial_values,
        model = model,
        parameters = parameters,
        seed = seed
      )

      self$n_chains <- nrow(self$free_state)

      # duplicate diag_sd if needed
      ## TODO improve explaining variable here - why does this need to happen?
      n_diag <- length(self$parameters$diag_sd)
      n_parameters <- self$n_free
      multiple_parameters <- n_parameters > 1
      if (n_diag != n_parameters && multiple_parameters) {
        diag_sd <- rep(self$parameters$diag_sd[1], n_parameters)
        self$parameters$diag_sd <- diag_sd
      }

      # define the draws tensor on the tf graph
      # define_tf_draws is now used in place of of run_burst
      self$define_tf_evaluate_sample_batch()
    },

    define_tf_evaluate_sample_batch = function() {
      self$tf_evaluate_sample_batch <- tensorflow::tf_function(
        f = self$define_tf_draws,
        input_signature = list(
          # free state
          tf$TensorSpec(shape = list(NULL, self$n_free), dtype = tf_float()),
          # sampler_burst_length
          tf$TensorSpec(shape = list(), dtype = tf$int32),
          # sampler_thin
          tf$TensorSpec(shape = list(), dtype = tf$int32),
          # sampler_param_vec
          tf$TensorSpec(
            shape = list(
              length(
                unlist(
                  self$sampler_parameter_values()
                )
              )
            ),
            dtype = tf_float()
          )
        )
      )
    },

    run_chain = function(
      n_samples,
      thin,
      warmup,
      verbose,
      pb_update,
      one_by_one,
      plan_is,
      n_cores,
      float_type,
      trace_batch_size,
      from_scratch = TRUE
    ) {
      self$warmup <- warmup
      self$thin <- thin
      dag <- self$model$dag

      # set the number of cores
      dag$n_cores <- n_cores

      if (!plan_is$parallel & verbose) {
        self$print_sampler_number()
      }
      if (plan_is$parallel) {
        dag$define_tf_trace_values_batch()

        dag$define_tf_log_prob_function()

        self$define_tf_evaluate_sample_batch()
      }

      # create these objects if needed
      if (from_scratch) {
        self$traced_free_state <- self$empty_matrices(
          n = self$n_chains,
          ncol = self$n_free
        )

        self$traced_values <- self$empty_matrices(
          n = self$n_chains,
          ncol = self$n_traced
        )
      }

      # how big would we like the bursts to be
      ideal_burst_size <- ifelse(one_by_one, 1L, pb_update)
      do_warmup <- self$warmup > 0
      if (do_warmup) {
        self$run_warmup(
          n_samples = n_samples,
          pb_update = pb_update,
          ideal_burst_size = ideal_burst_size,
          verbose = verbose
        )
      }

      self$run_sampling(
        n_samples = n_samples,
        pb_update = pb_update,
        ideal_burst_size = ideal_burst_size,
        trace_batch_size = trace_batch_size,
        thin = thin,
        verbose = verbose
      )

      # return self, to send results back when running in parallel
      self
    },

    run_warmup = function(
      n_samples,
      pb_update,
      ideal_burst_size,
      verbose
    ) {
      perform_warmup <- self$warmup > 0
      if (perform_warmup) {
        if (verbose) {
          pb_warmup <- create_progress_bar(
            phase = "warmup",
            iter = c(self$warmup, n_samples),
            pb_update = pb_update,
            width = self$pb_width
          )

          iterate_progress_bar(
            pb = pb_warmup,
            it = 0,
            rejects = 0,
            chains = self$n_chains,
            file = self$pb_file
          )
        } else {
          pb_warmup <- NULL
        }

        # split up warmup iterations into bursts of sampling
        burst_lengths <- self$burst_lengths(
          self$warmup,
          ideal_burst_size,
          warmup = TRUE
        )

        completed_iterations <- cumsum(burst_lengths)

        # relay between R and tensorflow in a burst to be cpu efficient
        for (burst in seq_along(burst_lengths)) {
          # TF1/2 check todo?
          # replace with define_tf_draws

          self$run_burst(n_samples = burst_lengths[burst])
          # align the free state back to the parameters we are tracing
          # TF1/2 check todo?
          # this is the tuning stage, might not need to evaluate
          # / record the parameter values, as they will be thrown away
          # after warmup - so could remove trace here.

          self$trace()
          # a memory efficient way to calculate summary stats of samples
          self$update_welford()
          self$tune(completed_iterations[burst], self$warmup)

          if (verbose) {
            # update the progress bar/percentage log
            iterate_progress_bar(
              pb = pb_warmup,
              it = completed_iterations[burst],
              rejects = self$numerical_rejections,
              chains = self$n_chains,
              file = self$pb_file
            )

            self$write_percentage_log(
              total = self$warmup,
              completed = completed_iterations[burst],
              stage = "warmup"
            )
          }
        }

        # scrub the free state trace and numerical rejections
        self$traced_free_state <- self$empty_matrices(
          n = self$n_chains,
          ncol = self$n_free
        )

        self$numerical_rejections <- 0
      } # end warmup
    },

    run_sampling = function(
      n_samples,
      pb_update,
      ideal_burst_size,
      trace_batch_size,
      thin,
      verbose
    ) {
      perform_sampling <- n_samples > 0
      if (perform_sampling) {
        # on exiting during the main sampling period (even if killed by the
        # user) trace the free state values

        on.exit(self$trace_values(trace_batch_size), add = TRUE)

        # main sampling
        if (verbose) {
          pb_sampling <- create_progress_bar(
            phase = "sampling",
            iter = c(self$warmup, n_samples),
            pb_update = pb_update,
            width = self$pb_width
          )
          iterate_progress_bar(
            pb = pb_sampling,
            it = 0,
            rejects = 0,
            chains = self$n_chains,
            file = self$pb_file
          )
        } else {
          pb_sampling <- NULL
        }

        # split up warmup iterations into bursts of sampling
        burst_lengths <- self$burst_lengths(n_samples, ideal_burst_size)
        completed_iterations <- cumsum(burst_lengths)

        for (burst in seq_along(burst_lengths)) {
          # so these bursts are R objects being passed through to python
          # and how often to return them
          # TF1/2 check todo
          # replace with define_tf_draws
          self$run_burst(n_samples = burst_lengths[burst], thin = thin)
          # trace is it receiving the python
          self$trace()

          if (verbose) {
            # update the progress bar/percentage log
            iterate_progress_bar(
              pb = pb_sampling,
              it = completed_iterations[burst],
              rejects = self$numerical_rejections,
              chains = self$n_chains,
              file = self$pb_file
            )

            self$write_percentage_log(
              total = n_samples,
              completed = completed_iterations[burst],
              stage = "sampling"
            )
          }
        }
      } # end sampling
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
      self$traced_values <- lapply(
        self$traced_free_state,
        self$model$dag$trace_values,
        trace_batch_size = trace_batch_size
      )
    },

    # print the sampler number (if relevant)
    print_sampler_number = function() {
      msg <- ""

      if (self$n_samplers > 1) {
        msg <- glue::glue(
          "\n\nsampler {self$sampler_number}/{self$n_samplers}"
        )
      }

      if (self$n_chains > 1) {
        n_cores <- self$model$dag$n_cores
        compute_options <- self$compute_options

        cores_text <- compute_text(n_cores, compute_options)

        msg <- glue::glue(
          "\n\nrunning {self$n_chains} chains simultaneously {cores_text}"
        )
      }

      if (!identical(msg, "")) {
        cli::cli_inform(msg)
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
    # TF1/2 check todo
    # need to convert this into a TF function
    define_tf_draws = function(
      free_state,
      sampler_burst_length,
      sampler_thin,
      sampler_param_vec
      # pass values through
    ) {
      dag <- self$model$dag
      tfe <- dag$tf_environment

      # TF1/2 check seed
      # how do TF2 and TFP use seeds?
      self$set_tf_seed()

      sampler_kernel <- self$define_tf_kernel(
        sampler_param_vec
      )

      # TF1/2 check
      # some sampler parameter values need to be re-run at each iteration to
      # decide, e.g., the leap step in HMC, which is run inside define_tf_kernel
      # currently we run `sample_parameter_values` which will randomly pick
      # an "l" step.
      # Need to understand if/how tf_function will re-run those values - might
      # need to pass these arguments directly

      # define the whole draws tensor
      # TF1/2 check
      # `seed` arg now gets passed to `sample_chain`.
      # Need to work out how to get sampler_batch() to run as a TF function.
      # To do that we need to work out how to get the free state

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
      return(
        sampler_batch
      )
    },

    # run a burst of the sampler
    # TF1/2 check
    # this will be removed in favour of the tf_function decorated
    # define_tf_draws() function that takes in argument values
    # sampler_burst_length and sampler_thin
    run_burst = function(n_samples, thin = 1L) {
      dag <- self$model$dag
      tfe <- dag$tf_environment

      param_vec <- unlist(self$sampler_parameter_values())
      # combine the sampler information with information on the sampler's tuning
      # parameters, and make into a dict

      dag$set_tf_data_list("batch_size", nrow(self$free_state))

      # run the sampler, handling numerical errors
      batch_results <- self$sample_carefully(
        free_state = self$free_state,
        sampler_burst_length = as.integer(n_samples),
        sampler_thin = as.integer(thin),
        sampler_param_vec = param_vec
      )

      # get trace of free state and drop the null dimension
      if (is.null(batch_results$all_states)) {
        browser()
      }
      free_state_draws <- as.array(batch_results$all_states)

      # if there is one sample at a time, and it's rejected, conversion from
      # python back to R can drop a dimension, so handle that here. Ugh.
      if (n_dim(free_state_draws) != 3) {
        dim(free_state_draws) <- c(1, dim(free_state_draws))
      }

      self$last_burst_free_states <- split_chains(free_state_draws)

      n_draws <- nrow(free_state_draws)
      if (n_draws > 0) {
        free_state <- free_state_draws[n_draws, , , drop = FALSE]
        dim(free_state) <- dim(free_state)[-1]
        self$free_state <- free_state
      }

      self$update_acceptance(results = batch_results$trace)
      self$update_rejection(results = batch_results$trace)
    },

    update_acceptance = function(results) {
      if (self$uses_metropolis) {
        # log acceptance probability
        log_accept_stats <- as.array(results$log_accept_ratio)
        is_accepted <- as.array(results$is_accepted)
        # related to tuning information
        self$accept_history <- rbind(self$accept_history, is_accepted)
        accept_stats_batch <- pmin(1, exp(log_accept_stats))
        # also related to tuning information
        self$mean_accept_stat <- mean(accept_stats_batch, na.rm = TRUE)
      }
    },

    update_rejection = function(results) {
      if (self$uses_metropolis) {
        log_accept_stats <- as.array(results$log_accept_ratio)
        # numerical rejections parameter sets
        bad <- sum(!is.finite(log_accept_stats))
        self$numerical_rejections <- self$numerical_rejections + bad
      }
    },

    tf_evaluate_sample_batch = NULL,

    sample_carefully = function(
      free_state,
      sampler_burst_length,
      sampler_thin,
      sampler_param_vec
    ) {
      # tryCatch handling for numerical errors
      dag <- self$model$dag
      tfe <- dag$tf_environment
      # legacy: previously we used `n_samples` not `sampler_burst_length`
      n_samples <- sampler_burst_length

      result <- cleanly(
        self$tf_evaluate_sample_batch(
          free_state = tensorflow::as_tensor(
            free_state,
            dtype = tf_float()
          ),
          sampler_burst_length = tensorflow::as_tensor(sampler_burst_length),
          sampler_thin = tensorflow::as_tensor(sampler_thin),
          sampler_param_vec = tensorflow::as_tensor(
            sampler_param_vec,
            dtype = tf_float(),
            shape = length(sampler_param_vec)
          )
        )
      ) # closing cleanly

      # if it's fine, batch_results is the output
      # if it's a non-numerical error, it will error
      # if it's a numerical error, batch_results will be an error object
      self$check_for_free_state_error(result, n_samples)

      result
    },

    check_for_free_state_error = function(result, n_samples) {
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
          greta_stash$tf_num_error <- result

          # otherwise, *one* of these multiple samples was bad. The sampler
          # won't be valid if we just restart, so we need to error here,
          # informing the user how to run one sample at a time
          cli::cli_abort(
            message = c(
              "TensorFlow hit a numerical problem that caused it to error",
              "{.pkg greta} can handle these as bad proposals if you rerun \\
              {.fun mcmc} with the argument {.code one_by_one = TRUE}.",
              "This will slow down the sampler slightly.",
              "The error encountered can be recovered and viewed with:",
              "{.code greta_notes_tf_num_error()}"
            )
          )
        }
      }
    },

    sampler_parameter_values = function() {
      # random number of integration steps
      self$parameters
    },
    empty_matrices = function(n, ncol) {
      replicate(
        n = n,
        matrix(data = NA, nrow = 0, ncol = ncol),
        simplify = FALSE
      )
    }
  )
)


tune_tf <- R6Class(
  "tune_tf",
  inherit = sampler
)

tune_r <- R6Class(
  "tune_r",
  inherit = sampler
)
