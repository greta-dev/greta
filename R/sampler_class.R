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
    n_bursts = 0L,
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
      super$initialize(
        initial_values = initial_values,
        model = model,
        parameters = parameters,
        seed = seed
      )

      self$n_chains <- nrow(self$free_state)

      # the kernels read one diag_sd per free parameter out of a flat vector,
      # and count the free parameters from its length, so a single diag_sd is
      # repeated for each of them
      has_diag_sd_per_parameter <- length(self$parameters$diag_sd) ==
        self$n_free
      if (!has_diag_sd_per_parameter) {
        self$parameters$diag_sd <- rep(self$parameters$diag_sd[1], self$n_free)
      }

      # wrapped in tf_function here so every burst reuses a single trace
      self$define_tf_evaluate_sample_batch()
    },

    define_tf_evaluate_sample_batch = function() {
      self$tf_evaluate_sample_batch <- tensorflow::tf_function(
        f = self$define_tf_draws,
        input_signature = list(
          # free state
          self$model$dag$free_state_signature()[[1]],
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
          ),
          # sampler_seed
          tf$TensorSpec(shape = list(2L), dtype = tf$int32)
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

      dag$n_cores <- n_cores

      if (!plan_is$parallel & verbose) {
        self$print_sampler_number()
      }
      if (plan_is$parallel) {
        # the worker gets the dag by serialisation, and a tf$Variable does not
        # survive that: the list arrives full of dead references. Clearing it
        # first makes define_data_variables() build new ones rather than try to
        # assign to the dead ones. greta-dev/greta#739
        dag$data_variables <- list()

        dag$define_tf_trace_values_batch()

        dag$define_tf_log_prob_function()

        self$define_tf_evaluate_sample_batch()
      }

      # extra_samples() appends to the trace the sampler already has
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

      self$run_warmup(
        n_samples = n_samples,
        pb_update = pb_update,
        ideal_burst_size = ifelse(one_by_one, 1L, pb_update),
        verbose = verbose
      )

      self$run_sampling(
        n_samples = n_samples,
        pb_update = pb_update,
        trace_batch_size = trace_batch_size,
        thin = thin,
        one_by_one = one_by_one,
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
          self$run_burst(n_samples = burst_lengths[burst])
          # this trace is scrubbed the moment warmup ends and nothing reads
          # it in between, so it is dead work: greta-dev/greta#834
          self$trace()
          # a memory efficient way to calculate summary stats of samples
          self$update_welford()
          self$tune(completed_iterations[burst], self$warmup)

          if (verbose) {
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

        # warmup's draws are not returned, and its numerical rejections are
        # not reported with sampling's
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
      trace_batch_size,
      thin,
      one_by_one,
      verbose
    ) {
      perform_sampling <- n_samples > 0
      if (perform_sampling) {
        # turn the free state trace into values on exit, even if the user
        # interrupts sampling, so the draws so far are kept
        on.exit(self$trace_values(trace_batch_size), add = TRUE)

        # the bar updates between bursts, which end on whole draws
        # (except with one_by_one), so round its updates to whole draws too
        if (one_by_one) {
          iterations_per_update <- pb_update
        } else {
          iterations_per_update <- thin * max(1, round(pb_update / thin))
        }

        if (verbose) {
          pb_sampling <- create_progress_bar(
            phase = "sampling",
            iter = c(self$warmup, n_samples),
            pb_update = iterations_per_update,
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

        if (one_by_one) {
          # one iteration per burst, so a numerical error rejects only its own
          # proposal
          burst_lengths <- rep(1L, n_samples)
        } else {
          # a burst shorter than thin has no draw to return, which errors in
          # TensorFlow (greta-dev/greta#318)
          n_draws <- n_samples %/% thin
          draws_per_update <- iterations_per_update / thin
          draws_per_burst <- self$burst_lengths(n_draws, draws_per_update)
          whole_draw_bursts <- draws_per_burst * thin

          # the iterations after the last draw keep nothing, but still run, so
          # the chain runs all n_samples iterations
          iterations_after_last_draw <- n_samples %% thin
          final_burst <- if (iterations_after_last_draw > 0) {
            iterations_after_last_draw
          } else {
            NULL
          }

          burst_lengths <- c(whole_draw_bursts, final_burst)
        }
        completed_iterations <- cumsum(burst_lengths)

        # TensorFlow thins bursts of whole draws; the rest run unthinned
        is_whole_draws <- burst_lengths %% thin == 0
        burst_thin <- ifelse(is_whole_draws, thin, 1L)
        ends_on_draw <- completed_iterations %% thin == 0

        for (burst in seq_along(burst_lengths)) {
          self$run_burst(
            n_samples = burst_lengths[burst],
            thin = burst_thin[burst]
          )
          if (ends_on_draw[burst]) {
            self$trace()
          }

          if (verbose) {
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

    # split n_samples into bursts that end at every multiple of pb_update and,
    # during warmup, of the tuning interval
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

      tuning_now <- self$in_periods(
        tuning_periods,
        iter,
        total
      )

      if (tuning_now) {
        # dual averaging step-size adaptation, after Hoffman and Gelman (2014)
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
      # from 10% to 40% of warmup, between epsilon's two tuning periods
      tuning_periods <- list(c(0.1, 0.4))

      tuning_now <- self$in_periods(
        tuning_periods,
        iterations_completed,
        total_iterations
      )

      if (tuning_now) {
        n_accepted <- sum(!self$accept_history)

        # meant to wait for more than 5 accepted proposals, but this counts
        # the rejected ones: greta-dev/greta#841
        if (n_accepted > 5) {
          # shrink the sample variance towards 1e-3, as Stan does when it
          # adapts its metric
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
      sampler_param_vec,
      sampler_seed
    ) {
      dag <- self$model$dag
      tfe <- dag$tf_environment

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

      # TFP takes its first result num_burnin_steps + 1 iterations in, and each
      # later one num_steps_between_results + 1 after the last, so thin - 1 for
      # both keeps every thin-th iteration, and a burst of d draws runs
      # d * thin iterations
      iterations_skipped <- tf$subtract(sampler_thin, 1L)

      sampler_batch <- tfp$mcmc$sample_chain(
        num_results = tf$math$floordiv(sampler_burst_length, sampler_thin),
        current_state = free_state,
        kernel = sampler_kernel,
        trace_fn = function(current_state, kernel_results) {
          kernel_results
        },
        num_burnin_steps = iterations_skipped,
        num_steps_between_results = iterations_skipped,
        parallel_iterations = 1L,
        seed = sampler_seed
      )
      return(
        sampler_batch
      )
    },

    # bursts break so that tuning and progress reporting can run in R between
    # them; moving the loop itself into TF is greta-dev/greta#547
    run_burst = function(n_samples, thin = 1L) {
      param_vec <- unlist(self$sampler_parameter_values())

      # a stateless seed, fixed by the sampler's seed and how many bursts it has
      # run. Seeding TensorFlow's global state would tie the random numbers to
      # the trace, so a future worker that rebuilds the tf_function for
      # extra_samples() would replay the first run's random numbers
      self$n_bursts <- self$n_bursts + 1L
      burst_seed <- c(self$seed, self$n_bursts)

      # run the sampler, handling numerical errors
      batch_results <- self$sample_carefully(
        free_state = self$free_state,
        sampler_burst_length = as.integer(n_samples),
        sampler_thin = as.integer(thin),
        sampler_param_vec = param_vec,
        sampler_seed = burst_seed
      )

      free_state_draws <- as.array(batch_results$all_states)

      # a rejected one-iteration burst comes back from
      # check_for_free_state_error() as the current free state, which has no
      # draw dimension, so add one
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

      if (self$uses_metropolis) {
        # log acceptance probability
        log_accept_stats <- as.array(batch_results$trace$log_accept_ratio)
        is_accepted <- as.array(batch_results$trace$is_accepted)
        self$accept_history <- rbind(self$accept_history, is_accepted)
        accept_stats_batch <- pmin(1, exp(log_accept_stats))
        self$mean_accept_stat <- mean(accept_stats_batch, na.rm = TRUE)

        # a non-finite acceptance ratio is a numerically rejected proposal
        bad <- sum(!is.finite(log_accept_stats))
        self$numerical_rejections <- self$numerical_rejections + bad
      }
    },

    tf_evaluate_sample_batch = NULL,

    sample_carefully = function(
      free_state,
      sampler_burst_length,
      sampler_thin,
      sampler_param_vec,
      sampler_seed
    ) {
      single_iteration <- sampler_burst_length == 1L

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
          ),
          sampler_seed = tensorflow::as_tensor(sampler_seed, dtype = tf$int32)
        )
      ) # closing cleanly

      self$check_for_free_state_error(result, single_iteration)

      result
    },

    check_for_free_state_error = function(result, single_iteration) {
      # cleanly() has already thrown any error that is not numerical, so an
      # error here is a numerical one
      if (inherits(result, "error")) {
        # in a burst of one iteration - every burst, with one_by_one - the
        # error came from its only proposal, so reject that proposal: mock up
        # a result that stays at the current state and pass it back
        if (single_iteration) {
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
