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
    # every iteration's random numbers come from the sampler's seed and the
    # iteration's number in the chain, so seeded draws do not depend on how
    # pb_update, verbose or one_by_one split the chain into calls
    iterations_run = 0L,
    thin = 1,
    warmup = 1,

    # tuning information. tuning_state is what define_tf_warmup_iterations()
    # carries from one call to the next, kept here between calls
    tuning_interval = 3,
    uses_metropolis = TRUE,
    accept_target = 0.5,
    tuning_state = NULL,

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
      self$define_tf_warmup()
    },

    define_tf_warmup = function() {
      float <- tf_float()
      scalar_integer <- tf$TensorSpec(shape = list(), dtype = tf$int32)
      self$tf_warmup <- tensorflow::tf_function(
        f = self$define_tf_warmup_iterations,
        input_signature = list(
          # free state
          self$model$dag$free_state_signature()[[1]],
          # n_iterations, iterations_done and total_warmup
          scalar_integer,
          scalar_integer,
          scalar_integer,
          # sampler_param_vec
          tf$TensorSpec(
            shape = list(length(unlist(self$sampler_parameter_values()))),
            dtype = float
          ),
          # tuning, welford_mean and welford_m2
          tf$TensorSpec(shape = list(6L), dtype = float),
          tf$TensorSpec(shape = list(self$n_free), dtype = float),
          tf$TensorSpec(shape = list(self$n_free), dtype = float),
          # call_seed
          tf$TensorSpec(shape = list(2L), dtype = tf$int32)
        )
      )
    },
    tf_warmup = NULL,

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
          tf$TensorSpec(shape = list(2L), dtype = tf$int32),
          # sampler_first_iteration
          tf$TensorSpec(shape = list(), dtype = tf$int32)
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
        self$define_tf_warmup()
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

        # tuning happens inside TensorFlow, so warmup returns to R only to
        # update the progress bar, or after every iteration with one_by_one
        returns_to_r <- verbose || ideal_burst_size == 1
        burst_lengths <- if (returns_to_r) {
          self$burst_lengths(self$warmup, ideal_burst_size)
        } else {
          self$warmup
        }
        completed_iterations <- cumsum(burst_lengths)

        self$reset_tuning_state()
        for (burst in seq_along(burst_lengths)) {
          self$run_warmup_burst(
            n_iterations = burst_lengths[burst],
            iterations_done = completed_iterations[burst] - burst_lengths[burst]
          )

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

        # warmup's numerical rejections are not reported with sampling's
        self$numerical_rejections <- 0
      } # end warmup
    },

    reset_tuning_state = function() {
      self$tuning_state <- list(
        tuning = c(
          hbar = 0,
          log_epsilon_bar = 0,
          count = 0,
          n_for_shrinkage = 0,
          accept_sum = 0,
          accept_count = 0
        ),
        welford_mean = rep(0, self$n_free),
        welford_m2 = rep(0, self$n_free)
      )
    },

    # runs n_iterations of warmup, tuning as it goes, and keeps the state,
    # parameters and tuning state it ends with
    run_warmup_burst = function(n_iterations, iterations_done) {
      param_vec <- unlist(self$sampler_parameter_values())

      # warmup starts the chain, so its iterations count from zero, and the
      # seed for each comes from that count inside TensorFlow
      self$n_bursts <- self$n_bursts + 1L
      call_seed <- c(self$seed, 0L)
      on.exit(
        self$iterations_run <- self$iterations_run + as.integer(n_iterations),
        add = TRUE
      )

      float <- tf_float()
      result <- cleanly(
        self$tf_warmup(
          free_state = tensorflow::as_tensor(self$free_state, dtype = float),
          n_iterations = tensorflow::as_tensor(as.integer(n_iterations)),
          iterations_done = tensorflow::as_tensor(as.integer(iterations_done)),
          total_warmup = tensorflow::as_tensor(as.integer(self$warmup)),
          sampler_param_vec = tensorflow::as_tensor(
            param_vec,
            dtype = float,
            shape = length(param_vec)
          ),
          tuning = tensorflow::as_tensor(
            unname(self$tuning_state$tuning),
            dtype = float
          ),
          welford_mean = tensorflow::as_tensor(
            self$tuning_state$welford_mean,
            dtype = float,
            shape = self$n_free
          ),
          welford_m2 = tensorflow::as_tensor(
            self$tuning_state$welford_m2,
            dtype = float,
            shape = self$n_free
          ),
          call_seed = tensorflow::as_tensor(call_seed, dtype = tf$int32)
        )
      )

      if (inherits(result, "error")) {
        # a numerical error in a single iteration rejects its proposal, and
        # leaves the state and tuning where they were
        if (n_iterations == 1) {
          self$numerical_rejections <- self$numerical_rejections + self$n_chains
          return(invisible(self))
        }
        self$check_for_free_state_error(result, single_iteration = FALSE)
      }

      free_state <- as.array(result$free_state)
      dim(free_state) <- c(self$n_chains, self$n_free)
      self$free_state <- free_state

      tuned <- as.numeric(result$sampler_param_vec)
      indices <- self$tuning_indices()
      if (!is.null(indices)) {
        self$parameters$epsilon <- tuned[indices$epsilon + 1]
        self$parameters$diag_sd <- tuned[indices$diag_sd + 1]
      }

      self$tuning_state <- list(
        tuning = setNames(
          as.numeric(result$tuning),
          names(self$tuning_state$tuning)
        ),
        welford_mean = as.numeric(result$welford_mean),
        welford_m2 = as.numeric(result$welford_m2)
      )
      self$numerical_rejections <- self$numerical_rejections +
        as.numeric(result$numerical_rejections)

      invisible(self)
    },

    # where epsilon and diag_sd sit in sampler_parameter_values(), counted
    # from zero as TensorFlow counts, or NULL for a sampler that does not tune
    tuning_indices = function() {
      NULL
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

    # split n_samples into bursts that end at every multiple of pb_update
    burst_lengths = function(n_samples, pb_update) {
      changepoints <- c(seq(0, n_samples, by = pb_update), n_samples)
      changepoints <- sort(unique(changepoints))
      diff(changepoints)
    },

    # Two independent seeds for each iteration of a call to TensorFlow: one
    # for the kernel's own parameters, such as hmc()'s leapfrog count, and one
    # for its step
    tf_iteration_seeds = function(call_seed, iteration) {
      list(
        kernel = tf$random$experimental$stateless_fold_in(
          call_seed,
          2L * iteration
        ),
        step = tf$random$experimental$stateless_fold_in(
          call_seed,
          2L * iteration + 1L
        )
      )
    },

    # the log density and gradient at the starting state, which the kernels
    # carry from one iteration to the next
    tf_bootstrap_results = function(free_state, sampler_param_vec, call_seed) {
      seeds <- self$tf_iteration_seeds(call_seed, 0L)
      kernel <- self$define_tf_kernel(sampler_param_vec, seed = seeds$kernel)
      kernel$bootstrap_results(free_state)
    },

    # one iteration of the sampler, with a kernel built from the parameters
    # given, and its acceptance. The slice sampler has no acceptance step, so
    # every iteration of it counts as accepted
    tf_step = function(
      state,
      kernel_results,
      sampler_param_vec,
      call_seed,
      iteration
    ) {
      seeds <- self$tf_iteration_seeds(call_seed, iteration)
      kernel <- self$define_tf_kernel(sampler_param_vec, seed = seeds$kernel)
      step <- kernel$one_step(state, kernel_results, seed = seeds$step)
      new_results <- step[[2]]

      if (self$uses_metropolis) {
        log_accept_ratio <- tf$cast(new_results$log_accept_ratio, state$dtype)
        is_accepted <- new_results$is_accepted
      } else {
        n_chains <- tf$shape(state)[0]
        log_accept_ratio <- tf$zeros(n_chains, dtype = state$dtype)
        is_accepted <- tf$ones(n_chains, dtype = tf$bool)
      }

      list(
        state = step[[1]],
        kernel_results = new_results,
        log_accept_ratio = log_accept_ratio,
        is_accepted = is_accepted
      )
    },

    # Runs n_iterations of warmup in one call to TensorFlow, tuning epsilon
    # and diag_sd between iterations. The tuning state comes in and goes back
    # out, so warmup can be split into calls for the progress bar, and a call
    # carries on from where the last one stopped.
    define_tf_warmup_iterations = function(
      free_state,
      n_iterations,
      iterations_done,
      total_warmup,
      sampler_param_vec,
      tuning,
      welford_mean,
      welford_m2,
      call_seed
    ) {
      kernel_results <- self$tf_bootstrap_results(
        free_state,
        sampler_param_vec,
        call_seed
      )
      tunes <- is.finite(self$tuning_interval)

      body <- function(
        iteration,
        state,
        kernel_results,
        param_vec,
        tuning,
        welford_mean,
        welford_m2,
        numerical_rejections
      ) {
        step <- self$tf_step(
          state,
          kernel_results,
          param_vec,
          call_seed,
          iterations_done + iteration
        )

        is_numerical_rejection <- tf$logical_not(
          tf$math$is_finite(step$log_accept_ratio)
        )
        numerical_rejections <- numerical_rejections +
          tf$reduce_sum(tf$cast(is_numerical_rejection, tf$int32))

        if (tunes) {
          tuned <- self$tf_tune(
            completed = iterations_done + iteration + 1L,
            total = total_warmup,
            step = step,
            param_vec = param_vec,
            tuning = tuning,
            welford_mean = welford_mean,
            welford_m2 = welford_m2
          )
          param_vec <- tuned$param_vec
          tuning <- tuned$tuning
          welford_mean <- tuned$welford_mean
          welford_m2 <- tuned$welford_m2
        }

        list(
          iteration + 1L,
          step$state,
          step$kernel_results,
          param_vec,
          tuning,
          welford_mean,
          welford_m2,
          numerical_rejections
        )
      }
      not_done <- function(iteration, ...) {
        tf$less(iteration, n_iterations)
      }

      loop <- tf$while_loop(
        cond = not_done,
        body = body,
        loop_vars = list(
          tf$constant(0L),
          free_state,
          kernel_results,
          sampler_param_vec,
          tuning,
          welford_mean,
          welford_m2,
          tf$constant(0L)
        )
      )

      list(
        free_state = loop[[2]],
        sampler_param_vec = loop[[4]],
        tuning = loop[[5]],
        welford_mean = loop[[6]],
        welford_m2 = loop[[7]],
        numerical_rejections = loop[[8]]
      )
    },

    # greta's warmup tuning, one iteration at a time inside TensorFlow. Every
    # tuning_interval iterations, and at the end of warmup, it updates epsilon
    # by dual averaging towards accept_target over the first 10% and last 60%
    # of warmup (Hoffman and Gelman, 2014), and diag_sd from the variance of
    # every warmup draw so far over the 30% between. `tuning` holds hbar, the
    # averaged log epsilon, the variance's draw count, the count used to
    # shrink the variance, and the summed and counted acceptance since the last
    # update.
    tf_tune = function(
      completed,
      total,
      step,
      param_vec,
      tuning,
      welford_mean,
      welford_m2
    ) {
      dtype <- param_vec$dtype
      tuning_values <- tf$unstack(tuning)
      hbar <- tuning_values[[1]]
      log_epsilon_bar <- tuning_values[[2]]
      count <- tuning_values[[3]]
      n_for_shrinkage <- tuning_values[[4]]
      accept_sum <- tuning_values[[5]]
      accept_count <- tuning_values[[6]]

      # a running variance over every chain's state, updated with a batch of
      # one state per chain (Chan, Golub and LeVeque, 1983)
      state <- step$state
      batch_size <- tf$cast(tf$shape(state)[0], dtype)
      batch_mean <- tf$reduce_mean(state, axis = 0L)
      batch_m2 <- tf$reduce_sum(tf$square(state - batch_mean), axis = 0L)
      new_count <- count + batch_size
      delta <- batch_mean - welford_mean
      welford_mean <- welford_mean + delta * batch_size / new_count
      welford_m2 <- welford_m2 +
        batch_m2 +
        tf$square(delta) * count * batch_size / new_count
      count <- new_count

      # counts rejected proposals, as tune_diag_sd() did, though it is meant
      # to count accepted ones: greta-dev/greta#841
      n_rejected <- tf$reduce_sum(
        tf$cast(tf$logical_not(step$is_accepted), dtype)
      )
      n_for_shrinkage <- n_for_shrinkage + n_rejected

      accept_stat <- tf$minimum(
        tf$constant(1, dtype),
        tf$exp(step$log_accept_ratio)
      )
      is_number <- tf$logical_not(tf$math$is_nan(accept_stat))
      accept_sum <- accept_sum +
        tf$reduce_sum(tf$where(
          is_number,
          accept_stat,
          tf$zeros_like(accept_stat)
        ))
      accept_count <- accept_count + tf$reduce_sum(tf$cast(is_number, dtype))

      at_tuning_point <- tf$logical_or(
        tf$equal(
          tf$math$floormod(completed, as.integer(self$tuning_interval)),
          0L
        ),
        tf$equal(completed, total)
      )

      epsilon_index <- self$tuning_indices()$epsilon
      diag_sd_index <- self$tuning_indices()$diag_sd

      update <- function() {
        completed_value <- tf$cast(completed, dtype)
        fraction <- completed_value / tf$cast(total, dtype)
        within <- function(lower, upper) {
          tf$logical_and(
            tf$greater(fraction, lower),
            tf$less_equal(fraction, upper)
          )
        }

        mean_accept <- tf$where(
          tf$greater(accept_count, 0),
          accept_sum / tf$maximum(accept_count, tf$constant(1, dtype)),
          tf$constant(0, dtype)
        )

        # dual averaging
        kappa <- 0.75
        gamma <- 0.1
        t0 <- 10
        mu <- log(t0 * 0.05)
        w1 <- 1 / (completed_value + t0)
        new_hbar <- (1 - w1) * hbar + w1 * (self$accept_target - mean_accept)
        log_epsilon <- mu - new_hbar * tf$sqrt(completed_value) / gamma
        w2 <- tf$pow(completed_value, -kappa)
        new_log_epsilon_bar <- w2 * log_epsilon + (1 - w2) * log_epsilon_bar
        # at the end of warmup, epsilon is the averaged value
        new_epsilon <- tf$where(
          tf$equal(completed, total),
          tf$exp(new_log_epsilon_bar),
          tf$exp(log_epsilon)
        )

        tuning_epsilon <- tf$logical_or(within(0, 0.1), within(0.4, 1))
        old_epsilon <- tf$gather(param_vec, epsilon_index)
        epsilon <- tf$where(tuning_epsilon, new_epsilon, old_epsilon)
        hbar <- tf$where(tuning_epsilon, new_hbar, hbar)
        log_epsilon_bar <- tf$where(
          tuning_epsilon,
          new_log_epsilon_bar,
          log_epsilon_bar
        )

        # diag_sd, from the sample variance shrunk towards 1e-3, as Stan does
        # when it adapts its metric
        tuning_diag_sd <- tf$logical_and(
          within(0.1, 0.4),
          tf$greater(n_for_shrinkage, 5)
        )
        sample_variance <- welford_m2 / (count - 1)
        shrinkage <- 1 / (n_for_shrinkage + 5)
        shrunk_variance <- n_for_shrinkage *
          shrinkage *
          sample_variance +
          5e-3 * shrinkage
        old_diag_sd <- tf$gather(param_vec, diag_sd_index)
        diag_sd <- tf$where(
          tuning_diag_sd,
          tf$sqrt(shrunk_variance),
          old_diag_sd
        )

        new_param_vec <- tf$tensor_scatter_nd_update(
          param_vec,
          indices = tf$expand_dims(
            tf$constant(c(epsilon_index, diag_sd_index), dtype = tf$int32),
            axis = 1L
          ),
          updates = tf$concat(
            list(tf$expand_dims(epsilon, 0L), diag_sd),
            axis = 0L
          )
        )
        zero <- tf$constant(0, dtype)
        list(
          new_param_vec,
          tf$stack(list(
            hbar,
            log_epsilon_bar,
            count,
            n_for_shrinkage,
            zero,
            zero
          ))
        )
      }
      keep <- function() {
        list(
          param_vec,
          tf$stack(list(
            hbar,
            log_epsilon_bar,
            count,
            n_for_shrinkage,
            accept_sum,
            accept_count
          ))
        )
      }
      updated <- tf$cond(at_tuning_point, update, keep)

      list(
        param_vec = updated[[1]],
        tuning = updated[[2]],
        welford_mean = welford_mean,
        welford_m2 = welford_m2
      )
    },

    # Runs sampler_burst_length iterations in one call to TensorFlow, keeping
    # every thin-th state, so a burst of d draws runs d * thin iterations. The
    # loop is greta's own rather than tfp$mcmc$sample_chain(), which steps one
    # kernel built before the loop: building the kernel inside the loop, from
    # each iteration's own seed, lets hmc() draw its leapfrog count every
    # iteration.
    define_tf_draws = function(
      free_state,
      sampler_burst_length,
      sampler_thin,
      sampler_param_vec,
      sampler_seed,
      sampler_first_iteration
    ) {
      n_draws <- tf$math$floordiv(sampler_burst_length, sampler_thin)

      kernel_results <- self$tf_bootstrap_results(
        free_state,
        sampler_param_vec,
        sampler_seed
      )

      draws <- tf$TensorArray(
        dtype = free_state$dtype,
        size = n_draws,
        element_shape = free_state$shape
      )
      log_accept_ratios <- tf$TensorArray(
        dtype = free_state$dtype,
        size = sampler_burst_length
      )
      accepted <- tf$TensorArray(dtype = tf$bool, size = sampler_burst_length)

      body <- function(
        iteration,
        state,
        kernel_results,
        draws,
        log_accept_ratios,
        accepted
      ) {
        step <- self$tf_step(
          state,
          kernel_results,
          sampler_param_vec,
          sampler_seed,
          sampler_first_iteration + iteration
        )

        completed <- iteration + 1L
        is_draw <- tf$equal(tf$math$floormod(completed, sampler_thin), 0L)
        draws <- tf$cond(
          is_draw,
          function() {
            draws$write(
              tf$math$floordiv(completed, sampler_thin) - 1L,
              step$state
            )
          },
          function() draws
        )

        list(
          completed,
          step$state,
          step$kernel_results,
          draws,
          log_accept_ratios$write(iteration, step$log_accept_ratio),
          accepted$write(iteration, step$is_accepted)
        )
      }
      not_done <- function(iteration, ...) {
        tf$less(iteration, sampler_burst_length)
      }

      loop <- tf$while_loop(
        cond = not_done,
        body = body,
        loop_vars = list(
          tf$constant(0L),
          free_state,
          kernel_results,
          draws,
          log_accept_ratios,
          accepted
        )
      )

      list(
        all_states = loop[[4]]$stack(),
        trace = list(
          log_accept_ratio = loop[[5]]$stack(),
          is_accepted = loop[[6]]$stack()
        )
      )
    },

    # sampling breaks into bursts only so the progress bar can update between
    # them
    run_burst = function(n_samples, thin = 1L) {
      param_vec <- unlist(self$sampler_parameter_values())

      # a stateless seed: the sampler's seed and each iteration's number in the
      # chain. Seeding TensorFlow's global state would tie the random numbers
      # to the trace, so a future worker that rebuilds the tf_function for
      # extra_samples() would replay the first run's random numbers
      self$n_bursts <- self$n_bursts + 1L
      first_iteration <- self$iterations_run
      self$iterations_run <- self$iterations_run + as.integer(n_samples)

      # run the sampler, handling numerical errors
      batch_results <- self$sample_carefully(
        free_state = self$free_state,
        sampler_burst_length = as.integer(n_samples),
        sampler_thin = as.integer(thin),
        sampler_param_vec = param_vec,
        sampler_seed = c(self$seed, 0L),
        sampler_first_iteration = first_iteration
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
        # a non-finite acceptance ratio is a numerically rejected proposal
        log_accept_stats <- as.array(batch_results$trace$log_accept_ratio)
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
      sampler_seed,
      sampler_first_iteration
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
          sampler_seed = tensorflow::as_tensor(sampler_seed, dtype = tf$int32),
          sampler_first_iteration = tensorflow::as_tensor(
            as.integer(sampler_first_iteration)
          )
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
