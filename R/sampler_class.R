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
    # the number of calls to TensorFlow so far, which only the tests read
    n_bursts = 0L,
    # every iteration's random numbers, whether it tunes, and whether its
    # state is a draw all come from its number in the chain, so seeded draws
    # do not depend on how pb_update, verbose or one_by_one split the chain
    # into calls
    iterations_run = 0L,
    sampling_start = 0L,
    thin = 1,
    warmup = 1,

    # tuning information. tuning_state is what define_iterations() carries
    # from one call to the next, kept here between calls
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

      # every call to TensorFlow passes the tuning state, including those of a
      # chain with no warmup
      self$reset_tuning_state()
    },

    # The settings read in R while the loop is traced. Samplers of one model
    # that share them share a trace, and everything else, such as epsilon, the
    # seed and the chain's position, goes in with each call. A parameter that
    # is not a number, such as rwmh()'s proposal, picks the code that is
    # traced rather than a value.
    trace_key = function() {
      code_parameters <- Filter(is.character, self$parameters)
      paste(
        class(self)[1],
        self$n_chains,
        length(unlist(self$sampler_parameter_values())),
        self$tuning_interval,
        self$accept_target,
        self$uses_metropolis,
        paste(unlist(code_parameters), collapse = ","),
        paste(self$compute_options, collapse = ","),
        sep = "|"
      )
    },

    # Each call to mcmc() makes new samplers, so they look their traced
    # function up on the model rather than trace the loop again. The function
    # keeps the sampler it was built from alive, so it is built from a copy
    # with no draws, or the model would hold on to the first sampler's draws.
    sampler_function = function() {
      dag <- self$model$dag
      key <- self$trace_key()
      if (is.null(dag$sampler_functions[[key]])) {
        template <- self$clone()
        template$traced_free_state <- list()
        template$traced_values <- list()
        template$last_burst_free_states <- list()
        template$tf_iterations <- NULL
        dag$sampler_functions[[key]] <- template$new_tf_iterations()
      }
      dag$sampler_functions[[key]]
    },

    # warmup and sampling call the same traced function, so the loop is traced
    # once
    new_tf_iterations = function() {
      float <- tf_float()
      scalar_integer <- tf$TensorSpec(shape = list(), dtype = tf$int32)
      tensorflow::tf_function(
        f = self$define_iterations,
        input_signature = list(
          # free_state
          self$free_state_signature(),
          # n_iterations, first_iteration, warmup, sampling_start and thin
          scalar_integer,
          scalar_integer,
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
          # seed
          tf$TensorSpec(shape = list(2L), dtype = tf$int32)
        )
      )
    },
    tf_iterations = NULL,

    # A sampler runs the same number of chains for as long as it exists, so
    # its function is traced for exactly that many rows, where the
    # log-density function's is traced for any number. TensorFlow runs a graph
    # whose shapes it knows faster: a second mcmc() call took 0.55 to 0.62
    # times as long this way on greta's example models (the tf-warmup and
    # branch versions in greta.benchmarks run 2026-10-07-tf-warmup-i547, at
    # greta.benchmarks commit c5d7064)
    free_state_signature = function() {
      self$model$dag$free_state_signature(n_rows = as.integer(self$n_chains))[[
        1
      ]]
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
      # extra_samples() runs no warmup, and carries on the chain's count of
      # iterations from where the last run left it
      self$sampling_start <- self$iterations_run + as.integer(warmup)
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
      }
      self$tf_iterations <- self$sampler_function()

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

      if (warmup > 0) {
        self$reset_tuning_state()
        self$run_phase(
          phase = "warmup",
          n_iterations = warmup,
          n_samples = n_samples,
          pb_update = pb_update,
          one_by_one = one_by_one,
          verbose = verbose
        )
        # warmup's numerical rejections are not reported with sampling's
        self$numerical_rejections <- 0
      }

      if (n_samples > 0) {
        # turn the free state trace into values on exit, even if the user
        # interrupts sampling, so the draws so far are kept
        on.exit(self$trace_values(trace_batch_size), add = TRUE)
        self$run_phase(
          phase = "sampling",
          n_iterations = n_samples,
          n_samples = n_samples,
          pb_update = pb_update,
          one_by_one = one_by_one,
          verbose = verbose
        )
      }

      # return self, to send results back when running in parallel
      self
    },

    # Runs one phase of the chain, warmup or sampling, in bursts of one call
    # to TensorFlow each. Tuning and thinning happen inside TensorFlow, so a
    # burst returns to R only to update the progress bar, or after every
    # iteration with one_by_one, so that a numerical error rejects only its
    # own proposal. Without a progress bar, run_samplers() makes pb_update
    # the whole phase.
    run_phase = function(
      phase,
      n_iterations,
      n_samples,
      pb_update,
      one_by_one,
      verbose
    ) {
      if (verbose) {
        pb <- create_progress_bar(
          phase = phase,
          iter = c(self$warmup, n_samples),
          pb_update = pb_update,
          width = self$pb_width
        )
        iterate_progress_bar(
          pb = pb,
          it = 0,
          rejects = 0,
          chains = self$n_chains,
          file = self$pb_file
        )
      }

      iterations_per_burst <- if (one_by_one) 1L else pb_update
      burst_lengths <- self$burst_lengths(n_iterations, iterations_per_burst)
      completed_iterations <- cumsum(burst_lengths)

      for (burst in seq_along(burst_lengths)) {
        self$run_iterations(burst_lengths[burst])
        self$trace()

        if (verbose) {
          iterate_progress_bar(
            pb = pb,
            it = completed_iterations[burst],
            rejects = self$numerical_rejections,
            chains = self$n_chains,
            file = self$pb_file
          )

          self$write_percentage_log(
            total = n_iterations,
            completed = completed_iterations[burst],
            stage = phase
          )
        }
      }
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

    # Runs the chain's next n_iterations in one call to TensorFlow, and keeps
    # the state, tuning and draws they end with
    run_iterations = function(n_iterations) {
      n_iterations <- as.integer(n_iterations)
      first_iteration <- self$iterations_run
      self$n_bursts <- self$n_bursts + 1L

      # a stateless seed, which TensorFlow combines with each iteration's
      # number in the chain. Seeding TensorFlow's global state would tie the
      # random numbers to the trace, so a future worker that rebuilds the
      # tf_function for extra_samples() would replay the first run's random
      # numbers
      seed <- c(self$seed, 0L)

      float <- tf_float()
      param_vec <- unlist(self$sampler_parameter_values())
      result <- cleanly(
        self$tf_iterations(
          free_state = tensorflow::as_tensor(self$free_state, dtype = float),
          n_iterations = tensorflow::as_tensor(n_iterations),
          first_iteration = tensorflow::as_tensor(first_iteration),
          warmup = tensorflow::as_tensor(as.integer(self$warmup)),
          sampling_start = tensorflow::as_tensor(self$sampling_start),
          thin = tensorflow::as_tensor(as.integer(self$thin)),
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
          seed = tensorflow::as_tensor(seed, dtype = tf$int32)
        )
      )

      # cleanly() has already thrown any error that is not numerical
      if (inherits(result, "error")) {
        if (n_iterations > 1) {
          self$abort_numerical_error(result)
        }
        self$reject_iteration(first_iteration)
      } else {
        free_state <- as.array(result$free_state)
        dim(free_state) <- c(self$n_chains, self$n_free)
        self$free_state <- free_state
        self$last_burst_free_states <- split_chains(as.array(result$draws))
        self$keep_tuning(
          result$sampler_param_vec,
          result$tuning,
          result$welford_mean,
          result$welford_m2
        )
        self$numerical_rejections <- self$numerical_rejections +
          as.numeric(result$numerical_rejections)
      }

      # counted once they have run, so that an interrupted call leaves the
      # chain's count where its state is
      self$iterations_run <- first_iteration + n_iterations
      invisible(self)
    },

    # keeps the tuned parameters and the tuning state TensorFlow returned
    keep_tuning = function(param_vec, tuning, welford_mean, welford_m2) {
      tuned <- as.numeric(param_vec)
      indices <- self$tuning_indices()
      if (!is.null(indices)) {
        self$parameters$epsilon <- tuned[indices$epsilon + 1]
        self$parameters$diag_sd <- tuned[indices$diag_sd + 1]
      }

      self$tuning_state <- list(
        tuning = setNames(as.numeric(tuning), names(self$tuning_state$tuning)),
        welford_mean = as.numeric(welford_mean),
        welford_m2 = as.numeric(welford_m2)
      )
    },

    # where epsilon and diag_sd sit in sampler_parameter_values(), counted
    # from zero as TensorFlow counts, or NULL for a sampler that does not tune
    tuning_indices = function() {
      NULL
    },

    # A numerical error in a call of one iteration came from its only
    # proposal, so the sampler rejects that proposal: the state stays where
    # it was, and is kept as a draw if one was due
    reject_iteration = function(iteration) {
      self$numerical_rejections <- self$numerical_rejections + self$n_chains

      tunes_iteration <- iteration < self$sampling_start &&
        is.finite(self$tuning_interval)
      if (tunes_iteration) {
        self$tune_rejected_iteration(iteration)
      }

      sampling_iterations <- iteration + 1L - self$sampling_start
      is_draw <- sampling_iterations > 0 &&
        sampling_iterations %% self$thin == 0
      n_draws <- as.integer(is_draw)
      draws <- array(
        rep(self$free_state, n_draws),
        dim = c(n_draws, self$n_chains, self$n_free)
      )
      self$last_burst_free_states <- split_chains(draws)
    },

    # Warmup tunes on a rejected proposal as on any other: an acceptance of
    # zero, and the state unchanged. If the iteration that errored ends warmup,
    # this is also what sets epsilon to its averaged value. It runs tf_tune()
    # eagerly, since only an iteration that errors with one_by_one comes here.
    tune_rejected_iteration = function(iteration) {
      float <- tf_float()
      warmup_start <- self$sampling_start - self$warmup
      rejected_step <- list(
        state = tensorflow::as_tensor(self$free_state, dtype = float),
        log_accept_ratio = tensorflow::as_tensor(
          rep(-Inf, self$n_chains),
          dtype = float
        ),
        is_accepted = tensorflow::as_tensor(rep(FALSE, self$n_chains))
      )
      param_vec <- unlist(self$sampler_parameter_values())
      tuned <- self$tf_tune(
        completed = tensorflow::as_tensor(
          as.integer(iteration - warmup_start + 1L)
        ),
        total = tensorflow::as_tensor(as.integer(self$warmup)),
        step = rejected_step,
        param_vec = tensorflow::as_tensor(
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
        )
      )
      self$keep_tuning(
        tuned$param_vec,
        tuned$tuning,
        tuned$welford_mean,
        tuned$welford_m2
      )
    },

    # In a call of more than one iteration, the sampler cannot tell which
    # proposal failed, and the chain would not be valid if it carried on, so
    # it stops and says how to run one iteration per call
    abort_numerical_error = function(error) {
      greta_stash$tf_num_error <- error
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

    # split n_iterations into bursts that end at every multiple of pb_update
    burst_lengths = function(n_iterations, pb_update) {
      changepoints <- c(seq(0, n_iterations, by = pb_update), n_iterations)
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

    # Runs n_iterations of the chain in one call to TensorFlow, from
    # first_iteration, the chain's count of iterations so far. The warmup
    # iterations before sampling_start tune epsilon and diag_sd, and the
    # tuning state comes in and goes back out, so a call carries on from where
    # the last one stopped. From sampling_start on, every thin-th state is
    # kept as a draw. The loop is greta's own rather than
    # tfp$mcmc$sample_chain(), which steps one kernel built before the loop:
    # building the kernel inside the loop, from each iteration's own seed,
    # lets hmc() draw its leapfrog count every iteration.
    define_iterations = function(
      free_state,
      n_iterations,
      first_iteration,
      warmup,
      sampling_start,
      thin,
      sampler_param_vec,
      tuning,
      welford_mean,
      welford_m2,
      seed
    ) {
      kernel_results <- self$tf_bootstrap_results(
        free_state,
        sampler_param_vec,
        seed
      )
      tunes <- is.finite(self$tuning_interval)
      warmup_start <- sampling_start - warmup

      draws_after <- function(iterations) {
        sampling_iterations <- tf$maximum(iterations - sampling_start, 0L)
        tf$math$floordiv(sampling_iterations, thin)
      }
      draws_before_call <- draws_after(first_iteration)
      # the shape comes from R rather than the free state, whose number of rows
      # can be left open, so that a call with no draws, such as one in warmup,
      # stacks to an empty array rather than erroring
      draws <- tf$TensorArray(
        dtype = free_state$dtype,
        size = draws_after(first_iteration + n_iterations) - draws_before_call,
        element_shape = list(self$n_chains, self$n_free)
      )

      body <- function(
        iteration,
        state,
        kernel_results,
        param_vec,
        tuning,
        welford_mean,
        welford_m2,
        draws,
        numerical_rejections
      ) {
        chain_iteration <- first_iteration + iteration
        step <- self$tf_step(
          state,
          kernel_results,
          param_vec,
          seed,
          chain_iteration
        )

        is_numerical_rejection <- tf$logical_not(
          tf$math$is_finite(step$log_accept_ratio)
        )
        numerical_rejections <- numerical_rejections +
          tf$reduce_sum(tf$cast(is_numerical_rejection, tf$int32))

        if (tunes) {
          tune <- function() {
            tuned <- self$tf_tune(
              completed = chain_iteration - warmup_start + 1L,
              total = warmup,
              step = step,
              param_vec = param_vec,
              tuning = tuning,
              welford_mean = welford_mean,
              welford_m2 = welford_m2
            )
            unname(tuned)
          }
          keep <- function() {
            list(param_vec, tuning, welford_mean, welford_m2)
          }
          is_warmup <- tf$less(chain_iteration, sampling_start)
          tuned <- tf$cond(is_warmup, tune, keep)
          param_vec <- tuned[[1]]
          tuning <- tuned[[2]]
          welford_mean <- tuned[[3]]
          welford_m2 <- tuned[[4]]
        }

        draws_so_far <- draws_after(chain_iteration + 1L)
        is_draw <- tf$greater(draws_so_far, draws_after(chain_iteration))
        draws <- tf$cond(
          is_draw,
          function() {
            draws$write(draws_so_far - draws_before_call - 1L, step$state)
          },
          \() draws
        )

        list(
          iteration + 1L,
          step$state,
          step$kernel_results,
          param_vec,
          tuning,
          welford_mean,
          welford_m2,
          draws,
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
          draws,
          tf$constant(0L)
        )
      )

      list(
        free_state = loop[[2]],
        sampler_param_vec = loop[[4]],
        tuning = loop[[5]],
        welford_mean = loop[[6]],
        welford_m2 = loop[[7]],
        draws = loop[[8]]$stack(),
        numerical_rejections = loop[[9]]
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
      # one state per chain
      running_variance <- tfp$experimental$stats$RunningVariance(
        num_samples = count,
        mean = welford_mean,
        sum_squared_residuals = welford_m2,
        event_ndims = 0L
      )$update(step$state, axis = 0L)
      count <- running_variance$num_samples
      welford_mean <- running_variance$mean
      welford_m2 <- running_variance$sum_squared_residuals

      # counts rejected proposals, though it is meant to count accepted ones:
      # greta-dev/greta#841
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
        sample_variance <- running_variance$variance(ddof = 1L)
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

    sampler_parameter_values = function() {
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
