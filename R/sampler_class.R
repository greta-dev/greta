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
    # an iteration's random numbers, tuning and thinning follow from its
    # number in the chain, not from how the chain is split into calls
    iterations_run = 0L,
    sampling_start = 0L,
    thin = 1,
    warmup = 1,

    # tuning information. tuning_state is what define_iterations() carries
    # from one call to the next, kept here between phases
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
    },

    # What the traced function reads in R, so samplers with the same settings
    # share one trace. Numbers such as epsilon and the seed go in with each
    # call instead.
    trace_settings = function() {
      list(
        sampler = class(self)[1],
        n_chains = self$n_chains,
        n_parameters = length(unlist(self$sampler_parameter_values())),
        tuning_interval = self$tuning_interval,
        accept_target = self$accept_target,
        uses_metropolis = self$uses_metropolis,
        # such as rwmh()'s proposal, which picks the code that is traced
        text_parameters = Filter(is.character, self$parameters),
        compute_options = self$compute_options
      )
    },

    # each call to mcmc() makes new samplers, so the traced function is kept on
    # the model
    sampler_function = function() {
      self$model$dag$traced_function(
        cache = "sampler_functions",
        settings = self$trace_settings(),
        build = self$new_tf_iterations_without_draws
      )
    },

    # the traced function keeps the sampler it was built from alive, so it is
    # built from a copy with no draws
    new_tf_iterations_without_draws = function() {
      template <- self$clone()
      template$traced_free_state <- list()
      template$traced_values <- list()
      template$last_burst_free_states <- list()
      template$tf_iterations <- NULL
      template$new_tf_iterations()
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
          # n_iterations
          scalar_integer,
          # first_iteration
          scalar_integer,
          # warmup
          scalar_integer,
          # sampling_start
          scalar_integer,
          # thin
          scalar_integer,
          # param_vec
          tf$TensorSpec(
            shape = list(length(unlist(self$sampler_parameter_values()))),
            dtype = float
          ),
          # tuning
          tf$TensorSpec(
            shape = list(length(self$tuning_state$tuning)),
            dtype = float
          ),
          # welford_mean
          tf$TensorSpec(shape = list(self$n_free), dtype = float),
          # welford_m2
          tf$TensorSpec(shape = list(self$n_free), dtype = float),
          # seed
          tf$TensorSpec(shape = list(2L), dtype = tf$int32)
        )
      )
    },
    tf_iterations = NULL,

    # A sampler always runs the same number of chains, so its function is
    # traced for exactly that many rows, which runs faster than any number:
    # with any number, a second mcmc() call took 1.6 to 1.8 times as long
    # (greta.benchmarks run 2026-10-07-tf-warmup-i547, at commit c5d7064)
    free_state_signature = function() {
      n_rows <- as.integer(self$n_chains)
      self$model$dag$free_state_signature(n_rows = n_rows)[[1]]
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

      # warmup tunes from a fresh state, and every call to TensorFlow passes
      # the tuning state, sampling's included
      self$reset_tuning_state()
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
        # keep draws so far by turning free state into values on exit
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
    # own proposal. Without either, the phase is one call.
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

      iterations_per_burst <- if (one_by_one) {
        1L
      } else if (verbose) {
        pb_update
      } else {
        n_iterations
      }
      burst_lengths <- self$burst_lengths(n_iterations, iterations_per_burst)
      completed_iterations <- cumsum(burst_lengths)

      # state and tuning stay as tensors between calls and come back to R once,
      # when the phase ends; inputs fixed for the phase become tensors once
      self$chain_tensors <- self$state_tensors()
      on.exit(self$keep_in_r(), add = TRUE)
      phase_inputs <- self$phase_inputs()

      for (burst in seq_along(burst_lengths)) {
        self$run_iterations(burst_lengths[burst], phase_inputs)
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

    # the traced function's inputs that stay the same through a phase, as
    # tensors
    phase_inputs = function() {
      # a stateless seed, which TensorFlow combines with each iteration's
      # number in the chain. Seeding TensorFlow's global state would tie the
      # random numbers to the trace, so a future worker that rebuilds the
      # tf_function for extra_samples() would replay the first run's random
      # numbers
      seed <- c(self$seed, 0L)
      list(
        warmup = tensorflow::as_tensor(as.integer(self$warmup)),
        sampling_start = tensorflow::as_tensor(self$sampling_start),
        thin = tensorflow::as_tensor(as.integer(self$thin)),
        seed = tensorflow::as_tensor(seed, dtype = tf$int32)
      )
    },

    # Runs the chain's next n_iterations in one call to TensorFlow, and keeps
    # the state, tuning and draws they end with
    run_iterations = function(n_iterations, phase_inputs) {
      n_iterations <- as.integer(n_iterations)
      first_iteration <- self$iterations_run
      self$n_bursts <- self$n_bursts + 1L

      tensors <- self$chain_tensors
      result <- cleanly(
        self$tf_iterations(
          free_state = tensors$free_state,
          n_iterations = tensorflow::as_tensor(n_iterations),
          first_iteration = tensorflow::as_tensor(first_iteration),
          warmup = phase_inputs$warmup,
          sampling_start = phase_inputs$sampling_start,
          thin = phase_inputs$thin,
          param_vec = tensors$param_vec,
          tuning = tensors$tuning,
          welford_mean = tensors$welford_mean,
          welford_m2 = tensors$welford_m2,
          seed = phase_inputs$seed
        )
      )

      # cleanly() has already thrown any error that is not numerical
      if (inherits(result, "error")) {
        if (n_iterations > 1) {
          self$abort_numerical_error(result)
        }
        self$reject_iteration(first_iteration)
      } else {
        self$chain_tensors <- result[names(tensors)]
        self$last_burst_free_states <- split_chains(as.array(result$draws))
        self$numerical_rejections <- self$numerical_rejections +
          as.numeric(result$numerical_rejections)
      }

      # counted once they have run, so that an interrupted call leaves the
      # chain's count where its state is
      self$iterations_run <- first_iteration + n_iterations
      invisible(self)
    },

    # the chain's state and tuning as tensors while a phase runs, and NULL
    # between phases, when the R fields hold them
    chain_tensors = NULL,

    # The sampler's state, parameters and tuning state, as the tensors the
    # traced function and tf_tune() take. The shapes are given so that a model
    # with one free parameter still passes vectors, as the signature expects.
    state_tensors = function() {
      float <- tf_float()
      param_vec <- unlist(self$sampler_parameter_values())
      list(
        free_state = tensorflow::as_tensor(self$free_state, dtype = float),
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
    },

    # keeps the state, tuned parameters and tuning state that a phase held as
    # tensors in the R fields, where they stay between phases
    keep_in_r = function() {
      tensors <- self$chain_tensors
      free_state <- as.array(tensors$free_state)
      dim(free_state) <- c(self$n_chains, self$n_free)
      self$free_state <- free_state

      if (self$tunes()) {
        param_vec <- as.numeric(tensors$param_vec)
        indices <- self$tuning_indices()
        self$parameters$epsilon <- param_vec[indices$epsilon + 1]
        self$parameters$diag_sd <- param_vec[indices$diag_sd + 1]
      }

      self$tuning_state <- list(
        tuning = setNames(
          as.numeric(tensors$tuning),
          names(self$tuning_state$tuning)
        ),
        welford_mean = as.numeric(tensors$welford_mean),
        welford_m2 = as.numeric(tensors$welford_m2)
      )
      self$chain_tensors <- NULL
    },

    # where epsilon and diag_sd sit in sampler_parameter_values(), counted
    # from zero as TensorFlow counts, or NULL for a sampler that does not tune
    tuning_indices = function() {
      NULL
    },

    tunes = function() {
      !is.null(self$tuning_indices())
    },

    # A numerical error in a call of one iteration came from its only
    # proposal, so the sampler rejects that proposal: the state stays where
    # it was, and is kept as a draw if one was due, by the rule
    # define_iterations() uses
    reject_iteration = function(iteration) {
      self$numerical_rejections <- self$numerical_rejections + self$n_chains

      tunes_iteration <- iteration < self$sampling_start && self$tunes()
      if (tunes_iteration) {
        self$tune_rejected_iteration(iteration)
      }

      sampling_iterations <- iteration + 1L - self$sampling_start
      is_draw <- sampling_iterations > 0 &&
        sampling_iterations %% self$thin == 0
      kept_state <- if (is_draw) {
        as.array(self$chain_tensors$free_state)
      } else {
        numeric()
      }
      draws <- array(
        kept_state,
        dim = c(as.integer(is_draw), self$n_chains, self$n_free)
      )
      self$last_burst_free_states <- split_chains(draws)
    },

    # Warmup tunes on a rejected proposal as on any other: an acceptance of
    # zero, and the state unchanged. If the iteration that errored ends warmup,
    # this is also what sets epsilon to its averaged value. It runs tf_tune()
    # eagerly, since only an iteration that errors with one_by_one comes here.
    tune_rejected_iteration = function(iteration) {
      warmup_start <- self$sampling_start - self$warmup
      tensors <- self$chain_tensors
      rejected_step <- list(
        state = tensors$free_state,
        log_accept_ratio = tensorflow::as_tensor(
          rep(-Inf, self$n_chains),
          dtype = tf_float()
        ),
        is_accepted = tensorflow::as_tensor(rep(FALSE, self$n_chains))
      )
      tuned <- self$tf_tune(
        completed = tensorflow::as_tensor(
          as.integer(iteration - warmup_start + 1L)
        ),
        total = tensorflow::as_tensor(as.integer(self$warmup)),
        step = rejected_step,
        param_vec = tensors$param_vec,
        tuning = tensors$tuning,
        welford_mean = tensors$welford_mean,
        welford_m2 = tensors$welford_m2
      )
      self$chain_tensors[names(tuned)] <- tuned
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
      param_vec,
      tuning,
      welford_mean,
      welford_m2,
      seed
    ) {
      kernel_results <- self$tf_bootstrap_results(free_state, param_vec, seed)
      tunes <- self$tunes()
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
      # the chain's count of completed iterations at which the next draw is
      # kept, carried through the loop so each iteration only compares it
      next_draw_at <- sampling_start + (draws_before_call + 1L) * thin

      body <- function(
        iteration,
        state,
        kernel_results,
        param_vec,
        tuning,
        welford_mean,
        welford_m2,
        draws,
        n_drawn,
        next_draw_at,
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

        if (self$uses_metropolis) {
          is_numerical_rejection <- tf$logical_not(
            tf$math$is_finite(step$log_accept_ratio)
          )
          numerical_rejections <- numerical_rejections +
            tf$reduce_sum(tf$cast(is_numerical_rejection, tf$int32))
        }

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

        is_draw <- tf$equal(chain_iteration + 1L, next_draw_at)
        kept <- tf$cond(
          is_draw,
          function() {
            list(
              draws$write(n_drawn, step$state),
              n_drawn + 1L,
              next_draw_at + thin
            )
          },
          \() list(draws, n_drawn, next_draw_at)
        )

        list(
          iteration + 1L,
          step$state,
          step$kernel_results,
          param_vec,
          tuning,
          welford_mean,
          welford_m2,
          kept[[1]],
          kept[[2]],
          kept[[3]],
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
          param_vec,
          tuning,
          welford_mean,
          welford_m2,
          draws,
          tf$constant(0L),
          next_draw_at,
          tf$constant(0L)
        )
      )

      list(
        free_state = loop[[2]],
        param_vec = loop[[4]],
        tuning = loop[[5]],
        welford_mean = loop[[6]],
        welford_m2 = loop[[7]],
        draws = loop[[8]]$stack(),
        numerical_rejections = loop[[11]]
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

      indices <- self$tuning_indices()
      epsilon_index <- indices$epsilon
      diag_sd_index <- indices$diag_sd

      update <- function() {
        completed_value <- tf$cast(completed, dtype)
        fraction <- completed_value / tf$cast(total, dtype)
        within <- function(lower, upper) {
          tf$logical_and(
            tf$greater(fraction, lower),
            tf$less_equal(fraction, upper)
          )
        }

        # accept_sum is zero whenever accept_count is, so this is zero then
        mean_accept <- accept_sum /
          tf$maximum(accept_count, tf$constant(1, dtype))

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
        list(new_param_vec, hbar, log_epsilon_bar)
      }
      keep <- function() {
        list(param_vec, hbar, log_epsilon_bar)
      }
      updated <- tf$cond(at_tuning_point, update, keep)

      # the acceptance counts start again after each update
      zero <- tf$constant(0, dtype)
      list(
        param_vec = updated[[1]],
        tuning = tf$stack(list(
          updated[[2]],
          updated[[3]],
          count,
          n_for_shrinkage,
          tf$where(at_tuning_point, zero, accept_sum),
          tf$where(at_tuning_point, zero, accept_count)
        )),
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
