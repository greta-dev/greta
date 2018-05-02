# R6 inference class objects

# base node class
inference <- R6Class(
  'inference',
  public = list(

    model = NULL,

    # RNG seed
    seed = 1,

    # size and current value of the free state
    n_free = 1L,
    free_state = 0,

    # and of the traced values
    n_traced = 1L,

    parameters = list(),
    tuning_periods = list(),

    # free state values for the last burst
    last_burst_free_states = matrix(0),
    # all recorded free state values
    traced_free_state = matrix(0),
    # all recorded greta array values
    traced_values = matrix(0),

    # where this is in the run
    initialize = function (initial_values,
                           model,
                           parameters = list(),
                           seed = get_seed()) {

      self$parameters <- parameters
      self$model <- model
      self$n_free <- length(model$dag$example_parameters())
      self$free_state <- self$initial_values(initial_values)
      self$n_traced <- length(model$dag$trace_values(self$free_state))
      self$seed <- seed

    },

    # set RNG seed for a tensorflow graph. Must be done before definition of a
    # random tensor
    set_tf_seed = function () {
      dag <- self$model$dag
      dag$tf_environment$rng_seed <- self$seed
    },

    initial_values = function (user_specified) {

      # check user-provided initial values
      if (!is.null(user_specified)) {

        # check their length
        if (length(user_specified) != self$n_free) {
          stop ("each set of initial values must be a vector of length ",
                self$n_free,
                call. = FALSE)
        }

        # check they can be be used
        valid <- self$valid_parameters(user_specified)
        if (!valid) {
          stop ("The log density and gradients could not be evaluated ",
                "at these initial values.",
                call. = FALSE)
        }

        initial_values <- user_specified

      } else {

        # otherwise, try several times to generate some
        valid <- FALSE
        attempts <- 1
        while (!valid & attempts < 20) {

          initial_values <- rnorm(self$n_free, 0, 0.1)

          # test validity of values
          valid <- self$valid_parameters(initial_values)
          attempts <- attempts + 1

        }

        if (!valid) {
          stop ("Could not find reasonable starting values after ", attempts,
                " attempts. Please specify initial values manually via the ",
                "initial_values argument to mcmc",
                call. = FALSE)
        }
      }

      # set them as the state
      self$free_state <- initial_values

    },

    # check whether the model can be evaluated at these parameters
    valid_parameters = function (parameters) {

      dag <- self$model$dag
      tfe <- dag$tf_environment

      if (!exists("joint_density_adj", envir = tfe)) {
        dag$on_graph(dag$define_joint_density())
      }

      if (!exists("gradients_adj", envir = tfe)) {
        dag$on_graph(dag$define_gradients())
      }

      dag$send_parameters(parameters)
      ld <- self$model$dag$log_density()
      grad <- self$model$dag$gradients()
      all(is.finite(c(ld, grad)))

    },

    # run a burst of sampling, and put the resulting free state values in
    # last_burst_free_states
    run_burst = function () {

      stop ("no method to run a burst in the base inference class")
      self$last_burst_free_states <- free_states

    },

    # store the free state, and/or corresponding values of the target greta
    # arrays for the latest batch of raw draws
    trace = function (values = TRUE, free_state = TRUE) {

      if (free_state) {
        # append the free state trace
        self$traced_free_state <- rbind(self$traced_free_state,
                                        self$last_burst_free_states)
      }

      if (values) {
        # calculate the observed values
        last_burst_free_states <- self$last_burst_free_states
        last_burst_values <- self$trace_burst_values(last_burst_free_states)
        self$traced_values <- rbind(self$traced_values,
                                    last_burst_values)
      }

    },

    # given a matrix of free state values, get a matrix of values of the target
    # greta arrays
    trace_burst_values = function (burst_free_states) {

      values_trace <- apply(burst_free_states, 1, self$model$dag$trace_values)

      # make sure it's a matrix in the correct orientation
      if (is.matrix(values_trace)) {
        values_trace <- t(values_trace)
      } else {
        values_trace <- matrix(values_trace)
      }

      values_trace

    },

    # is the sampler in one of the tuning periods for a given parameter
    in_periods = function (periods, i, n_samples) {

      within <- function (period, fraction)
        fraction > period[1] & fraction <= period[2]

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

    acceptance_rate = 0,
    numerical_rejections = 0,
    chain_number = 1,
    n_chains = 1,

    # how often to tune during warmup
    tuning_interval = 10,

    run_chain = function (n_samples, thin, warmup, verbose, pb_update) {

      self$print_chain_number()

      self$traced_free_state <- matrix(NA, 0, self$n_free)
      self$traced_values <- matrix(NA, 0, self$n_traced)

      # if warmup is required, do that now
      if (warmup > 0) {

        if (verbose) {
          pb_warmup <- create_progress_bar("warmup",
                                           c(warmup, n_samples),
                                           pb_update)
          iterate_progress_bar(pb_warmup, 0, 0)
        } else {
          pb_warmup <- NULL
        }

        # split up warmup iterations into bursts of sampling
        burst_lengths <- self$burst_lengths(warmup,
                                            pb_update,
                                            warmup = TRUE)
        completed_iterations <- cumsum(burst_lengths)

        for (burst in seq_along(burst_lengths)) {

          self$run_burst(burst_lengths[burst], thin = thin)
          self$tune(completed_iterations[burst], warmup)
          self$trace(values = FALSE)

          if (verbose) {
            iterate_progress_bar(pb_warmup,
                                 it = completed_iterations[burst],
                                 rejects = self$numerical_rejections)
          }

        }

      }

      # scrub the free state trace
      self$traced_free_state <- matrix(NA, 0, self$n_free)

      # main sampling
      if (verbose) {
        pb_sampling <- create_progress_bar('sampling',
                                           c(warmup, n_samples),
                                           pb_update)
        iterate_progress_bar(pb_sampling, 0, 0)
      } else {
        pb_sampling <- NULL
      }

      # split up warmup iterations into bursts of sampling
      burst_lengths <- self$burst_lengths(n_samples, pb_update)
      completed_iterations <- cumsum(burst_lengths)

      for (burst in seq_along(burst_lengths)) {

        self$run_burst(burst_lengths[burst], thin = thin)
        self$trace()

        if (verbose) {
          iterate_progress_bar(pb_sampling,
                               it = completed_iterations[burst],
                               rejects = self$numerical_rejections)
        }

      }

      sampler

    },

    # print the chain number (if relevant)
    print_chain_number = function () {

      if (self$n_chains > 1) {
        msg <- sprintf("\nchain %i/%i\n",
                       self$chain_number,
                       self$n_chains)
        cat(msg)
      }

    },

    # split the number of samples up into bursts of running the sampler,
    # considering the progress bar update frequency and the parameter tuning
    # schedule during warmup
    burst_lengths = function (n_samples, pb_update, warmup = FALSE) {

      # when to stop for progress bar updates
      changepoints <- c(seq(0, n_samples, by = pb_update), n_samples)

      if (warmup) {

        # when to break to update tuning
        tuning_points <- seq(0, n_samples, by = self$tuning_interval)
        changepoints <- c(changepoints, tuning_points)

      }

      changepoints <- sort(unique(changepoints))
      diff(changepoints)

    },

    # by default, samplers have an empty tuning method
    tune = function (iterations_completed, total_iterations) {
      invisible(NULL)
    }

  )
)

hmc_sampler <- R6Class(
  "hmc_sampler",
  inherit = sampler,
  public = list(

    parameters = list(Lmin = 10,
                      Lmax = 20,
                      epsilon = 0.005,
                      diag_sd = 1),

    # tuning information for these variables
    mean_accept_stat = 0.5,
    sum_epsilon_trace = NULL,
    hbar = 0,
    log_epsilon_bar = 0,

    tf_kernel = NULL,

    initialize = function (initial_values,
                           model,
                           parameters = list(),
                           seed) {

      # initialize the inference method
      super$initialize(initial_values = initial_values,
                       model = model,
                       parameters = parameters,
                       seed = seed)

      # duplicate diag_sd if needed
      n_diag <- length(self$parameters$diag_sd)
      n_parameters <- length(model$dag$example_parameters())
      if (n_diag != n_parameters && n_parameters > 1) {
        diag_sd <- rep(self$parameters$diag_sd[1], n_parameters)
        self$parameters$diag_sd <- diag_sd
      }

      # define the draws tensor on the tf graph
      tfe <- model$dag$tf_environment
      if (!exists("hmc_batch", envir = tfe)) {
        self$define_tf_hmc_draws(define_variables = TRUE)
      }

    },

    define_tf_hmc_draws = function (define_variables = FALSE) {

      dag <- self$model$dag
      tfe <- dag$tf_environment

      super$set_tf_seed()

      if (define_variables) {
        # define tensors for the parameters
        dag$tf_run(hmc_epsilon <- tf$placeholder(dtype = tf_float(),
                                                 shape = list()))
        dag$tf_run(hmc_L <- tf$placeholder(dtype = tf$int32,
                                           shape = list()))
        dag$tf_run(hmc_diag_sd <- tf$placeholder(dtype = tf_float(),
                                           shape = length(free_state)))
        dag$tf_run(hmc_step_sizes <- hmc_epsilon * hmc_diag_sd)

        # and the sampler info
        dag$tf_run(hmc_burst_length <- tf$placeholder(dtype = tf$int32,
                                                      shape = list()))
        dag$tf_run(hmc_thin <- tf$placeholder(dtype = tf$int32,
                                              shape = list()))
        tfe$log_prob_fun <- dag$generate_log_prob_function(adjust = TRUE)

      }

      dag$tf_run(
        tf_kernel <- tfp$mcmc$HamiltonianMonteCarlo(
          target_log_prob_fn = log_prob_fun,
          step_size = hmc_step_sizes,
          num_leapfrog_steps = hmc_L,
          seed = rng_seed)
      )

      # define the whole draws tensor
      dag$tf_run(
        hmc_batch <- tfp$mcmc$sample_chain(
          num_results = hmc_burst_length %/% hmc_thin,
          current_state = tf$reshape(free_state, list(length(free_state))),
          kernel = tf_kernel,
          num_burnin_steps = 0L,
          num_steps_between_results = hmc_thin)
      )

    },

    run_burst = function (n_samples, thin) {
      self$tf_run_burst(n_samples, thin)
    },

    # run a burst with tensorflow HMC
    tf_run_burst = function (n_samples, thin) {

      dag <- self$model$dag
      tfe <- dag$tf_environment

      # get parameters
      Lmin <- self$parameters$Lmin
      Lmax <- self$parameters$Lmax
      L <- sample(seq(Lmin, Lmax), 1)
      epsilon <- self$parameters$epsilon
      diag_sd <- self$parameters$diag_sd

      # set up dict
      tfe$hmc_values <- list(free_state = as.matrix(self$free_state),
                             hmc_L = L,
                             hmc_epsilon = epsilon,
                             hmc_diag_sd = diag_sd,
                             hmc_burst_length = as.integer(n_samples),
                             hmc_thin = as.integer(thin))
      dag$tf_run(hmc_dict <- do.call(dict, hmc_values))

      # run sampler
      hmc_batch_results <- dag$tf_run(sess$run(hmc_batch,
                                               feed_dict = hmc_dict))

      # get trace of free state
      free_state_draws <- hmc_batch_results[[1]]
      self$last_burst_free_states <- free_state_draws
      n_draws <- nrow(free_state_draws)
      self$free_state <- free_state_draws[n_draws, ]

      # log acceptance probability
      log_accept_stats <- hmc_batch_results[[2]]$log_accept_ratio
      accept_stats_batch <- pmin(1, exp(log_accept_stats))
      self$mean_accept_stat <- mean(accept_stats_batch)

    },

    # run the sampler for n_samples (possibly thinning)
    r_run_burst = function (n_samples, thin) {

      self$last_burst_free_states <- matrix(NA, n_samples, self$n_free)

      # unpack options
      epsilon <- self$parameters$epsilon
      diag_sd <- self$parameters$diag_sd
      L <- seq(self$parameters$Lmin,
               self$parameters$Lmax)

      dag <- self$model$dag
      n_free <- self$n_free
      x <- self$free_state

      # track acceptance and numerical rejections
      accept_trace <- rep(0, n_samples)

      # set initial location, log joint density and gradients
      dag$send_parameters(x)
      grad <- dag$gradients()
      logprob <- dag$log_density()

      # loop through iterations
      for (i in 1:n_samples) {

        # copy old state
        x_old <- x
        logprob_old <- logprob
        grad_old <- grad
        p <- p_old <- rnorm(n_free)

        # start leapfrog steps
        reject <- FALSE
        n_steps <- ifelse(length(L) == 1, L,
                          base::sample(L, 1))

        for (l in seq_len(n_steps)) {

          # step
          p <- p + 0.5 * epsilon * grad * diag_sd
          x <- x + epsilon * p * diag_sd

          # send parameters
          dag$send_parameters(x)
          grad <- dag$gradients()

          # check gradients are finite
          if (any(!is.finite(grad))) {
            reject <- TRUE
            break()
          }

          p <- p + 0.5 * epsilon * grad * diag_sd

        }

        # if the step was bad, reject it out of hand
        if (reject) {

          self$numerical_rejections <- self$numerical_rejections + 1
          x <- x_old
          logprob <- logprob_old
          grad <- grad_old

        } else {

          # otherwise do the Metropolis accept/reject step

          # inner products
          p_prod <- 0.5 * sum(p ^ 2)
          p_prod_old <- 0.5 * sum(p_old ^ 2)

          # acceptance ratio
          logprob <- dag$log_density()
          log_accept_ratio <- logprob - p_prod - logprob_old + p_prod_old
          log_u <- log(runif(1))

          if (log_u < log_accept_ratio) {

            accept_trace[i] <- 1

          } else {

            # on rejection, reset all the parameters and push old parameters to
            # the graph for the trace
            x <- x_old
            logprob <- logprob_old
            grad <- grad_old

          }

        }

        # store the values of the free state for this burst
        if (i %% thin == 0) {
          self$last_burst_free_states[i %/% thin, ] <- x
        }


      }

      # assign the free state and tuning information at the end of this burst
      self$free_state <- x
      self$accept_trace <- c(self$accept_trace, accept_trace)

    },

    # overall tuning method
    tune = function(iterations_completed, total_iterations) {
      self$tune_epsilon(iterations_completed, total_iterations)
      self$tune_diag_sd(iterations_completed, total_iterations)
    },

    tune_epsilon = function (iter, total) {

      # tuning periods for the tunable parameters (first 10%, last 60%)
      tuning_periods <- list(c(0, 0.1), c(0.4, 1))

      # whether we're tuning now
      tuning_now <- self$in_periods(tuning_periods,
                                    iter,
                                    total)

      if (tuning_now) {

        # epsilon & tuning parameters
        target <- 0.651
        kappa <- 0.75
        gamma <- 0.1
        t0 <- 10
        mu <- log(t0 * 0.05)

        hbar <- self$hbar
        log_epsilon_bar <- self$log_epsilon_bar
        mean_accept_stat <- self$mean_accept_stat

        w1 <- 1 / (iter + t0)
        hbar <- (1 - w1) * hbar + w1 * (target - mean_accept_stat)
        log_epsilon <- mu - hbar * sqrt(iter) / gamma
        w2 <- iter ^ -kappa
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

    tune_diag_sd = function (iterations_completed, total_iterations) {

      # when, during warmup, to tune this parameter (after epsilon, but stopping
      # before halfway through)
      tuning_periods <- list(c(0.1, 0.4))

      tuning_now <- self$in_periods(tuning_periods,
                                    iterations_completed,
                                    total_iterations)

      if (tuning_now) {

        samples <- self$traced_free_state
        dups <- duplicated(samples)
        if (length(dups) > 0) {
          samples <- samples[!dups, , drop = FALSE]
        }
        n_accepted <- nrow(samples)

        # provided there have been at least 5 acceptances in the warmup so far
        if (n_accepted > 5) {

          # get the sample posterior variance and shrink it
          sample_var <- sample_variance(samples)
          shrinkage <- 1 / (n_accepted + 5)
          var_shrunk <- n_accepted * shrinkage * sample_var + 5e-3 * shrinkage
          self$parameters$diag_sd <- sqrt(var_shrunk)

        }

      }

    }

  )
)

