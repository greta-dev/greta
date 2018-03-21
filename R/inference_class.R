# R6 inference class objects

# base node class
inference <- R6Class(
  'inference',
  public = list(

    model = NULL,

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
    initialize = function (initial_values, model, parameters = list()) {

      self$parameters <- parameters
      self$model <- model
      self$n_free <- length(model$dag$example_parameters())
      self$free_state <- self$initial_values(initial_values)
      self$n_traced <- length(model$dag$trace_values(self$free_state))

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
          stop ('The log density and gradients could not be evaluated at these ',
                'initial values.',
                call. = FALSE)
        }

        initial_values <- user_specified

      } else {

        # otherwise, try several times to generate some
        valid <- FALSE
        attempts <- 1
        while (!valid & attempts < 20) {

          initial_values <- rnorm(self$n_free, 0, 0.5)

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

      self$model$dag$send_parameters(parameters)
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

    # get the values of the target greta arrays for the latest batch of raw draws, and
    trace = function () {

      # append the free state trace
      self$traced_free_state <- rbind(self$traced_free_state,
                                      self$last_burst_free_states)

      # calculate the observed values
      last_burst_values <- self$trace_burst_values(self$last_burst_free_states)
      self$traced_values <- rbind(self$traced_values,
                                  last_burst_values)

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

    # given the progress through this warmup phase, and the latest acceptance
    # information, tune the sampler parameters
    tune = function (iterations_completed, total_iterations) {

      something_to_tune <- !identical(self$tuning_periods, list())

      if (something_to_tune) {

        tuning_now <- vapply(self$tuning_periods,
                             self$in_periods,
                             iterations_completed,
                             total_iterations,
                             FUN.VALUE = FALSE)

        parameters_to_tune <- self$tuned_parameters[tuning_now]

        # loop through those parameters, using their tuning method to update the
        # parameter

      }

    },

    # is the sampler in one of the tuning periods for a given parameter
    in_periods = function (periods, i, n_samples) {

      within <- function (period, fraction)
        fraction >= period[1] & fraction <= period[2]

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

#     initialize = function (initial_values, model, parameters = list()) {
#       super$initialize(initial_values, model, parameters)
#     },

    run_chain = function (n_samples, thin, warmup, verbose, pb_update) {

      self$print_chain_number()

      self$traced_free_state = matrix(NA, 0, self$n_free)
      self$traced_values = matrix(NA, 0, self$n_traced)

      # if warmup is required, do that now
      if (warmup > 0) {

        if (verbose) {
          pb_warmup <- create_progress_bar("warmup", c(warmup, n_samples), pb_update)
          iterate_progress_bar(pb_warmup, 0, 0)
        } else {
          pb_warmup <- NULL
        }

        # split up warmup iterations into bursts of sampling
        burst_lengths <- self$burst_lengths(warmup, pb_update, warmup = TRUE)
        completed_iterations <- cumsum(burst_lengths)

        for (burst in seq_along(burst_lengths)) {

          self$run_burst(burst_lengths[burst], thin = thin)
          # how far through the warmup phase are we?
          self$tune(completed_iterations[burst], warmup)

          if (verbose) {
            iterate_progress_bar(pb_warmup,
                                 it = completed_iterations[burst],
                                 rejects = self$numerical_rejections)
          }

        }

      }

      # main sampling
      if (verbose) {
        pb_sampling <- create_progress_bar('sampling', c(warmup, n_samples), pb_update)
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

        # when to stop for training periods
        tuning_periods <- unlist(self$tuning_periods)

        if (!is.null(tuning_periods)) {

          changepoints <- c(changepoints,
                            round(tuning_periods * n_samples))

        }

      }

      changepoints <- sort(unique(changepoints))
      diff(changepoints)

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

    # the sections of the warmup period in which to tune each thing: c(start,
    # stop) as fractions
    tuning_periods = list(epsilon = list(c(0, 0.1),
                                         c(0.4, 1)),
                          diag_sd = list(c(0.1, 0.4))),
#
#     initialize = function (initial_values, model, parameters = list()) {
#       super$initialize(initial_values, model, parameters)
#     },
#
#     run_chain = function (n_samples, thin, warmup, verbose, pb_update) {
#       super$run_chain(n_samples, thin, warmup, verbose, pb_update)
#     },

    # run the sampler for n_samples (possibly thinning)
    run_burst = function (n_samples, thin) {

      self$last_burst_free_states <- matrix(NA, n_samples, self$n_free)

      # method using R HMC (later convert to tensorflow HMC)

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

            # on rejection, reset all the parameters and push old parameters to the
            # graph for the trace
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

      # assign the free state at the end of this burst
      self$free_state <- x

    }

  )
)
