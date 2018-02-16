hmc <- function (dag,
                 init,
                 n_samples,
                 thin,
                 verbose,
                 pb,
                 tune = FALSE,
                 stash = FALSE,
                 control = list(Lmin = 10,
                                Lmax = 20,
                                epsilon = 0.005,
                                diag_sd = 1)) {

  # unpack options
  Lmin <- control$Lmin
  Lmax <- control$Lmax
  epsilon <- control$epsilon
  diag_sd <- control$diag_sd

  if (tune) {

    # when to start and stop each type of tuning, in fractions of the tuning period
    epsilon_periods <- list(c(0, 0.1), c(0.4, 1))
    diag_sd_periods <- list(c(0.1, 0.4))

    # epslion tuning parameters
    accept_group <- 50
    target_acceptance <- 0.651
    kappa <- 0.75
    gamma <- 0.1

    # initialise welford accumulator for marginal variance
    diag_sd_update_rate <- 5
    welford_m <- 0
    welford_m2 <- 0

    epsilon_trace <- rep(NA, n_samples)

  }

  numerical_rejections <- 0

  # start the progress bar
  if (verbose)
    iterate_progress_bar(pb = pb, it = 0, rejects = 0)

  # set initial location, log joint density and gradients
  x <- init
  dag$send_parameters(x)
  grad <- dag$gradients()
  logprob <- dag$log_density()

  # set up trace store (grab values of target variables from graph to get
  # dimension and names)
  init_trace <- dag$trace_values()
  n_target <- length(init_trace)
  trace <- matrix(NA,
                  nrow = n_samples %/% thin,
                  ncol = n_target)
  colnames(trace) <- names(init_trace)

  # also trace the raw values, with the dag attached
  raw <- matrix(NA,
                nrow = n_samples %/% thin,
                ncol = length(x))
  colnames(raw) <- names(dag$example_parameters())

  # if anything goes awry, stash the trace and raw trace so far
  if (stash)
    on.exit(stash_trace(trace, raw))

  # track acceptance
  accept_trace <- rep(0, n_samples)

  # get free parameter dimension
  npar <- length(x)

  L <- Lmin:Lmax

  # loop through iterations
  for (i in 1:n_samples) {

    # copy old state
    x_old <- x
    logprob_old <- logprob
    grad_old <- grad
    p <- p_old <- rnorm(npar)

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

      numerical_rejections <- numerical_rejections + 1
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

    # either way, store density and location of target parameters straight from the graph
    # reset dag parameters for extracting the trace
    if (i %% thin == 0) {
      dag$send_parameters(x)
      trace[i / thin, ] <- dag$trace_values()
      raw[i / thin, ] <- x
    }

    if (verbose)
      iterate_progress_bar(pb = pb, it = i, rejects = numerical_rejections)

    # optionally tune epsilon
    if (tune) {

      adapt_epsilon <- in_periods(i, n_samples, epsilon_periods)
      if (adapt_epsilon) {

        # acceptance rate over the last accept_group runs
        start <- max(1, i - accept_group)
        end <- i
        accept_rate <- mean(accept_trace[start:end], na.rm = TRUE)

        # decrease the adaptation rate as we go
        adapt_rate <- min(1, gamma * i ^ (-kappa))

        # shift epsilon in the right direction, making sure it never goes negative
        epsilon <- epsilon + pmax(-(epsilon + sqrt(.Machine$double.eps)),
                                  adapt_rate * (accept_rate - target_acceptance))

        # keep track of epsilon
        epsilon_trace[i] <- epsilon

      }

      # only adapt diag_sd in the first third of tuning, so that epsilon can
      # settle in during the second half
      adapt_diag_sd <- in_periods(i, n_samples, diag_sd_periods)
      if (adapt_diag_sd) {

        # update welford accumulator for posterior variance
        n_accepted <- sum(accept_trace)

        # update only if this step was accepted
        if (accept_trace[i] == 1) {
          welford_delta <- x - welford_m
          welford_m <- welford_m + welford_delta / n_accepted
          welford_m2 <- welford_m2 + welford_delta * (x - welford_m)
        }

        # if there are samples, and we want to adapt, get the sample posterior
        # variance and shrink it
        if (n_accepted > 1 & (i %% diag_sd_update_rate == 0)) {
          sample_var <- welford_m2 / (n_accepted - 1)
          var_shrinkage <- 1 / (n_accepted + 5)
          var_shrunk <- n_accepted * var_shrinkage * sample_var + 5e-3 * var_shrinkage
          diag_sd <- sqrt(var_shrunk)
        }

      }

    }

  }

  # store the tuned epsilon as the mean of the last half
  if (tune) {
    start <- floor(n_samples/2)
    end <- n_samples
    control$epsilon <- mean(epsilon_trace[start:end], na.rm = TRUE)
    control$diag_sd <- diag_sd
  }

  stash_trace(trace, raw)
  trace <- stashed_samples()
  attr(trace, 'last_x') <- x
  attr(trace, 'control') <- control
  trace

}

# determine whether the sampler is within one of the adaptation periods for a given parameter
in_periods <- function (i, n_samples, periods) {
  fraction <- i / n_samples
  in_period <- vapply(periods, within, fraction, FUN.VALUE = FALSE)
  any(in_period)
}

within <- function (period, fraction)
  fraction >= period[1] & fraction <= period[2]


samplers_module <- module(hmc)
