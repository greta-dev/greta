#' @name greta-inference
#' @title Statistical Inference on Greta Models
#' @description Carry out statistical inference on parameters of interest by MCMC
NULL

#' @rdname greta-inference
#' @export
#' @importFrom stats rnorm runif
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom coda mcmc mcmc.list
#'
#' @param model greta_model object
#' @param method the method used to sample values. Currently only \code{hmc} is
#'   implemented
#' @param n_samples the number of samples to draw (after any warm-up, but before
#'   thinning)
#' @param thin the thinning rate; every \code{thin} samples is retained, the
#'   rest are discarded
#' @param warmup the number of samples to spend warming up the sampler. During
#'   this phase the sampler moves toward the highest density area and may tune
#'   sampler hyperparameters.
#' @param verbose whether to print progress information to the console
#' @param control an optional named list of hyperparameters and options to
#'   control behaviour of the sampler. See Details.
#' @param initial_values an optional named vector of initial values for the free
#'   parameters in the model
#'
#' @details
#'   If \code{verbose = TRUE}, the progress bar shows the number of iterations
#'   so far and the expected time to complete the phase of model fitting (warmup
#'   or sampling). Occasionally, a proposed set of parameters can have such a
#'   low density as to cause numerical instability (I.e. the log density or its
#'   gradient is \code{NA},\code{Inf} or \code{-Inf}). When this happens, the
#'   progress bar will also display the proportion of samples so far that were
#'   'bad' (numerically unstable) and therefore rejected. If you're getting a
#'   lot of these, you may want to manually define starting values to move the
#'   sampler into a more reasonable part of the aprameter space.
#'
#'   Currently, the only implemented MCMC procedure is static Hamiltonian
#'   Monte Carlo (\code{method = 'hmc'}). During the warmup iterations, the
#'   leapfrog stepsize hyperparameter \code{epsilon} is tuned to maximise the
#'   sampler efficiency. The \code{control} argument can be used to specify the
#'   initial value for epsilon, along with two other hyprparameters: \code{Lmin}
#'   and \code{Lmax}; positive integers (with \code{Lmax > Lmin}) giving the
#'   upper and lower limits to the number of leapfrog steps per iteration (from
#'   which the number is selected uniformly at random).
#'
#'   If you're getting a lot of rejected proposals, you might want to reduce
#'   \code{epsilon}, or if the sampler is moving too slowly you might want to
#'   increase \code{epsilon}.
#'
#'   The default control options for HMC are:
#'   \code{control = list(Lmin = 10, Lmax = 20, epsilon = 0.005)}
#'
#' @return \code{mcmc} - an \code{mcmc.list} object that can be analysed using
#'   functions from the coda package. This will on contain mcmc samples of the
#'   parameters of interest, as defined in \code{model}.
#'
#' @examples
#' \dontrun{
#'
#' # define a simple model
#' mu = free()
#' sigma = lognormal(1, 0.1)
#' x = rnorm(10)
#' distribution(x) = normal(mu, sigma)
#' m <- model(mu, sigma)
#'
#' # carry out mcmc on the model
#' draws <- mcmc(m,
#'               n_samples = 100,
#'               warmup = 10)
#' }
mcmc <- function (model,
                  method = c('hmc'),
                  n_samples = 1000,
                  thin = 1,
                  warmup = 100,
                  verbose = TRUE,
                  control = list(),
                  initial_values = NULL) {

  method <- match.arg(method)

  # find variable names to label samples
  target_greta_arrays <- model$target_greta_arrays
  names <- names(target_greta_arrays)

  # check they're not data nodes, provide a useful error message if they are
  are_data <- vapply(target_greta_arrays,
                     function (x) inherits(x$node, 'data_node'),
                     FUN.VALUE = FALSE)

  if (any(are_data)) {
    is_are <- ifelse(sum(are_data) == 1, 'is a data greta array', 'are data greta arrays')
    bad_greta_arrays <- paste(names[are_data], collapse = ', ')
    msg <- sprintf('%s %s, data greta arrays cannot be sampled',
                   bad_greta_arrays,
                   is_are)
    stop (msg, call. = FALSE)
  }

  # get the dag containing the target nodes
  dag <- model$dag

  # random starting locations
  if (is.null(initial_values)) {

    # try several times
    valid <- FALSE
    attempts <- 1
    while (!valid & attempts < 10) {

      initial_values <- dag$example_parameters()
      # increase the jitter each time
      initial_values[] <- rnorm(length(initial_values), 0, 1 + attempts / 5)

      # test validity of values
      valid <- valid_parameters(dag, initial_values)
      attempts <- attempts + 1

    }

    if (!valid) {
      stop ('Could not find reasonable starting values after ', attempts,
            ' attempts. Please specify initial values manually via the ',
            'initial_values argument to mcmc',
            call. = FALSE)
    }

  } else {

    if (!valid_parameters(dag, initial_values)) {
      stop ('The log density and gradients could not be evaluated at these ',
            'initial values.',
            call. = FALSE)
    }

  }


  # get default control options
  con <- switch(method,
                hmc = list(Lmin = 10,
                           Lmax = 20,
                           epsilon = 0.005))

  # update them with user overrides
  con[names(control)] <- control

  # fetch the algorithm
  method <- switch(method,
                   hmc = hmc)

  # if warmup is required, do that now and update init
  if (warmup > 0) {

    if (verbose)
      pb_warmup <- create_progress_bar('warmup', c(warmup, n_samples))
    else
      pb_warmup <- NULL

    # run it
    warmup_draws <- method(dag = dag,
                           init = initial_values,
                           n_samples = warmup,
                           thin = thin,
                           verbose = verbose,
                           pb = pb_warmup,
                           tune = TRUE,
                           control = con)

    # use the last draw of the full parameter vector as the init
    initial_values <- attr(warmup_draws, 'last_x')
    con <- attr(warmup_draws, 'control')

  }

  if (verbose)
    pb_sampling <- create_progress_bar('sampling', c(warmup, n_samples))
  else
    pb_sampling <- NULL


  # run the sampler
  draws <- method(dag = dag,
                  init = initial_values,
                  n_samples = n_samples,
                  thin = thin,
                  verbose = verbose,
                  pb = pb_sampling,
                  tune = FALSE,
                  control = con)

  # coerce to data.frame, but keep the sample density
  draws_df <- data.frame(draws)
  draws_mcmc <- coda::mcmc(draws_df)
  draws_mcmc_list <- coda::mcmc.list(draws_mcmc)

  draws_mcmc_list

}

hmc <- function (dag,
                 init,
                 n_samples,
                 thin,
                 verbose,
                 pb,
                 tune = FALSE,
                 control = list(Lmin = 10,
                                Lmax = 20,
                                epsilon = 0.005)) {

  # unpack options
  Lmin <- control$Lmin
  Lmax <- control$Lmax
  epsilon <- control$epsilon

  # tuning parameters
  accept_group = 50
  target_acceptance = 0.651
  kappa = 0.75
  gamma = 0.1

  numerical_rejections <- 0

  # set initial location, log joint density and gradients
  x <- init
  dag$send_parameters(x)
  grad <- dag$gradients()
  logprob <- dag$log_density()

  if (tune)
    epsilon_trace <- rep(NA, n_samples)

  # set up trace store (grab values of target variables from graph to get
  # dimension and names)
  init_trace <- dag$trace_values()
  n_target <- length(init_trace)
  trace <- matrix(NA,
                  nrow = n_samples %/% thin,
                  ncol = n_target)
  colnames(trace) <- names(init_trace)

  # set up log joint density store
  ljd <- rep(NA, n_samples)

  # track acceptance
  accept_trace <- rep(0, n_samples)

  # get free parameter dimension
  npar <- length(x)

  accept_count <- 0

  # loop through iterations
  for (i in 1:n_samples) {

    # copy old state
    x_old <- x
    logprob_old <- logprob
    grad_old <- grad
    p_old <- rnorm(npar)

    # start leapfrog steps
    reject <- FALSE
    p <- p_old + 0.5 * epsilon * grad
    n_steps <- base::sample(Lmin:Lmax, 1)
    for (l in seq_len(n_steps)) {

      # step
      x <- x + epsilon * p

      # send parameters
      dag$send_parameters(x)
      logprob <- dag$log_density()
      grad <- dag$gradients()

      # check gradients are finite
      if (any(!is.finite(grad))) {
        reject <- TRUE
        break()
      }

      p <- p + epsilon * grad

    }

    p <- p - 0.5 * epsilon * grad

    # if the step was bad, reject it out of hand
    if (reject) {

      numerical_rejections <- numerical_rejections + 1
      x <- x_old
      logprob <- logprob_old
      grad <- grad_old



    } else {

      # otherwise do the Metropolis accept/reject step

      # inner products
      p_prod <- (t(p) %*% p)[1, 1]
      p_prod_old <- (t(p_old) %*% p_old)[1, 1]

      # acceptance ratio
      log_accept_ratio = logprob - 0.5 * p_prod - logprob_old + 0.5 * p_prod_old
      log_u = log(runif(1))

      if (log_u < log_accept_ratio) {

        # on acceptance, iterate the counter and leave the parameters in the dag
        # to be put in the trace
        accept_count <- accept_count + 1
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
      ljd[i / thin] <- dag$log_density()
    }

    if (verbose)
      iterate_progress_bar(pb = pb, it = i, rejects = numerical_rejections)

    # optionally tune epsilon
    if (tune) {

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

  }

  # store the tuned epsilon as the mean of the last half
  if (tune) {
    start <- floor(n_samples/2)
    end <- n_samples
    control$epsilon <- mean(epsilon_trace[start:end], na.rm = TRUE)
  }

  attr(trace, 'density') <- -ljd
  attr(trace, 'last_x') <- x
  attr(trace, 'control') <- control
  trace

}
