#' @name greta-inference
#' @title Statistical Inference on Greta Models
#' @description Define a \code{greta_model} object and carry out statistical
#'   inference on parameters of interest by MCMC
NULL

#' @rdname greta-inference
#' @export
#'
#' @param \dots \code{greta_array} objects to be tracked by the model (i.e.
#'   those for which samples will be retained during mcmc). If not provided, all
#'   of the non-data \code{greta_array} objects defined in the calling
#'   environment will be tracked.
#'
#' @return \code{define_model} - a \code{greta_model} object. See
#'   \code{\link{greta-model}} for details.
#'
#' @examples
#'
#' \dontrun{
#'
#' # define a simple model
#' mu = free()
#' sigma = lognormal(1, 0.1)
#' x = observed(rnorm(10))
#' x %~% normal(mu, sigma)
#'
#' m <- define_model(mu, sigma)
#'
#' }
define_model <- function (...) {

  # nodes required
  target_greta_arrays <- list(...)

  # if no arrays were specified, find all of the non-data arrays
  if (identical(target_greta_arrays, list())) {

    target_greta_arrays <- all_greta_arrays(parent.frame(),
                                            include_data = FALSE)

  } else {

    # otherwise, find variable names for the provided nodes
    names <- substitute(list(...))[-1]
    names <- vapply(names, deparse, '')
    names(target_greta_arrays) <- names

  }

  # get the dag containing the target nodes
  dag <- dag_class$new(target_greta_arrays)

  # define the TF graph
  dag$define_tf()

  # create the model object and add the arraysof interest
  model <- as.greta_model(dag)
  model$target_greta_arrays <- target_greta_arrays

  model

}

#' @rdname greta-inference
#' @export
#' @importFrom stats rnorm runif
#' @importFrom utils setTxtProgressBar txtProgressBar
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
#'   control behaviour of the sampler
#' @param initial_values an optional named vector of initial values for the free
#'   parameters in the model
#'
#' @return \code{mcmc} - a dataframe of mcmc samples of the parameters of interest, as defined
#'   in \code{model}. This dataframe can be coerced to other formats to check
#'   model convergence or summarise results, e.g. using \code{coda::mcmc}.
#'
#' @examples
#' \dontrun{
#' # carry out mcmc on the model
#' draws <- mcmc(m,
#'               n_samples = 100,
#'               warmup = 10)
#' }
mcmc <- function (model,
                  method = c('hmc', 'nuts'),
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
  type <- vapply(target_greta_arrays, member, 'node$type', FUN.VALUE = '')
  bad <- type == 'data'
  if (any(bad)) {
    is_are <- ifelse(sum(bad) == 1, 'is an observed greta array', 'are observed greta arrays')
    bad_greta_arrays <- paste(names[bad], collapse = ', ')
    msg <- sprintf('%s %s, observed greta arrays cannot be sampled',
                   bad_greta_arrays,
                   is_are)
    stop (msg)
  }

  # get the dag containing the target nodes
  dag <- model$dag

  # random starting locations
  if (is.null(initial_values)) {
    initial_values <- dag$example_parameters()
    initial_values[] <- rnorm(length(initial_values), 0, 0.1)
  }

  # get default control options
  con <- switch(method,
                hmc = list(Lmin = 10,
                           Lmax = 20,
                           epsilon = 0.005),
                nuts = list())

  # update them with user overrides
  con[names(control)] <- control

  # fetch the algorithm
  method <- switch(method,
                   hmc = hmc,
                   nuts = nuts)

  # if warmup is required, do that now and update init
  if (warmup > 0) {

    if (verbose)
      message('warming up')

    # run it
    warmup_draws <- method(dag = dag,
                           init = initial_values,
                           n_samples = warmup,
                           thin = thin,
                           verbose = verbose,
                           control = con)

    # use the last draw of the full parameter vector as the init
    init <- attr(warmup_draws, 'last_x')

    if (verbose)
      message('sampling')

  }

  # run the sampler
  draws <- method(dag = dag,
                  init = initial_values,
                  n_samples = n_samples,
                  thin = thin,
                  verbose = verbose,
                  control = con)

  # coerce to data.frame, but keep the sample density
  draws_df <- data.frame(draws)
  attr(draws_df, 'density') <- attr(draws, 'density')
  attr(draws_df, 'last_x') <- attr(draws, 'last_x')
  draws_df

}

# run NUTS HMC sampler
nuts <- function (dag, init, n_samples, thin, verbose, control = list()) {
  stop ('not yet implemented')
}

hmc <- function (dag,
                 init,
                 n_samples,
                 thin,
                 verbose,
                 control = list(Lmin = 10,
                                Lmax = 20,
                                epsilon = 0.005)) {

  # unpack options
  Lmin <- control$Lmin
  Lmax <- control$Lmax
  epsilon <- control$epsilon

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

  # set up log joint density store
  ljd <- rep(NA, n_samples)

  # get free parameter dimension
  npar <- length(x)

  accept_count <- 0

  # set up progress bar
  if (verbose)
    pb <- txtProgressBar(max = n_samples, style = 3)

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

      if (verbose)
        message ('proposal rejected due to numerical instability')

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

      # if (verbose) {
      #
      #   # optionally report acceptance statistics
      #   acceptance_rate <- round(accept_count / i, 3)
      #   message(sprintf('iteration %i, acceptance rate: %s',
      #                   i,
      #                   prettyNum(acceptance_rate)))
      # }

    }

    if (verbose)
      setTxtProgressBar(pb, i)

  }

  if (verbose)
    close(pb)

  attr(trace, 'density') <- -ljd
  attr(trace, 'last_x') <- x
  trace

}
