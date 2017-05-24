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
#' x = rnorm(10)
#' distribution(x) = normal(mu, sigma)
#'
#' m <- define_model(mu, sigma)
#'
#' }
define_model <- function (...) {

  check_tf_version('error')

  tf$reset_default_graph()

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

  if (length(target_greta_arrays) == 0) {
    stop ('could not find any non-data greta arrays',
          call. = FALSE)
  }

  # get the dag containing the target nodes
  dag <- dag_class$new(target_greta_arrays)

  # get and check the types
  types <- dag$node_types

  # the user might pass greta arrays with groups of nodes that are unconnected
  # to one another. Need to check there are densities in each graph

  # so find the subgraph to which each node belongs
  graph_id <- dag$subgraph_membership()

  graphs <- unique(graph_id)
  n_graphs <- length(graphs)

  # separate messages to avoid the subgraphs issue for beginners
  if (n_graphs == 1) {
    density_message <- paste('none of the greta arrays in the model are',
                             'associated with a probability density, so a',
                             'model cannot be defined')
    variable_message <- paste('none of the greta arrays in the model are',
                              'unknown, so a model cannot be defined')
  } else {
    density_message <- paste('the model contains', n_graphs, 'disjoint graphs,',
                             'one or more of these sub-graphs does not contain',
                             'any greta arrays that are associated with a',
                             'probability density, so a model cannot be',
                             'defined')
    variable_message <- paste('the model contains', n_graphs, 'disjoint',
                              'graphs, one or more of these sub-graphs does',
                              'not contain any greta arrays that are unknown,',
                              'so a model cannot be defined')
  }

  for (graph in graphs) {

    types_sub <- types[graph_id == graph]

    # check they have a density among them
    if (!('distribution' %in% types_sub))
      stop (density_message, call. = FALSE)

    # check they have a variable node among them
    if (!('variable' %in% types_sub))
      stop (variable_message, call. = FALSE)

  }

  # define the TF graph
  dag$define_tf()

  # create the model object and add the arraysof interest
  model <- as.greta_model(dag)
  model$target_greta_arrays <- target_greta_arrays
  model$visible_greta_arrays <- all_greta_arrays(parent.frame())

  model

}

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

#' @details Currently, the only implemented MCMC procedure is static Hamiltonian
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
#'   increase \code{epsilon}. You can also
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
      message('warming up')

    # run it
    warmup_draws <- method(dag = dag,
                           init = initial_values,
                           n_samples = warmup,
                           thin = thin,
                           verbose = verbose,
                           tune = TRUE,
                           control = con)

    # use the last draw of the full parameter vector as the init
    initial_values <- attr(warmup_draws, 'last_x')
    con <- attr(warmup_draws, 'control')

    if (verbose)
      message('sampling')

  }

  # run the sampler
  draws <- method(dag = dag,
                  init = initial_values,
                  n_samples = n_samples,
                  thin = thin,
                  verbose = verbose,
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
      setTxtProgressBar(pb, i)

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

  if (verbose)
    close(pb)

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
