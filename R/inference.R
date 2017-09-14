#' @name inference
#' @title statistical inference on greta models
#' @description Carry out statistical inference on greta models by
#'   MCMC or likelihood/posterior optimisation.
NULL

#' @rdname inference
#' @export
#' @importFrom stats na.omit
#'
#' @details If the sampler is aborted before finishing, the samples collected so
#'   far can be retrieved with \code{stashed_samples()}. Only samples from the
#'   sampling phase will be returned.
stashed_samples <- function () {

  stashed <- exists('trace_stash', envir = greta_stash)

  if (stashed) {

    # get them, remove the NAs, and return
    draws <- greta_stash$trace_stash
    draws_clean <- na.omit(draws)
    draws_prepped <- prepare_draws(draws_clean)

    return (draws_prepped)

  } else {

    return (invisible())

  }

}

# create an object stash in greta's namespace, to return traces to the user when
# they abort a run
greta_stash <- new.env()

stash_trace <- function (trace)
  assign('trace_stash', trace, envir = greta_stash)

#' @rdname inference
#' @export
#' @importFrom stats rnorm runif
#' @importFrom utils setTxtProgressBar txtProgressBar
#'
#' @param model greta_model object
#' @param method the method used to sample or optimise values. Currently only
#'   one method is available for each procedure: \code{hmc} and \code{adagrad}
#' @param n_samples the number of MCMC samples to draw (after any warm-up, but
#'   before thinning)
#' @param thin the MCMC thinning rate; every \code{thin} samples is retained,
#'   the rest are discarded
#' @param warmup the number of samples to spend warming up the mcmc sampler.
#'   During this phase the sampler moves toward the highest density area and
#'   tunes sampler hyperparameters.
#' @param verbose whether to print progress information to the console
#' @param pb_update how regularly to update the progress bar (in iterations)
#' @param control an optional named list of hyperparameters and options to
#'   control behaviour of the sampler or optimiser. See Details.
#' @param initial_values an optional named vector of initial values for the free
#'   parameters in the model. These will be used as the starting point for
#'   sampling/optimisation
#'
#' @details For \code{mcmc()} if \code{verbose = TRUE}, the progress bar shows
#'   the number of iterations so far and the expected time to complete the phase
#'   of model fitting (warmup or sampling). Updating the progress bar regularly
#'   slows down sampling, by as much as 9 seconds per 1000 updates. So if you
#'   want the sampler to run faster, you can change \code{pb_update} to increase
#'   the number of iterations between updates of the progress bar, or turn the
#'   progress bar off altogether by setting \code{verbose = FALSE}.
#'
#'   Occasionally, a proposed set of parameters can cause numerical instability
#'   (I.e. the log density or its gradient is \code{NA}, \code{Inf} or
#'   \code{-Inf}); normally because the log joint density is so low that it
#'   can't be represented as a floating point number. When this happens, the
#'   progress bar will also display the proportion of samples so far that were
#'   'bad' (numerically unstable) and therefore rejected.
#'   If you're getting a lot of numerical instability, you might want to
#'   manually define starting values to move the sampler into a more reasonable
#'   part of the parameter space. Alternatively, you could redefine the model
#'   (via \code{model}) to have double precision, though this will slow down
#'   sampling.
#'
#'   Currently, the only implemented MCMC procedure is static Hamiltonian
#'   Monte Carlo (\code{method = "hmc"}). During the warmup iterations, the
#'   leapfrog stepsize hyperparameter \code{epsilon} is tuned to maximise the
#'   sampler efficiency. The \code{control} argument can be used to specify the
#'   initial value for epsilon, along with two other hyperparameters: \code{Lmin}
#'   and \code{Lmax}; positive integers (with \code{Lmax > Lmin}) giving the
#'   upper and lower limits to the number of leapfrog steps per iteration (from
#'   which the number is selected uniformly at random).
#'
#'   The default control options for HMC are:
#'   \code{control = list(Lmin = 10, Lmax = 20, epsilon = 0.005)}
#'
#' @return \code{mcmc} & \code{stashed_samples} - an \code{mcmc.list} object that can be analysed using
#'   functions from the coda package. This will contain mcmc samples of the
#'   greta arrays used to create \code{model}.
#'
#' @examples
#' \dontrun{
#' # define a simple model
#' mu = variable()
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
                  method = c("hmc", "slice", "default"),
                  n_samples = 1000,
                  thin = 1,
                  warmup = 100,
                  verbose = TRUE,
                  pb_update = 10,
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

  # extract variable nodes
  variable_nodes <- dag$node_tf_names[grep("variable", dag$node_tf_names)]

  # extract discrete vars if they exist
  if (dag$discrete) {
    discrete_vars <- sapply(dag$node_list[names(variable_nodes)],
                            function(x)
                              x$distribution$discrete)
    discrete_names <- variable_nodes[which(discrete_vars)]
    # check that sampler is OK
    if (method == "hmc") {
      is_are <- ifelse(sum(discrete_vars) == 1,
                       'is a discrete random variable',
                       'are discrete random variables')
      bad_greta_arrays <- paste(discrete_names, collapse = ', ')
      msg <- sprintf("%s %s, discrete random variables cannot be sampled with hmc sampler",
                     bad_greta_arrays,
                     is_are)
      stop(msg, call. = FALSE)
    }
  }

  # random starting locations
  if (is.null(initial_values)) {

    # try several times
    valid <- FALSE
    attempts <- 1
    initial_values <- dag$example_parameters()

    if (dag$discrete) {
      # identify discrete parameters
      discrete <- grepl(paste(discrete_names, collapse = "|"), names(initial_values))
    }

    while (!valid & attempts < 10) {

      # increase the jitter each time for continuous vars
      if (dag$discrete) {
        # set different inits for discrete and continuous variables
        initial_values[!discrete] <- rnorm(sum(!discrete), 0, 1 + attempts / 5)
        initial_values[discrete] <- rbinom(sum(discrete), 1, 0.5)
      } else {
        initial_values[] <- rnorm(length(initial_values), 0, 1 + attempts / 5)
      }

      # test validity of values
      valid <- greta:::valid_parameters(dag, initial_values)
      attempts <- attempts + 1

    }

    if (!valid) {
      stop ('Could not find reasonable starting values after ', attempts,
            ' attempts. Please specify initial values manually via the ',
            'initial_values argument to mcmc',
            call. = FALSE)
    }
  } else {
    if (!greta:::valid_parameters(dag, initial_values)) {
      stop ('The log density and gradients could not be evaluated at these ',
            'initial values.',
            call. = FALSE)
    }
  }

  # get default control options
  ## tidy this to give method specific options only
  con <- list(Lmin = 10,
              Lmax = 20,
              w_size = 1.0,
              max_iter = 10000,
              epsilon = 0.005,
              slice_eps = 0.0001)

  # update them with user overrides
  con[names(control)] <- control

  # fetch the algorithm
  method <- switch(method,
                   hmc = hmc,
                   slice = slice,
                   default = hybrid)

  # if warmup is required, do that now and update init
  if (warmup > 0) {

    if (verbose)
      pb_warmup <- create_progress_bar('warmup', c(warmup, n_samples), pb_update)
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
                           stash = FALSE,
                           control = con)

    # use the last draw of the full parameter vector as the init
    initial_values <- attr(warmup_draws, 'last_x')
    con <- attr(warmup_draws, 'control')

  }

  if (verbose)
    pb_sampling <- create_progress_bar('sampling', c(warmup, n_samples), pb_update)
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
                  stash = TRUE,
                  control = con)

  # if this was successful, trash the stash, prepare and return the draws
  rm('trace_stash', envir = greta_stash)
  prepare_draws(draws)

}

#' @importFrom coda mcmc mcmc.list
prepare_draws <- function (draws) {
  # given a matrix of draws returned by the sampler, prepare it and return
  draws_df <- data.frame(draws)
  draws_mcmc <- coda::mcmc(draws_df)
  coda::mcmc.list(draws_mcmc)
}


#' @rdname inference
#' @export
#'
#' @param max_iterations the maximum number of iterations before giving up
#' @param tolerance the numerical tolerance for the solution, the optimiser stops when the (absolute) difference in the joint density between successive iterations drops below this level
#'
#' @details Currently, the only implemented optimisation algorithm is Adagrad
#'   (\code{method = "adagrad"}). The \code{control} argument can be used to
#'   specify the optimiser hyperparameters: \code{learning_rate} (default 0.8),
#'   \code{initial_accumulator_value} (default 0.1) and \code{use_locking}
#'   (default \code{TRUE}). The are passed directly to TensorFlow's optimisers,
#'   see
#'   \href{https://www.tensorflow.org/api_docs/python/tf/train/AdagradOptimizer}{the
#'   TensorFlow docs} for more information
#'
#' @return \code{opt} - a list containing the following named elements:
#'   \itemize{
#'     \item{par}{the best set of parameters found}
#'     \item{value}{the log joint density of the model at the parameters par}
#'     \item{iterations}{the number of iterations taken by the optimiser}
#'     \item{convergence}{an integer code, 0 indicates successful completion, 1
#'     indicates the iteration limit max_iterations had been reached}
#'   }
#'
#' @examples
#' \dontrun{
#' # find the MAP estimate
#' opt_res <- opt(m)
#' }
opt <- function (model,
                  method = c("adagrad"),
                  max_iterations = 100,
                  tolerance = 1e-6,
                  control = list(),
                  initial_values = NULL) {

  # get the tensorflow environment
  tfe <- model$dag$tf_environment

  # get the method
  method <- match.arg(method)
  optimise_fun <- switch (method,
                          adagrad = tf$train$AdagradOptimizer)

  # default control options
  con <- switch (method,
                 adagrad = list(learning_rate = 0.8,
                                initial_accumulator_value = 0.1,
                                use_locking = TRUE))

  # update them with user overrides
  con[names(control)] <- control

  # set up optimiser
  tfe$optimiser <- do.call(optimise_fun, con)
  with(tfe, train <- optimiser$minimize(-joint_density))

  # random initial values if unspecified
  if (is.null(initial_values)) {
    initial_values <- model$dag$example_parameters()
    initial_values[] <- rnorm(length(initial_values))
  }

  # initialize the variables, then set the ones we care about
  with(tfe, sess$run(tf$global_variables_initializer()))
  parameters <- relist_tf(initial_values, model$dag$parameters_example)

  for (i in seq_along(parameters)) {
    variable_name <- paste0(names(parameters)[i], '_free')
    vble <- tfe[[variable_name]]
    init <- tf$constant(parameters[[i]],
                        shape = vble$shape,
                        dtype = tf_float())
    tmp <- tfe$sess$run(vble$assign(init))
  }

  diff <- old_obj <- Inf
  it <- 0

  while (it < max_iterations & diff > tolerance) {
    it <- it + 1
    with(tfe, sess$run(train))
    obj <- with(tfe, sess$run(-joint_density))
    diff <- abs(old_obj - obj)
    old_obj <- obj
  }

  list(par = model$dag$trace_values(),
       value = with(tfe, sess$run(joint_density)),
       iterations = it,
       convergence = ifelse(it < max_iterations, 0, 1))

}

stash_module <- module(greta_stash,
                       stash_trace,
                       prepare_draws)

inference_module <- module(dag_class,
                           progress_bar = progress_bar_module,
                           samplers = samplers_module,
                           stash = stash_module)
