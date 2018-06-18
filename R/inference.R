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

    stash <- greta_stash$trace_stash

    # get them, remove the NAs, and return
    draws_clean <- na.omit(stash$trace)
    draws_prepped <- prepare_draws(draws_clean)
    draws_mcmclist <- mcmc.list(draws_prepped)

    # prep the raw model objects
    model_info <- new.env()
    raw_clean <- na.omit(stash$raw)
    raw_prepped <- prepare_draws(raw_clean)
    model_info$raw_draws <- mcmc.list(raw_prepped)
    model_info$model <- greta_stash$model

    # add the raw draws as an attribute
    attr(draws_mcmclist, "model_info") <- model_info

    return (draws_mcmclist)

  } else {

    return (invisible())

  }

}

# create an object stash in greta's namespace, to return traces to the user when
# they abort a run
greta_stash <- new.env()

stash_trace <- function (trace, raw) {
  assign('trace_stash',
         list(trace = trace,
              raw = raw),
         envir = greta_stash)
}

#' @rdname inference
#' @export
#' @importFrom stats rnorm runif
#' @importFrom utils setTxtProgressBar txtProgressBar
#'
#' @param model greta_model object
#' @param method method used to sample or optimise values. Currently only
#'   one method is available for each procedure: \code{hmc} and \code{adagrad}
#' @param n_samples number of MCMC samples to draw (after any warm-up, but
#'   before thinning)
#' @param thin MCMC thinning rate; every \code{thin} samples is retained,
#'   the rest are discarded
#' @param warmup number of samples to spend warming up the mcmc sampler.
#'   During this phase the sampler moves toward the highest density area and
#'   tunes sampler hyperparameters.
#' @param chains number of MCMC chains to run
#' @param verbose whether to print progress information to the console
#' @param pb_update how regularly to update the progress bar (in iterations)
#' @param control an optional named list of hyperparameters and options to
#'   control behaviour of the sampler or optimiser. See Details.
#' @param initial_values an optional vector (or list of vectors, for multiple
#'   chains) of initial values for the free parameters in the model. These will
#'   be used as the starting point for sampling/optimisation.
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
#'   Currently, the only implemented MCMC procedure is static Hamiltonian Monte
#'   Carlo (\code{method = "hmc"}). During the warmup iterations, the leapfrog
#'   stepsize hyperparameter \code{epsilon} is tuned to maximise the sampler
#'   efficiency, and the posterior marginal standard deviations are estimated
#'   \code{diag_sd}. The \code{control} argument can be used to specify the
#'   initial value for \code{epsilon}, \code{diag_sd}, and two other
#'   hyperparameters: \code{Lmin} and \code{Lmax}; positive integers (with
#'   \code{Lmax > Lmin}) giving the upper and lower limits to the number of
#'   leapfrog steps per iteration (from which the number is selected uniformly
#'   at random).
#'
#'   The default control options for HMC are:
#'   \code{control = list(Lmin = 10, Lmax = 20, epsilon = 0.005, diag_sd = 1)}
#'
#' @return \code{mcmc} & \code{stashed_samples} - an \code{mcmc.list} object
#'   that can be analysed using functions from the coda package. This will
#'   contain mcmc samples of the greta arrays used to create \code{model}.
#'
#' @examples
#' \dontrun{
#' # define a simple model
#' mu <- variable()
#' sigma <- lognormal(1, 0.1)
#' x <- rnorm(10)
#' distribution(x) <- normal(mu, sigma)
#' m <- model(mu, sigma)
#'
#' # carry out mcmc on the model
#' draws <- mcmc(m,
#'               n_samples = 100,
#'               warmup = 10)
#' }
mcmc <- function (model,
                  method = c("hmc"),
                  n_samples = 1000,
                  thin = 1,
                  warmup = 100,
                  chains = 1,
                  verbose = TRUE,
                  pb_update = 10,
                  control = list(),
                  initial_values = NULL) {

  method <- match.arg(method)

  # store the model
  greta_stash$model <- model

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

  param <- dag$example_parameters()
  n_initial <- length(param)

  # random starting locations
  if (is.null(initial_values)) {

    # try several times
    valid <- FALSE
    attempts <- 1
    while (!valid & attempts < 20) {

      initial_values <- replicate(chains,
                                  rnorm(n_initial, 0, 0.5),
                                  simplify = FALSE)

      # test validity of values
      valid <- valid_parameters(initial_values, dag)
      attempts <- attempts + 1

    }

    if (!valid) {
      stop ("Could not find reasonable starting values after ", attempts,
            " attempts. Please specify initial values manually via the ",
            "initial_values argument to mcmc",
            call. = FALSE)
    }

  } else {

    # if they provided a list, check it
    if (is.list(initial_values)) {

      n_sets <- length(initial_values)

      if (n_sets != chains) {
        stop (n_sets, " sets of initial values were provided, but there ",
              ifelse(chains > 1, "are ", "is only "), chains, " chain",
              ifelse(chains > 1, "s", ""),
              call. = FALSE)
      }

      n_param <- vapply(initial_values, length, FUN.VALUE = 0)

      if (!all(n_param == n_initial)) {
        stop ("each set of initial values must be a vector of length ",
              n_initial,
              call. = FALSE)
      }

    } else {

      # replicate
      initial_values <- replicate(chains,
                                  initial_values,
                                  simplify = FALSE)

      if (chains > 1) {
        message ("\nonly one set of was initial values given, ",
                 "and was used for all chains\n")
      }

    }

    # check they are valid
    if (!valid_parameters(initial_values, dag)) {
      stop ('The log density and gradients could not be evaluated at these ',
            'initial values.',
            call. = FALSE)
    }

  }


  # get default control options
  con <- switch(method,
                hmc = list(Lmin = 10,
                           Lmax = 20,
                           epsilon = 0.005,
                           diag_sd = 1))

  # update them with user overrides
  con[names(control)] <- control

  # fetch the algorithm
  method <- switch(method,
                   hmc = hmc)

  print_chain <- chains > 1

  chains_list <- lapply(seq_len(chains),
                        run_chain,
                        dag = dag,
                        method = method,
                        n_samples = n_samples,
                        thin = thin,
                        warmup = warmup,
                        chains = chains,
                        verbose = verbose,
                        pb_update = pb_update,
                        control = con,
                        initial_values = initial_values,
                        print_chain = print_chain)

  # get raw_draws
  raw_list <- lapply(chains_list,
                     function (x) {
                       attr(x, "model_info")$raw_draws[[1]]
                       })
  chains_list <- lapply(chains_list, `[[`, 1)

  model_info <- new.env()
  model_info$raw_draws <- do.call(mcmc.list, raw_list)
  model_info$model <- greta_stash$model
  chains <- do.call(mcmc.list, chains_list)
  attr(chains, "model_info") <- model_info
  chains

}


run_chain <- function (chain, dag, method, n_samples, thin,
                       warmup, chains, verbose, pb_update,
                       control, initial_values, print_chain) {

  if (print_chain) {
    msg <- sprintf("\nchain %i/%i\n", chain, chains)
    message(msg)
  }

  initial_values_chain <- initial_values[[chain]]

  # if warmup is required, do that now and update init
  if (warmup > 0) {

    if (verbose)
      pb_warmup <- create_progress_bar('warmup', c(warmup, n_samples), pb_update)
    else
      pb_warmup <- NULL

    # run it
    warmup_draws <- method(dag = dag,
                           init = initial_values_chain,
                           n_samples = warmup,
                           thin = thin,
                           verbose = verbose,
                           pb = pb_warmup,
                           tune = TRUE,
                           stash = FALSE,
                           control = control)

    # use the last draw of the full parameter vector as the init
    initial_values_chain <- attr(warmup_draws, 'last_x')
    control <- attr(warmup_draws, 'control')

  }

  if (verbose)
    pb_sampling <- create_progress_bar('sampling', c(warmup, n_samples), pb_update)
  else
    pb_sampling <- NULL

  # run the sampler
  draws <- method(dag = dag,
                  init = initial_values_chain,
                  n_samples = n_samples,
                  thin = thin,
                  verbose = verbose,
                  pb = pb_sampling,
                  tune = FALSE,
                  stash = TRUE,
                  control = control)

  # if this was successful, trash the stash, prepare and return the draws
  rm('trace_stash', envir = greta_stash)
  draws

}


#' @importFrom coda mcmc mcmc.list
prepare_draws <- function (draws) {
  # given a matrix of draws returned by the sampler, prepare it and return
  draws_df <- data.frame(draws)
  coda::mcmc(draws_df)
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

  # mock up some names to avoid CRAN-check note
  optimiser <- joint_density <- sess <- NULL

  # get the tensorflow environment
  tfe <- model$dag$tf_environment
  on_graph <- model$dag$on_graph
  tf_run <- model$dag$tf_run

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
  on_graph(tfe$optimiser <- do.call(optimise_fun, con))
  tf_run(train <- optimiser$minimize(-joint_density))

  # random initial values if unspecified
  if (is.null(initial_values)) {
    initial_values <- model$dag$example_parameters()
    initial_values[] <- rnorm(length(initial_values))
  }

  # initialize the variables, then set the ones we care about
  tf_run(sess$run(tf$global_variables_initializer()))
  parameters <- relist_tf(initial_values, model$dag$parameters_example)

  for (i in seq_along(parameters)) {
    variable_name <- paste0(names(parameters)[i], '_free')
    vble <- tfe[[variable_name]]
    on_graph( init <- tf$constant(parameters[[i]],
                                  shape = vble$shape,
                                  dtype = tf_float()))
    . <- on_graph(tfe$sess$run(vble$assign(init)))
  }

  diff <- old_obj <- Inf
  it <- 0

  while (it < max_iterations & diff > tolerance) {
    it <- it + 1
    tf_run(sess$run(train))
    obj <- tf_run(sess$run(-joint_density))
    diff <- abs(old_obj - obj)
    old_obj <- obj
  }

  list(par = model$dag$trace_values(),
       value = tf_run(sess$run(joint_density)),
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
