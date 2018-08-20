#' @name inference
#' @title statistical inference on greta models
#' @description Carry out statistical inference on greta models by
#'   MCMC or likelihood/posterior optimisation.
NULL


# create an object stash in greta's namespace, to return traces to the user when
# they abort a run
greta_stash <- new.env()

#' @rdname inference
#' @export
#' @importFrom stats rnorm runif
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom future plan
#'
#' @param model greta_model object
#' @param sampler sampler used to draw values in MCMC. See \code{\link{samplers}} for options.
#' @param n_samples number of MCMC samples to draw per chain (after any warm-up, but
#'   before thinning)
#' @param thin MCMC thinning rate; every \code{thin} samples is retained,
#'   the rest are discarded
#' @param warmup number of samples to spend warming up the mcmc sampler.
#'   During this phase the sampler moves toward the highest density area and
#'   tunes sampler hyperparameters.
#' @param chains number of MCMC chains to run
#' @param n_cores the maximum number of CPU cores used by \emph{each} chain.
#' @param verbose whether to print progress information to the console
#' @param pb_update how regularly to update the progress bar (in iterations)
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
#'   part of the parameter space.
#'
#'   Multiple mcmc chains can be run in parallel by setting the execution plan
#'   with the \code{future} package. Only \code{plan(multisession)} futures or
#'   \code{plan(cluster)} futures that don't use fork clusters are allowed,
#'   since forked processes conflict with tensorflow's parallelism.
#'
#'   If \code{n_cores = NULL} and mcmc chains are being run sequentially, each
#'   chain will be allowed to use all CPU cores. If chains are being run in
#'   parallel, \code{n_cores} will be set so that \code{n_cores * chains} is
#'   less than the number of CPU cores.
#'
#' @return \code{mcmc}, \code{stashed_samples} & \code{extra_samples} - an
#'   \code{mcmc.list} object that can be analysed using functions from the coda
#'   package. This will contain mcmc samples of the greta arrays used to create
#'   \code{model}.
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
                  sampler = hmc(),
                  n_samples = 1000,
                  thin = 1,
                  warmup = 1000,
                  chains = 1,
                  n_cores = NULL,
                  verbose = TRUE,
                  pb_update = 50,
                  initial_values = NULL) {

  check_future_plan()

  # find variable names to label samples
  target_greta_arrays <- model$target_greta_arrays
  names <- names(target_greta_arrays)

  # check they're not data nodes, provide a useful error message if they are
  are_data <- vapply(target_greta_arrays,
                     function (x) inherits(x$node, 'data_node'),
                     FUN.VALUE = FALSE)

  if (any(are_data)) {

    is_are <- ifelse(sum(are_data) == 1,
                     "is a data greta array",
                     "are data greta arrays")
    bad_greta_arrays <- paste(names[are_data],
                              collapse = ", ")
    msg <- sprintf("%s %s, data greta arrays cannot be sampled",
                   bad_greta_arrays,
                   is_are)
    stop (msg, call. = FALSE)

  }

  # get the dag containing the target nodes
  dag <- model$dag

  # turn initial values into a list is needed (checking the length)
  initial_values <- prep_initials(initial_values, chains)

  # create a sampler object for each chain, using these (possibly NULL) initial
  # values
  samplers <- lapply(initial_values,
                     build_sampler,
                     sampler,
                     model)

  # add chain info for printing
  for (i in seq_len(chains)) {
    samplers[[i]]$chain_number <- i
    samplers[[i]]$n_chains <- chains
  }

  run_samplers(samplers = samplers,
               n_samples = n_samples,
               thin = thin,
               warmup = warmup,
               verbose = verbose,
               pb_update = pb_update,
               n_cores = n_cores,
               from_scratch = TRUE)

}

#' @importFrom future future resolved value
run_samplers <- function (samplers,
                          n_samples,
                          thin,
                          warmup,
                          verbose,
                          pb_update,
                          n_cores,
                          from_scratch) {

  # check the future plan is valid
  check_future_plan()

  dag <- samplers[[1]]$model$dag
  sequential <- inherits(future::plan(), "sequential")
  chains <- samplers[[1]]$n_chains
  n_cores <- check_n_cores(n_cores, chains, sequential)
  float_type <- dag$tf_float

  # stash the samplers now, to retrieve draws later
  greta_stash$samplers <- samplers

  parallel_reporting <- verbose & !sequential & !is.null(greta_stash$callbacks)

  if (!sequential & chains > 1) {
    cores_text <- ifelse(n_cores == 1, "1 core", sprintf("up to %i cores", n_cores))
    cat(sprintf("\nrunning %i chains in parallel, each on %s\n\n",
                chains, cores_text))
    verbose <- FALSE
  }

  n_chain <- length(samplers)
  chains <- seq_len(n_chain)

  # if we're running in parallel and there are callbacks registered,
  # give the samplers somewhere to write their progress
  if (parallel_reporting) {

    log_files <- replicate(n_chain, create_log_file())
    for (chain in chains)
      samplers[[chain]]$trace_log_file <- log_files[[chain]]

    greta_stash$trace_log_files <- log_files
    greta_stash$mcmc_info <- list(n_samples = n_samples)

  }

  # dispatch all the jobs
  futures <- list()
  for (chain in chains) {
    sampler <- samplers[[chain]]
    futures[[chain]] <- future(sampler$run_chain(n_samples = n_samples,
                                                 thin = thin,
                                                 warmup = warmup,
                                                 verbose = verbose,
                                                 pb_update = pb_update,
                                                 sequential = sequential,
                                                 n_cores = n_cores,
                                                 float_type = float_type,
                                                 from_scratch = from_scratch),
                                seed = future_seed())
  }

  # if we're non-sequential and there's a callback registered,
  # loop until they are resolved, executing the callbacks
  if (parallel_reporting) {

    while (!all(vapply(futures, resolved, FALSE))) {

      # loop through callbacks executing them
      for (callback in greta_stash$callbacks)
        callback()

      # get some nap time
      Sys.sleep(0.1)

    }

    cat("\n")

  }

  # then retrieve the samplers
  samplers <- lapply(futures, value)

  # if we were running in parallel, we need to put the samplers back in the
  # stash to return
  if (!sequential) {
    greta_stash$samplers <- samplers
  }

  # get chains from the samplers, with raw values as an attribute
  draws <- stashed_samples()

  # empty the stash
  rm("samplers", envir = greta_stash)

  draws

}


#' @rdname inference
#' @export
#' @importFrom stats na.omit
#' @importFrom coda mcmc.list
#'
#' @details If the sampler is aborted before finishing, the samples collected so
#'   far can be retrieved with \code{stashed_samples()}. Only samples from the
#'   sampling phase will be returned.
stashed_samples <- function () {

  stashed <- exists("samplers", envir = greta_stash)

  if (stashed) {

    samplers <- greta_stash$samplers

    # get draws as a matrix
    free_state_draws <- lapply(samplers, member, "traced_free_state")
    values_draws <- lapply(samplers, member, "traced_values")

    # convert to mcmc objects
    free_state_draws <- lapply(free_state_draws, prepare_draws)
    values_draws <- lapply(values_draws, prepare_draws)

    # convert to mcmc.list objects
    free_state_draws <- coda::mcmc.list(free_state_draws)
    values_draws <- coda::mcmc.list(values_draws)

    # prep the raw model objects
    model_info <- new.env()
    model_info$raw_draws <- free_state_draws
    model_info$samplers <- samplers
    model_info$model <- samplers[[1]]$model

    # add the raw draws as an attribute
    attr(values_draws, "model_info") <- model_info

    return (values_draws)

  } else {

    return (invisible())

  }

}

#' @rdname inference
#'
#' @export
#'
#' @param draws an mcmc.list object returned by \code{mcmc} or
#'   \code{stashed_samples}
#'
#' @details Samples returned by \code{mcmc()} and \code{stashed_samples()} can
#'   be added to with \code{extra_samples()}. This continues the chain from the
#'   last value of the previous chain and uses the same sampler and model as was
#'   used to generate the previous samples. It is not possible to change the
#'   sampler or extend the warmup period.
#'
extra_samples <- function (draws,
                           n_samples = 1000,
                           thin = 1,
                           n_cores = NULL,
                           verbose = TRUE,
                           pb_update = 50) {

  model_info <- get_model_info(draws)
  samplers <- model_info$samplers

  # set the last values as the current free state values
  for (sampler in samplers) {
    free_state_draws <- sampler$traced_free_state
    n_draws <- nrow(free_state_draws)
    sampler$free_state <- free_state_draws[n_draws, ]
  }

  run_samplers(samplers = samplers,
               n_samples = n_samples,
               thin = thin,
               warmup = 0L,
               verbose = verbose,
               pb_update = pb_update,
               n_cores = n_cores,
               from_scratch = FALSE)

}

# convert (possibly NULL) user-specified initial values into a list of the
# correct length, with nice error messages
prep_initials <- function (initial_values, n_chains) {

  # if the user provided a list of initial values, check the length
  if (is.list(initial_values)) {

    n_sets <- length(initial_values)

    if (n_sets != n_chains) {
      stop (n_sets, " sets of initial values were provided, but there ",
            ifelse(n_chains > 1, "are ", "is only "), n_chains, " chain",
            ifelse(n_chains > 1, "s", ""),
            call. = FALSE)
    }

  } else {

    # otherwise replicate them for each chain
    if (is.null(initial_values)) {

      initial_values <- replicate(n_chains, NULL)

    } else {

      initial_values <- replicate(n_chains,
                                  initial_values,
                                  simplify = FALSE)

      if (n_chains > 1) {
        message ("\nonly one set of was initial values given, ",
                 "and was used for all chains\n")
      }

    }

  }

  initial_values

}

#' @rdname inference
#' @export
#'
#' @param max_iterations the maximum number of iterations before giving up
#' @param tolerance the numerical tolerance for the solution, the optimiser
#'   stops when the (absolute) difference in the joint density between
#'   successive iterations drops below this level
#' @param optimiser an \code{optimiser} object giving the optimisation algorithm
#'   and parameters See \code{\link{optimisers}}.
#'
#' @return \code{opt} - a list containing the following named elements:
#'   \itemize{
#'    \item{par} {the best set of parameters found}
#'    \item{value} {the log joint density of the model at the parameters par}
#'    \item{iterations} {the number of iterations taken by the optimiser}
#'    \item{convergence} {an integer code, 0 indicates successful completion,
#'     1 indicates the iteration limit \code{max_iterations} had been reached} }
#'
#' @examples
#' \dontrun{
#' # find the MAP estimate
#' opt_res <- opt(m)
#' }
opt <- function (model,
                 optimiser = bfgs(),
                 max_iterations = 100,
                 tolerance = 1e-6,
                 initial_values = NULL) {

  # create R6 object of the right type
  object <- optimiser$class$new(initial_values = initial_values,
                                model = model,
                                name = optimiser$name,
                                method = optimiser$method,
                                parameters = optimiser$parameters,
                                other_args = optimiser$other_args,
                                max_iterations = max_iterations,
                                tolerance = tolerance)

  # run it
  object$run()

}

inference_module <- module(dag_class,
                           progress_bar = progress_bar_module)
