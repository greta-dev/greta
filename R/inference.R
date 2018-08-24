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
#' @param one_by_one whether to run TensorFlow MCMC code one iteration at a
#'   time, so that greta can handle numerical errors as 'bad' proposals (see
#'   below).
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
#'   progress bar will also display the proportion of proposals so far that were
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
                  one_by_one = FALSE,
                  initial_values = initials()) {

  check_future_plan()

  # find variable names to label samples
  target_greta_arrays <- model$target_greta_arrays
  names <- names(target_greta_arrays)

  # check they're not data nodes, provide a useful error message if they are
  are_data <- vapply(target_greta_arrays,
                     function (x) inherits(get_node(x), 'data_node'),
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

  # turn initial values into a list if needed (checking the length), and convert
  # from a named list on the constrained scale to free state vectors
  initial_values <- prep_initials(initial_values, chains, dag)

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
               one_by_one = one_by_one,
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
                          one_by_one,
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
    cores_text <- ifelse(n_cores == 1,
                         "1 core",
                         sprintf("up to %i cores", n_cores))
    msg <- sprintf("\nrunning %i chains in parallel, each on %s\n\n",
                   chains, cores_text)
    message(msg, appendLF = FALSE)
  }

  n_chain <- length(samplers)
  chains <- seq_len(n_chain)

  # determine the type of progress information
  if (bar_width(n_chain) < 42)
    progress_callback <- percentages
  else
    progress_callback <- progress_bars

  greta_stash$callbacks$parallel_progress <- progress_callback

  # if we're running in parallel and there are callbacks registered,
  # give the samplers somewhere to write their progress
  if (parallel_reporting) {

    trace_log_files <- replicate(n_chain, create_log_file())
    percentage_log_files <- replicate(n_chain, create_log_file(TRUE))
    progress_bar_log_files <- replicate(n_chain, create_log_file(TRUE))

    pb_width <- bar_width(n_chain)
    for (chain in chains) {

      # set the log files
      sampler <- samplers[[chain]]
      sampler$trace_log_file <- trace_log_files[[chain]]
      sampler$percentage_file <- percentage_log_files[[chain]]
      sampler$pb_file <- progress_bar_log_files[[chain]]

      # set the progress bar widths for writing
      sampler$pb_width <- pb_width

    }

    greta_stash$trace_log_files <- trace_log_files
    greta_stash$percentage_log_files <- percentage_log_files
    greta_stash$progress_bar_log_files <- progress_bar_log_files
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
                                                 one_by_one = one_by_one,
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
                           pb_update = 50,
                           one_by_one = FALSE) {

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
               one_by_one = one_by_one,
               n_cores = n_cores,
               from_scratch = FALSE)

}

# convert some 'data' values form the constrained to the free state, for a given
# 'node'
to_free <- function (node, data) {
  lower <- node$lower
  upper <- node$upper
  fun <- switch(node$constraint,
                none = function (x) x,
                high = function (x) log(x - lower),
                low = function (x) log(upper - x),
                both = function(x) qlogis((y - lower) / (upper - lower)))
  fun(data)
}

# convert a named list of initial values into the corresponding vector of values
# on the free state
parse_initial_values <- function (initials, dag) {

  # line up greta array names with tf names
  greta_names <- names(dag$parameters_example)
  tf_names <- vapply(names(initials),
                     function(name) {
                       ga <- get(name)
                       node <- get_node(ga)
                       dag$tf_name(node)
                     },
                     "")
  order <- match(greta_names, tf_names)

  # reorder the inits, and transform them to the free scale
  initials <- initials[order]
  initials <- lapply(initials, as.array)
  nodes <- dag$node_list[match(tf_names, dag$node_tf_names)]
  inits_free <- mapply(to_free, nodes, initials)

  # flatten into tensorflow's expected order
  unlist_tf(inits_free)

}

# convert (possibly NULL) user-specified initial values into a list of the
# correct length, with nice error messages
prep_initials <- function (initial_values, n_chains, dag) {

  # if the user passed a single set of initial values, repeat them for all
  # chains
  if (inherits(initial_values, "initials")) {

    initial_values <- replicate(n_chains,
                                initial_values,
                                simplify = FALSE)

    is_blank <- identical(initial_values, initials())

    if (is_blank & n_chains > 1) {
      message ("only one set of initial values was provided, and was ",
               "used for all chains")
    }

  } else if (is.list(initial_values)) {

    # if the user provided a list of initial values, check elements and the length
    are_initials <- lapply(initial_values, inherits, "initials")

    if (all(are_initials)) {

      n_sets <- length(initial_values)

      if (n_sets != n_chains) {
        stop (n_sets, " sets of initial values were provided, but there ",
              ifelse(n_chains > 1, "are ", "is only "), n_chains, " chain",
              ifelse(n_chains > 1, "s", ""),
              call. = FALSE)
      }

    } else {

      initial_values <- NULL

    }

  } else {

    initial_values <- NULL

  }

  # error on a bad object
  if (is.null(initial_values)) {

    stop ("initial_values must an initials object created with initials(), ",
          "or a simple list of initials objects",
          call. = FALSE)

  }

  # convert them to free state vectors
  initial_values <- lapply(initial_values,
                           parse_initial_values,
                           dag)

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
                 initial_values = initials()) {

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

# simple class system for initial values

#' @rdname inference
#' @export
#' @param ... named numeric values, with names giving the greta arrays to which
#'   they correspond
initials <- function (...) {

  values <- list(...)
  names <- names(values)

  if (length(names) != length(values)) {
    stop ("all initial values must be named",
          call. = FALSE)
  }

  values <- lapply(values, as.array)

  are_numeric <- vapply(values, is.numeric, FUN.VALUE = FALSE)
  if (!all(are_numeric)) {
    stop ("initial values must be numeric",
          call. = FALSE)
  }

  class(values) <- c("initials", class(values))
  values
}

#' @export
print.initials <- function (x, ...) {

  if (identical(x, initials())) {

    msg <- "an empty greta initials object"

  } else {

    cat("a greta initials object with values:\n\n")
    print(unclass(x))

  }

  invisible(x)

}


inference_module <- module(dag_class,
                           progress_bar = progress_bar_module)
