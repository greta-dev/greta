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
#' @importFrom future plan nbrOfWorkers
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
#' # define a simple Bayesian model
#' mu <- normal(0, 5)
#' sigma <- lognormal(1, 0.1)
#' x <- rnorm(10)
#' distribution(x) <- normal(mu, sigma)
#' m <- model(mu, sigma)
#'
#' # carry out mcmc on the model
#' draws <- mcmc(m, n_samples = 100)
#'
#' # add some more samples
#' draws <- extra_samples(draws, 200)
#'
#' # or find the MAP estimate
#' opt_res <- opt(m)
#'
#' # get the MLE of the normal variance
#' mu <- variable()
#' variance <- variable(lower = 0)
#' distribution(x) <- normal(mu, sqrt(variance))
#' m2 <- model(variance)
#'
#' # adjust = FALSE skips the jacobian adjustments used in MAP estimation, to
#' # give the true maximum likelihood estimates
#' o <- opt(m2, adjust = FALSE)
#'
#' # the MLE corresponds to the *unadjusted* sample variance, but differs
#' # from the sample variance
#' o$par
#' mean((x - mean(x)) ^ 2)  # same
#' var(x)  # different
#' }
mcmc <- function (model,
                  sampler = hmc(),
                  n_samples = 1000,
                  thin = 1,
                  warmup = 2000,
                  chains = 1,
                  n_cores = NULL,
                  verbose = TRUE,
                  pb_update = 50,
                  one_by_one = FALSE,
                  initial_values = initials()) {

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

  chains <- check_positive_integer(chains, "chains")

  # turn initial values into a list if needed (checking the length), and convert
  # from a named list on the constrained scale to free state vectors
  initial_values <- prep_initials(initial_values, chains, dag)

  # determine the number of separate samplers to spin up, based on the future plan
  max_samplers <- future::nbrOfWorkers()

  # divide chains up between the workers
  chain_assignment <- sort(rep(seq_len(max_samplers),
                               length.out = chains))

  # divide the initial values between them
  initial_values_split <- split(initial_values, chain_assignment)

  n_samplers <- length(initial_values_split)

  # create a sampler object for each parallel job, using these (possibly NULL)
  # initial values
  samplers <- lapply(initial_values_split,
                     build_sampler,
                     sampler,
                     model)

  # add chain info for printing
  for (i in seq_len(n_samplers)) {
    samplers[[i]]$sampler_number <- i
    samplers[[i]]$n_samplers <- n_samplers
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

    trace_names <- samplers[[1]]$model$dag$trace_names

    # get draws as 3D arrays for each sampler
    free_state_draws <- lapply(samplers, member, "traced_free_state")
    values_draws <- lapply(samplers, member, "traced_values")

    # split along chain dimension, to get a list of chains
    free_state_draws <- unlist(free_state_draws, recursive = FALSE)
    values_draws <- unlist(values_draws, recursive = FALSE)

    # apply the trace names back to them
    values_draws <- lapply(values_draws, `colnames<-`, trace_names)

    # if there are no samples, return a list of NULLs
    if (nrow(values_draws[[1]]) == 0) {

      return (replicate(length(samplers), NULL))

    } else {

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

    }

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

  unsupported_error <- function () {
    stop ("some provided initial values are outside the range of values ",
          "their variables can take",
          call. = FALSE)
  }

  high <- function (x) {
    if (any(x <= lower))
      unsupported_error()
    log(x - lower)
  }

  low <- function (x) {
    if (any(x >= upper))
      unsupported_error()
    log(upper - x)
  }

  both <- function(x) {
    if (any(x >= upper | x <= lower))
      unsupported_error()
    qlogis((x - lower) / (upper - lower))
  }

  fun <- switch(node$constraint,
                none = identity,
                high = high,
                low = low,
                both = both)

  fun(data)

}

# convert a named list of initial values into the corresponding vector of values
# on the free state
parse_initial_values <- function (initials, dag) {

  # skip if no inits provided
  if (identical(initials, initials())) {

    return (dag$example_parameters(flat = TRUE))

  }

  # find the elements we have been given initial values for
  tf_names <- vapply(names(initials),
                     function(name, env) {
                       ga <- get(name, envir = env)
                       node <- get_node(ga)
                       dag$tf_name(node)
                     },
                     env = parent.frame(4),
                     FUN.VALUE = "")

  params <- dag$example_parameters(flat = FALSE)
  idx <- match(tf_names, names(params))

  # make nodes do this conversion and checking in the future also make them
  # handle more complex situations (like Wishart, which is an operation with a
  # distribution, so needs to find the corresponding variable), and check the
  # provided values are in the support of the (constrained scale of the) variable

  # find the corresponding nodes and check they are variable nodes
  nodes <- dag$node_list[match(tf_names, dag$node_tf_names)]
  types <- lapply(nodes, node_type)
  are_variables <- vapply(types, identical, "variable", FUN.VALUE = FALSE)

  if (!all(are_variables)) {
    stop ("initial values can only be set for variable greta arrays",
          call. = FALSE)
  }

  target_dims <- lapply(params[idx], dim)
  replacement_dims <- lapply(initials, dim)
  same_dims <- mapply(identical, target_dims, replacement_dims)

  if (!all(same_dims)) {
    mismatches <- which(!same_dims)
    stop ("the initial values provided have different dimensions ",
          "than the named greta arrays",
          call. = FALSE)
  }

  # convert the initial values to their free states
  inits_free <- mapply(to_free, nodes, initials, SIMPLIFY = FALSE)

  # set them in the list and flatten to a vector in the same order as tensorflow
  params[idx] <- inits_free
  unlist_tf(params)

}

# convert (possibly NULL) user-specified initial values into a list of the
# correct length, with nice error messages
prep_initials <- function (initial_values, n_chains, dag) {

  # if the user passed a single set of initial values, repeat them for all
  # chains
  if (inherits(initial_values, "initials")) {

    is_blank <- identical(initial_values, initials())

    if (!is_blank & n_chains > 1) {
      message ("only one set of initial values was provided, and was ",
               "used for all chains")
    }

    initial_values <- replicate(n_chains,
                                initial_values,
                                simplify = FALSE)

  } else if (is.list(initial_values)) {

    # if the user provided a list of initial values, check elements and the length
    are_initials <- vapply(initial_values, inherits, "initials",
                           FUN.VALUE = FALSE)

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
#' @param adjust whether to account for log jacobian adjustments in the joint
#'   density. Set to \code{FALSE} (and do not use priors) for maximum likelihood
#'   estimates, or \code{TRUE} for maximum \emph{a posteriori} estimates.
#'
#' @return \code{opt} - a list containing the following named elements:
#'   \itemize{
#'    \item{par} {the best set of parameters found}
#'    \item{value} {the (unadjusted) log joint density of the model at the parameters 'par'}
#'    \item{iterations} {the number of iterations taken by the optimiser}
#'    \item{convergence} {an integer code, 0 indicates successful completion,
#'     1 indicates the iteration limit \code{max_iterations} had been reached} }
#'
opt <- function (model,
                 optimiser = bfgs(),
                 max_iterations = 100,
                 tolerance = 1e-6,
                 initial_values = initials(),
                 adjust = TRUE) {

  # check initial values. Can up the number of chains in the future to handle
  # random restarts
  initial_values_list <- prep_initials(initial_values, 1, model$dag)

  # create R6 object of the right type
  object <- optimiser$class$new(initial_values = initial_values_list[1],
                                model = model,
                                name = optimiser$name,
                                method = optimiser$method,
                                parameters = optimiser$parameters,
                                other_args = optimiser$other_args,
                                max_iterations = max_iterations,
                                tolerance = tolerance,
                                adjust = adjust)

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

  # coerce to greta-array-like shape
  values <- lapply(values, as_2D_array)

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

    cat ("an empty greta initials object")

  } else {

    cat("a greta initials object with values:\n\n")
    print(unclass(x))

  }

  invisible(x)

}

inference_module <- module(dag_class,
                           progress_bar = progress_bar_module)
