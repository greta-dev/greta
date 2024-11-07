#' @name inference
#' @title Statistical inference on greta models.
#' @description Carry out statistical inference on greta models by
#'   MCMC or likelihood/posterior optimisation.
NULL


# create an object stash in greta's namespace, to return traces to the user when
# they abort a run
greta_stash <- new.env()
greta_stash$python_has_been_initialised <- FALSE
greta_stash$numerical_messages <- c(
  "is not invertible",
  "Cholesky decomposition was not successful"
)

#' @rdname inference
#' @export
#' @importFrom stats rnorm runif
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom future plan nbrOfWorkers
#'
#' @param model greta_model object
#' @param sampler sampler used to draw values in MCMC. See
#'   [samplers()] for options.
#' @param n_samples number of MCMC samples to draw per chain (after any warm-up,
#'   but before thinning)
#' @param thin MCMC thinning rate; every `thin` samples is retained, the
#'   rest are discarded
#' @param warmup number of samples to spend warming up the mcmc sampler (moving
#'   chains toward the highest density area and tuning sampler hyperparameters).
#' @param chains number of MCMC chains to run
#' @param n_cores the maximum number of CPU cores used by each sampler (see
#'   details).
#' @param verbose whether to print progress information to the console
#' @param pb_update how regularly to update the progress bar (in iterations).
#'   If `pb_update` is less than or equal to `thin`, it will be set
#'   to `thin + 1` to ensure at least one saved iteration per
#'   `pb_update` iterations.
#' @param one_by_one whether to run TensorFlow MCMC code one iteration at a
#'   time, so that greta can handle numerical errors as 'bad' proposals (see
#'   below).
#' @param initial_values an optional `initials` object (or list of
#'   `initials` objects of length `chains`) giving initial values for
#'   some or all of the variables in the model. These will be used as the
#'   starting point for sampling/optimisation.
#' @param trace_batch_size the number of posterior samples to process at a time
#'   when tracing the parameters of interest; reduce this to reduce memory
#'   demands
#'
#' @param compute_options Default is to use CPU only with `cpu_only()`. Use
#'   `gpu_only()` to use only GPU. In the future we will add more options for
#'   specifying CPU and GPU use.
#'
#' @details For `mcmc()` if `verbose = TRUE`, the progress bar shows
#'   the number of iterations so far and the expected time to complete the phase
#'   of model fitting (warmup or sampling). Occasionally, a proposed set of
#'   parameters can cause numerical instability (I.e. the log density or its
#'   gradient is `NA`, `Inf` or `-Inf`); normally because the log
#'   joint density is so low that it can't be represented as a floating point
#'   number. When this happens, the progress bar will also display the
#'   proportion of proposals so far that were 'bad' (numerically unstable) and
#'   therefore rejected. Some numerical instability during the warmup phase is
#'   normal, but 'bad' samples during the sampling phase can lead to bias in
#'   your posterior sample. If you only have a few bad samples (<10\\%), you can
#'   usually resolve this with a longer warmup period or by manually defining
#'   starting values to move the sampler into a more reasonable part of the
#'   parameter space. If you have more samples than that, it may be that your
#'   model is misspecified. You can often diagnose this by using
#'   [calculate()] to evaluate the values of greta arrays, given
#'   fixed values of model parameters, and checking the results are what you
#'   expect.
#'
#'   greta runs multiple chains simultaneously with a single sampler,
#'   vectorising all operations across the chains. E.g. a scalar addition in
#'   your model is computed as an elementwise vector addition (with vectors
#'   having length `chains`), a vector addition is computed as a matrix
#'   addition etc. TensorFlow is able to parallelise these operations, and this
#'   approach reduced computational overheads, so this is the most efficient of
#'   computing on multiple chains.
#'
#'   Multiple mcmc samplers (each of which can simultaneously run multiple
#'   chains) can also be run in parallel by setting the execution plan with the
#'   `future` package. Only `plan(multisession)` futures or
#'   `plan(cluster)` futures that don't use fork clusters are allowed,
#'   since forked processes conflict with TensorFlow's parallelism. Explicitly
#'   parallelising chains on a local machine with `plan(multisession)` will
#'   probably be slower than running multiple chains simultaneously in a single
#'   sampler (with `plan(sequential)`, the default) because of the overhead
#'   required to start new sessions. However, `plan(cluster)` can be used
#'   to run chains on a cluster of machines on a local or remote network. See
#'   [future::cluster()] for details, and the
#'   `future.batchtools` package to set up plans on clusters with job
#'   schedulers.
#'
#'   If `n_cores = NULL` and mcmc samplers are being run sequentially, each
#'   sampler will be allowed to use all CPU cores (possibly to compute multiple
#'   chains sequentially). If samplers are being run in parallel with the
#'   `future` package, `n_cores` will be set so that `n_cores *
#'   [future::nbrOfWorkers]` is less than the number
#'   of CPU cores.
#'
#'   After carrying out mcmc on all the model parameters, `mcmc()`
#'   calculates the values of (i.e. traces) the parameters of interest for each
#'   of these samples, similarly to [calculate()]. Multiple
#'   posterior samples can be traced simultaneously, though this can require
#'   large amounts of memory for large models. As in `calculate`, the
#'   argument `trace_batch_size` can be modified to trade-off speed against
#'   memory usage.
#'
#' @note to set a seed with MCMC you can use [set.seed()], or
#'   [tensorflow::set_random_seed()]. They both given identical results. See
#'   examples below.
#'
#' @return `mcmc`, `stashed_samples` & `extra_samples` - a
#'   `greta_mcmc_list` object that can be analysed using functions from the
#'   coda package. This will contain mcmc samples of the greta arrays used to
#'   create `model`.
#'
#' @examples
#' \dontrun{
#' # define a simple Bayesian model
#' x <- rnorm(10)
#' mu <- normal(0, 5)
#' sigma <- lognormal(1, 0.1)
#' distribution(x) <- normal(mu, sigma)
#' m <- model(mu, sigma)
#'
#' # carry out mcmc on the model
#' draws <- mcmc(m, n_samples = 100)
#'
#' # add some more samples
#' draws <- extra_samples(draws, 200)
#'
#' #' # initial values can be passed for some or all model variables
#' draws <- mcmc(m, chains = 1, initial_values = initials(mu = -1))
#'
#' # if there are multiple chains, a list of initial values should be passed,
#' # othewise the same initial values will be used for all chains
#' inits <- list(initials(sigma = 0.5), initials(sigma = 1))
#' draws <- mcmc(m, chains = 2, initial_values = inits)
#'
#' # you can auto-generate a list of initials with something like this:
#' inits <- replicate(4,
#'   initials(mu = rnorm(1), sigma = runif(1)),
#'   simplify = FALSE
#' )
#' draws <- mcmc(m, chains = 4, initial_values = inits)
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
#' mean((x - mean(x))^2) # same
#' var(x) # different
#'
#' # initial values can also be passed to optimisers:
#' o <- opt(m2, initial_values = initials(variance = 1))
#'
#' # and you can return a list of the Hessians for each of these parameters
#' o <- opt(m2, hessian = TRUE)
#' o$hessian
#'
#'
#' # to get a hessian matrix across multiple greta arrays, you must first
#' # combine them and then split them up for use in the model (so that the
#' # combined vector is part of the model) and pass that vector to model:
#' params <- c(variable(), variable(lower = 0))
#' mu <- params[1]
#' variance <- params[2]
#' distribution(x) <- normal(mu, sqrt(variance))
#' m3 <- model(params)
#' o <- opt(m3, hessian = TRUE)
#' o$hessian
#'
#' # using set.seed or tensorflow::set_random_seed to set RNG for MCMC
#' a <- normal(0, 1)
#' y <- normal(a, 1)
#' m <- model(y)
#'
#' set.seed(12345)
#' one <- mcmc(m, n_samples = 1, chains = 1)
#' set.seed(12345)
#' two <- mcmc(m, n_samples = 1, chains = 1)
#' # same
#' all.equal(as.numeric(one), as.numeric(two))
#' tensorflow::set_random_seed(12345)
#' one_tf <- mcmc(m, n_samples = 1, chains = 1)
#' tensorflow::set_random_seed(12345)
#' two_tf <- mcmc(m, n_samples = 1, chains = 1)
#' # same
#' all.equal(as.numeric(one_tf), as.numeric(two_tf))
#' # different
#' all.equal(as.numeric(one), as.numeric(one_tf))
#'
#' }
mcmc <- function(
    model,
    sampler = hmc(),
    n_samples = 1000,
    thin = 1,
    warmup = 1000,
    chains = 4,
    n_cores = NULL,
    verbose = TRUE,
    pb_update = 50,
    one_by_one = FALSE,
    initial_values = initials(),
    trace_batch_size = 100,
    compute_options = cpu_only()
) {

  # set device to be CPU/GPU for the entire run
  with(tf$device(compute_options), {

    # message users about random seeds and GPU usage if they are using GPU
    message_if_using_gpu(compute_options)

    check_if_greta_array_in_mcmc(model)

    # check the trace batch size
    trace_batch_size <- check_trace_batch_size(trace_batch_size)

    check_not_data_greta_arrays(model)

    # get the dag containing the target nodes
    dag <- model$dag

    chains <- check_positive_integer(chains, "chains")

    # turn initial values into a list if needed (checking the length), and convert
    # from a named list on the constrained scale to free state vectors
    initial_values <- prep_initials(initial_values, chains, dag)

    # determine the number of separate samplers to spin up, based on the future
    # plan
    max_samplers <- future::nbrOfWorkers()

    # divide chains up between the workers
    chain_assignment <- sort(rep(
      seq_len(max_samplers),
      length.out = chains
    ))

    # divide the initial values between them
    initial_values_split <- split(initial_values, chain_assignment)

    n_samplers <- length(initial_values_split)

    # create a sampler object for each parallel job, using these (possibly NULL)
    # initial values
    samplers <- lapply(
      initial_values_split,
      build_sampler,
      sampler,
      model,
      compute_options = compute_options
    )

    # add chain info for printing
    for (i in seq_len(n_samplers)) {
      samplers[[i]]$sampler_number <- i
      samplers[[i]]$n_samplers <- n_samplers
    }

    # if verbose = FALSE, make pb_update as big as possible to speed up sampling
    if (!verbose) {
      pb_update <- Inf
    }

    # now make it finite
    pb_update <- min(pb_update, max(warmup, n_samples))
    pb_update <- max(pb_update, thin + 1)

    run_samplers(
      samplers = samplers,
      n_samples = n_samples,
      thin = thin,
      warmup = warmup,
      verbose = verbose,
      pb_update = pb_update,
      one_by_one = one_by_one,
      n_cores = n_cores,
      from_scratch = TRUE,
      trace_batch_size = trace_batch_size,
      compute_options = compute_options
    )

    # close `with` setting to control CPU/GPU usage
  })
}

#' @importFrom future future resolved value
run_samplers <- function(samplers,
                         n_samples,
                         thin,
                         warmup,
                         verbose,
                         pb_update,
                         one_by_one,
                         n_cores,
                         from_scratch,
                         trace_batch_size,
                         compute_options) {

  # check the future plan is valid, and get information about it
  plan_is <- check_future_plan()

  # coerce the iterations to integer
  n_samples <- as.integer(n_samples)
  warmup <- as.integer(warmup)
  thin <- as.integer(thin)

  dag <- samplers[[1]]$model$dag
  chains <- samplers[[1]]$n_chains
  n_cores <- check_n_cores(n_cores, length(samplers), plan_is)
  float_type <- dag$tf_float

  # stash the samplers now, to retrieve draws later
  greta_stash$samplers <- samplers

  parallel_reporting <- verbose &
    plan_is$parallel &
    plan_is$local &
    !is.null(greta_stash$callbacks)

  ## TODO add explaining variable
  if (plan_is$parallel & plan_is$local & length(samplers) > 1) {
    cores_text <- compute_text(n_cores, compute_options)
    msg <- glue::glue(
      "\n",
      "running {length(samplers)} samplers in parallel, ",
      "{cores_text}",
      "\n\n"
    )
    message(msg)
  }

  is_remote_machine <- plan_is$parallel & !plan_is$local
  if (is_remote_machine) {

    cli::cli_inform(
      "running {length(samplers)} \\
      {?sampler on a remote machine/samplers on remote machines}"
    )

  }

  n_chain <- length(samplers)
  chains <- seq_len(n_chain)

  # determine the type of progress information
  if (bar_width(n_chain) < 42) {
    progress_callback <- percentages
  } else {
    progress_callback <- progress_bars
  }

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

  if (plan_is$parallel) {
    dispatch <- future::future
  } else {
    dispatch <- function(expr, ...) expr
  }

  # dispatch all the jobs
  for (chain in chains) {
    sampler <- samplers[[chain]]
    sampler$compute_options <- compute_options
    samplers[[chain]] <- dispatch(
      sampler$run_chain(
        n_samples = n_samples,
        thin = thin,
        warmup = warmup,
        verbose = verbose,
        pb_update = pb_update,
        one_by_one = one_by_one,
        plan_is = plan_is,
        n_cores = n_cores,
        float_type = float_type,
        trace_batch_size = trace_batch_size,
        from_scratch = from_scratch
      ),
      seed = future_seed()
    )
  }

  # if we're non-sequential and there's a callback registered,
  # loop until they are resolved, executing the callbacks
  if (parallel_reporting) {
    while (!all(vapply(samplers, resolved, FALSE))) {

      # loop through callbacks executing them
      for (callback in greta_stash$callbacks) {
        callback()
      }

      # get some nap time
      Sys.sleep(0.1)
    }

    cat("\n")
  }


  # if we were running in parallel, retrieve the samplers and put them back in
  # the stash to return
  if (plan_is$parallel) {
    samplers <- lapply(samplers, future::value)
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
#' @details If the sampler is aborted before finishing (and `future`
#'   parallelism isn't being used), the samples collected so far can be
#'   retrieved with `stashed_samples()`. Only samples from the sampling
#'   phase will be returned.
stashed_samples <- function() {
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
    no_samples <- nrow(values_draws[[1]]) == 0
    if (no_samples) {
      return(replicate(length(samplers), NULL))
    } else {
      thins <- lapply(samplers, member, "thin")

      # convert to mcmc objects, passing on thinning
      free_state_draws <- mapply(prepare_draws,
                                 draws = free_state_draws,
                                 thin = thins,
                                 SIMPLIFY = FALSE
      )
      values_draws <- mapply(prepare_draws,
                             draws = values_draws,
                             thin = thins,
                             SIMPLIFY = FALSE
      )

      # convert to mcmc.list objects
      free_state_draws <- coda::mcmc.list(free_state_draws)
      values_draws <- coda::mcmc.list(values_draws)

      # prep the raw model objects
      model_info <- list(
        raw_draws = free_state_draws,
        samplers = samplers,
        model = samplers[[1]]$model
      )

      values_draws <- as_greta_mcmc_list(values_draws, model_info)

      return(values_draws)
    }
  } else {
    return(invisible())
  }
}

#' @rdname inference
#'
#' @export
#'
#' @param draws a greta_mcmc_list object returned by `mcmc` or
#'   `stashed_samples`
#'
#' @details Samples returned by `mcmc()` and `stashed_samples()` can
#'   be added to with `extra_samples()`. This continues the chain from the
#'   last value of the previous chain and uses the same sampler and model as was
#'   used to generate the previous samples. It is not possible to change the
#'   sampler or extend the warmup period.
#'
extra_samples <- function(draws,
                          n_samples = 1000,
                          thin = 1,
                          n_cores = NULL,
                          verbose = TRUE,
                          pb_update = 50,
                          one_by_one = FALSE,
                          trace_batch_size = 100,
                          compute_options = cpu_only()) {
  model_info <- get_model_info(draws)
  samplers <- model_info$samplers

  # set the last values as the current free state values
  for (sampler in samplers) {
    free_state_draws <- sampler$traced_free_state
    n_draws <- nrow(free_state_draws[[1]])
    free_state_draws <- lapply(free_state_draws, `[`, n_draws, )
    sampler$free_state <- do.call(rbind, free_state_draws)
  }

  run_samplers(
    samplers = samplers,
    n_samples = n_samples,
    thin = thin,
    warmup = 0L,
    verbose = verbose,
    pb_update = pb_update,
    one_by_one = one_by_one,
    n_cores = n_cores,
    from_scratch = FALSE,
    trace_batch_size = trace_batch_size,
    compute_options = compute_options
  )
}

# convert some 'data' values from the constrained to the free state, for a given
# 'node'
#' @importFrom stats qlogis
to_free <- function(node, data) {
  # use reverse mode of bijectors!

  lower <- node$lower
  upper <- node$upper

  # TODO
  # replace these with more informative errors related to the range of values
  unsupported_error <- function() {
    cli::cli_abort(
      "Some provided initial values are outside the range of values their \\
        variables can take"
    )
  }

  high <- function(x) {
    initials_outside_support <- any(x <= lower)
    if (initials_outside_support) {
      unsupported_error()
    }
    log(x - lower)
  }

  low <- function(x) {
    initials_outside_support <- any(x >= upper)
    if (initials_outside_support) {
      unsupported_error()
    }
    log(upper - x)
  }

  both <- function(x) {
    initials_outside_support <- any(x >= upper | x <= lower)
    if (initials_outside_support) {
      unsupported_error()
    }
    stats::qlogis((x - lower) / (upper - lower))
  }

  fun <- switch(node$constraint,
                scalar_all_none = identity,
                scalar_all_high = high,
                scalar_all_low = low,
                scalar_all_both = both
  )

  fun(data)
}

# convert a named list of initial values into the corresponding vector of values
# on the free state
parse_initial_values <- function(initials, dag) {

  # skip if no inits provided
  if (identical(initials, initials())) {
    free_parameters <- dag$example_parameters(free = TRUE)
    free_parameters <- unlist_tf(free_parameters)
    return(free_parameters)
  }

  # find the elements we have been given initial values for
  tf_names <- vapply(names(initials),
                     function(name, env) {
                       ga <- get(name, envir = env)
                       node <- get_node(ga)
                       dag$tf_name(node)
                     },
                     env = parent.frame(4),
                     FUN.VALUE = ""
  )

  check_greta_arrays_associated_with_model(tf_names)

  params <- dag$example_parameters(free = FALSE)
  idx <- match(tf_names, names(params))

  # make nodes do this conversion and checking in the future also make them
  # handle more complex situations (like Wishart, which is an operation with a
  # distribution, so needs to find the corresponding variable), and check the
  # provided values are in the support of the (constrained scale of the)
  # variable

  # find the corresponding nodes and check they are variable nodes
  forward_names <- glue::glue("all_forward_{dag$node_tf_names}")
  nodes <- dag$node_list[match(tf_names, forward_names)]

  check_nodes_all_variable(nodes)

  target_dims <- lapply(params[idx], dim)
  replacement_dims <- lapply(initials, dim)
  check_initial_values_correct_dim(target_dims, replacement_dims)

  # convert the initial values to their free states
  inits_free <- mapply(to_free, nodes, initials, SIMPLIFY = FALSE)

  # set them in the list and flatten to a vector in the same order as tensorflow
  params[idx] <- inits_free
  params <- unlist_tf(params)

  # force them to be row vectors and return
  params <- matrix(params, nrow = 1)
  params
}

# convert (possibly NULL) user-specified initial values into a list of the
# correct length, with nice error messages
prep_initials <- function(initial_values, n_chains, dag) {

  # if the user passed a single set of initial values, repeat them for all
  # chains
  if (is.initials(initial_values)) {
    inform_if_one_set_of_initials(initial_values, n_chains)

    initial_values <- replicate(n_chains,
                                initial_values,
                                simplify = FALSE
    )
  }

  # TODO: revisit logic here for errors and messages
  check_initial_values_match_chains(initial_values, n_chains)
  check_initial_values_correct_class(initial_values)

  # convert them to free state vectors
  initial_values <- lapply(
    initial_values,
    parse_initial_values,
    dag
  )

  initial_values
}

#' @rdname inference
#' @export
#' @param ... named numeric values, giving initial values of some or all of the
#'   variables in the model (unnamed variables will be automatically
#'   initialised)
#'
initials <- function(...) {
  values <- list(...)
  names <- names(values)

  check_initials_are_named(values)

  # coerce to greta-array-like shape
  values <- lapply(values, as_2d_array)

  check_initials_are_numeric(values)

  class(values) <- c("initials", class(values))
  values
}

#' @export
print.initials <- function(x, ...) {
  empty_initials <- identical(x, initials())
  if (empty_initials) {
    cat("an empty greta initials object")
  } else {
    cat("a greta initials object with values:\n\n")
    print(unclass(x))
  }

  invisible(x)
}

#' @rdname inference
#' @export
#'
#' @param max_iterations the maximum number of iterations before giving up
#' @param tolerance the numerical tolerance for the solution, the optimiser
#'   stops when the (absolute) difference in the joint density between
#'   successive iterations drops below this level
#' @param optimiser an `optimiser` object giving the optimisation algorithm
#'   and parameters See [optimisers()].
#' @param adjust whether to account for Jacobian adjustments in the joint
#'   density. Set to `FALSE` (and do not use priors) for maximum likelihood
#'   estimates, or `TRUE` for maximum *a posteriori* estimates.
#' @param hessian whether to return a list of *analytically* differentiated
#'   Hessian arrays for the parameters
#' @param compute_options Default is to use CPU only with `cpu_only()`. Use
#'   `gpu_only()` to use only GPU. In the future we will add more options for
#'   specifying CPU and GPU use.
#'
#' @details Because `opt()` acts on a list of greta arrays with possibly
#'   varying dimension, the `par` and `hessian` objects returned by
#'   `opt()` are named lists, rather than a vector (`par`) and a
#'   matrix (`hessian`), as returned by [stats::optim()].
#'   Because greta arrays may not be vectors, the Hessians may not be matrices,
#'   but could be higher-dimensional arrays. To return a Hessian matrix covering
#'   multiple model parameters, you can construct your model so that all those
#'   parameters are in a vector, then split the vector up to define the model.
#'   The parameter vector can then be passed to model. See example.
#'

#' @return `opt` - a list containing the following named elements:
#'   \itemize{
#'    \item `par` a named list of the optimal values for the greta arrays
#'     specified in `model`
#'    \item `value` the (unadjusted) negative log joint density of the
#'     model at the parameters 'par'
#'    \item `iterations` the number of iterations taken by the optimiser
#'    \item `convergence` an integer code, 0 indicates successful
#'     completion, 1 indicates the iteration limit `max_iterations` had
#'     been reached
#'   \item `hessian` (if `hessian = TRUE`) a named list of hessian
#'     matrices/arrays for the parameters (w.r.t. `value`)
#'  }
#'
opt <- function(model,
                optimiser = bfgs(),
                max_iterations = 100,
                tolerance = 1e-6,
                initial_values = initials(),
                adjust = TRUE,
                hessian = FALSE,
                compute_options = cpu_only()) {

  # set device to be CPU/GPU for the entire run
  with(tf$device(compute_options), {

    # message users about random seeds and GPU usage if they are using GPU
    message_if_using_gpu(compute_options)

    # check initial values. Can up the number of chains in the future to handle
    # random restarts
    initial_values_list <- prep_initials(initial_values, 1, model$dag)

    # create R6 object of the right type
    object <- optimiser$class$new(
      initial_values = initial_values_list[1],
      model = model,
      name = optimiser$name,
      method = optimiser$method,
      parameters = optimiser$parameters,
      other_args = optimiser$other_args,
      max_iterations = max_iterations,
      tolerance = tolerance,
      adjust = adjust
    )

    # run it and get the outputs
    object$run()
    outputs <- object$return_outputs()

    # optionally evaluate the hessians at these parameters (return as hessian for
    # objective function)
    if (hessian) {
      which_objective <- ifelse(adjust, "adjusted", "unadjusted")
      hessians <- model$dag$hessians(
        # coercing to Variable as TFP free state is a Tensor not a variable
        # and you can overload a Variable as a Variable without consequence
        # (hopefully)
        free_state = tf$Variable(object$free_state),
        which_objective = which_objective
      )
      hessians <- lapply(hessians, `*`, -1)
      outputs$hessian <- hessians
    }

    outputs

    # close setting of CPU/GPU usage
  })
}

inference_module <- module(dag_class,
                           progress_bar = progress_bar_module
)
