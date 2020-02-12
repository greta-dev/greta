# calculating values outside inference

#' @name calculate
#' @title calculate greta arrays given fixed values
#' @description Calculate the values that greta arrays would take, given
#'   temporary, or simulated values for the greta arrays on which they depend.
#'   This can be used to check the behaviour of your model, make predictions to
#'   new data after model fitting, or simulate datasets from either the prior or
#'   posterior of your model.
#'
#' @param ... one or more greta_arrays for which to calculate the value
#' @param values a named list giving temporary values of the greta arrays with
#'   which \code{target} is connected, or a \code{greta_mcmc_list} object
#'   returned by \code{\link{mcmc}}.
#' @param nsim an optional positive integer scalar for the number of responses
#'   to simulate if stochastic greta arrays are present in the model - see Details.
#' @param precision the floating point precision to use when calculating values.
#' @param trace_batch_size the number of posterior samples to process at a time
#'   when \code{target} is a \code{greta_mcmc_list} object; reduce this to
#'   reduce memory demands
#' @param seed an optional seed to be used in set.seed immediately before the
#'   simulation so as to generate a reproducible sample
#'
#' @return Values of the target greta array(s), given values of the greta arrays
#'   on which they depend (either specified in \code{values} or sampled from
#'   their priors). If \code{values} is a  \code{\link[mcmc]{greta_mcmc_list}}
#'   and \code{nsim = NULL}, this will be a \code{greta_mcmc_list} object of
#'   posterior samples for the target greta arrays. Otherwise, the result will
#'   be a named list of numeric R arrays. If \code{nsim = NULL} the dimensions
#'   of returned numeric R arrays will be the same as the corresponding greta
#'   arrays, otherwise an additional dimension with \code{nsim} elements will be
#'   prepended, to represent multiple simulations.
#'
#' @details The greta arrays named in \code{values} need not be variables, they
#'   can also be other operations or even data.
#'
#'   At present, if \code{values} is a named list it must contain values for
#'   \emph{all} of the variable greta arrays with which \code{target} is
#'   connected, even values are given for intermediate operations, or the target
#'   doesn't depend on the variable. That may be relaxed in a future release.
#'
#'   If the model contains stochastic greta arrays; those with a distribution,
#'   calculate can be used to sample from these distributions (and all greta
#'   arrays that depend on them) by setting the \code{nsim} argument to a
#'   positive integer for the required number of samples. If \code{values} is
#'   specified (either as a list of fixed values or as draws), those values will
#'   be used, and remaining variables will be sampled conditional on them.
#'   Observed data with distributions (i.e. response variables defined with
#'   \code{distribution()} can also be sampled, provided they are defined as
#'   greta arrays. This behaviour can be used for a number of tasks, like
#'   simulating datasets for known parameter sets, simulating parameters and
#'   data from a set of priors, or simulating datasets from a model posterior.
#'   See some examples of these below.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # define a variable greta array, and another that is calculated from it
#' # then calculate what value y would take for different values of x
#' x <- normal(0, 1, dim = 3)
#' a <- lognormal(0, 1)
#' y <- sum(x ^ 2) + a
#' calculate(y, values = list(x = c(0.1, 0.2, 0.3), a = 2))
#'
#' # by setting nsim, you can also sample values from their priors
#' calculate(y, nsim = 3)
#'
#' # you can combine sampling and fixed values
#' calculate(y, values = list(a = 2), nsim = 3)
#'
#' # if the greta array only depends on data,
#' # you can pass an empty list to values (this is the default)
#' x <- ones(3, 3)
#' y <- sum(x)
#' calculate(y)
#'
#' # define a model
#' alpha <- normal(0, 1)
#' beta <- normal(0, 1)
#' sigma <- lognormal(1, 0.1)
#' y <- as_data(iris$Petal.Width)
#' mu <- alpha + iris$Petal.Length * beta
#' distribution(y) <- normal(mu, sigma)
#' m <- model(alpha, beta, sigma)
#'
#' # sample values of the parameters, or different observation data (y), from
#' # the priors (useful for prior # predictive checking) - see also
#' # ?simulate.greta_model
#' calculate(alpha, beta, sigma, nsim = 100)
#' calculate(y, nsim = 100)
#'
#' # calculate intermediate greta arrays, given some parameter values (useful
#' # for debugging models)
#' calculate(mu[1:5], values = list(alpha = 1, beta = 2, sigma = 0.5))
#' calculate(mu[1:5], values = list(alpha = -1, beta = 0.2, sigma = 0.5))
#'
#' # simulate datasets given fixed parameter values
#' calculate(y, values = list(alpha = -1, beta = 0.2, sigma = 0.5), nsim = 10)
#'
#' # you can use calculate in conjunction with posterior samples from MCMC, e.g.
#' # sampling different observation datasets, given a random set of these
#' # posterior samples - useful for posterior predictive model checks
#' draws <- mcmc(m, n_samples = 500)
#' calculate(y, values = draws, nsim = 100)
#'
#' # you can use calculate on greta arrays created even after the inference on
#' # the model - e.g. to plot response curves
#' petal_length_plot <- seq(min(iris$Petal.Length),
#'                          max(iris$Petal.Length),
#'                          length.out = 100)
#' mu_plot <- alpha + petal_length_plot * beta
#' mu_plot_draws <- calculate(mu_plot, values = draws)
#' mu_est <- colMeans(mu_plot_draws[[1]])
#' plot(mu_est ~ petal_length_plot, type = "n",
#'      ylim = range(mu_plot_draws[[1]]))
#' apply(mu_plot_draws[[1]], 1, lines,
#'       x = petal_length_plot, col = grey(0.8))
#' lines(mu_est ~ petal_length_plot, lwd = 2)
#'
#' # trace_batch_size can be changed to trade off speed against memory usage
#' # when calculating. These all produce the same result, but have increasing
#' # memory requirements:
#' mu_plot_draws_1 <- calculate(mu_plot, values = draws, trace_batch_size = 1)
#' mu_plot_draws_10 <- calculate(mu_plot, values = draws, trace_batch_size = 10)
#' mu_plot_draws_inf <- calculate(mu_plot, values = draws, trace_batch_size = Inf)
#'
#' }
calculate <- function(...,
                      values = list(),
                      nsim = NULL,
                      precision = c("double", "single"),
                      trace_batch_size = 100,
                      seed = NULL) {

  # turn the provided greta arrays into a list and try to find the names
  target <- list(...)
  names <- names(target)

  # see if any names are missing and try to fill them in
  if (is.null(names)) {
    names_missing <- rep(TRUE, length(target))
  } else {
    names_missing <- names == ""
  }

  if (any(names_missing)) {
    scraped_names <- substitute(list(...))[-1]
    missing_names <- vapply(scraped_names[names_missing], deparse, "")
    names[names_missing] <- missing_names
    names(target) <- names
  }

  # catch empty lists here, since check_greta_arrays assumes data greta arrays
  # have been stripped out
  if (identical(target, list())) {
    stop ("no greta arrays to calculate were provided",
          call. = FALSE)
  }

  # check the inputs
  check_greta_arrays(target,
                     "calculate",
                     "\nPerhaps you forgot to explicitly name other arguments?")

  # checks and RNG seed setting if we're sampling
  if (!is.null(nsim)) {

    # check nsim is valid
    nsim <- check_positive_integer(nsim)

    # if an RNG seed was provided use it and reset the RNG on exiting
    if (!is.null(seed)) {

      if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        runif(1)
      }

      R.seed <- get(".Random.seed", envir = .GlobalEnv)
      on.exit(assign(".Random.seed", R.seed, envir = .GlobalEnv))
      set.seed(seed)

    }
  }

  # set precision
  tf_float <- switch(match.arg(precision),
                     double = "float64",
                     single = "float32")

  if (inherits(values, "greta_mcmc_list")) {

    result <- calculate_greta_mcmc_list(
      target = target,
      values = values,
      nsim = nsim,
      tf_float = tf_float,
      trace_batch_size = trace_batch_size
    )

  } else {

    result <- calculate_list(target = target,
                             values = values,
                             nsim = nsim,
                             tf_float = tf_float,
                             env = parent.frame())

  }

  # if the result wasn't mcmcsamples or simulations, drop the batch dimension
  if (!inherits(result, "greta_mcmc_list") & is.null(nsim)) {
      result <- lapply(result, drop_first_dim)
  }

  result

}

#' @importFrom coda thin
#' @importFrom stats start end
calculate_greta_mcmc_list <- function(target,
                                      values,
                                      nsim,
                                      tf_float,
                                      trace_batch_size) {

  stochastic <- !is.null(nsim)

  # check trace_batch_size is valid
  trace_batch_size <- check_trace_batch_size(trace_batch_size)

  # get the free state draws and old dag from the samples
  model_info <- get_model_info(values)
  mcmc_dag <- model_info$model$dag
  draws <- model_info$raw_draws

  # build a new dag from the targets and set the mode (whether we also need to
  # IID sample some nodes)
  dag <- dag_class$new(target, tf_float = tf_float)
  dag$mode <- ifelse(stochastic, "hybrid", "all_forward")

  # find variable nodes in the new dag that don't have a free state in the old one.
  mcmc_dag_variables <- mcmc_dag$node_list[mcmc_dag$node_types == "variable"]
  dag_variables <- dag$node_list[dag$node_types == "variable"]
  stateless_names <- setdiff(names(dag_variables), names(mcmc_dag_variables))
  dag$variables_without_free_state <- dag_variables[stateless_names]

  # check there's some commonality between the two dags
  connected_to_draws <- names(dag$node_list) %in% names(mcmc_dag$node_list)
  if (!any(connected_to_draws)) {
    stop ("the target greta arrays do not appear to be connected ",
          "to those in the greta_mcmc_list object",
          call. = FALSE)
  }

  # if they didn't specify nsim, check we can deterministically compute the
  # targets from the draws
  if (!stochastic) {

    # see if the new dag introduces any new variables
    new_types <- dag$node_types[!connected_to_draws]
    if (any(new_types == "variable")) {
      stop ("the target greta arrays are related to new variables ",
            "that are not in the MCMC samples so cannot be calculated ",
            "from the samples alone. Set 'nsim' if you want to sample them ",
            "conditionally on the MCMC samples",
            call. = FALSE)
    }

    # see if any of the targets are stochastic and not sampled in the mcmc
    target_nodes <- lapply(target, get_node)
    target_node_names <- vapply(target_nodes,
                                member,
                                "unique_name",
                                FUN.VALUE = character(1))
    existing_variables <- target_node_names %in% names(mcmc_dag_variables)
    have_distributions <- vapply(target_nodes,
                                 has_distribution,
                                 FUN.VALUE = logical(1))
    new_stochastics <- have_distributions & !existing_variables
    if (any(new_stochastics)) {
      n_stoch <- sum(new_stochastics)
      stop ("the greta array",
            ngettext(n_stoch, " ", "s "),
            paste(names(target)[new_stochastics], collapse = ", "),
            ngettext(n_stoch,
                     " has a distribution and is ",
                     " have distributions and are"),
            "not in the MCMC samples, so cannot be calculated ",
            "from the samples alone. Set 'nsim' if you want to sample them ",
            "conditionally on the MCMC samples",
            call. = FALSE)
    }

  }

  # is this still needed?
  self <- dag  # mock for scoping
  self
  dag$define_tf()

  dag$target_nodes <- lapply(target, get_node)
  names(dag$target_nodes) <- names(target)

  # if we're doing stochastic sampling, subsample the draws
  if (stochastic) {

    draws <- as.matrix(draws)
    n_samples <- nrow(draws)

    # if nsim is greater than the number of samples, sample with replacement and warn
    replace <- FALSE
    if (nsim > n_samples) {
      replace <- TRUE
      warning("nsim was greater than the number of posterior samples in ",
              "values, so posterior samples had to be drawn with replacement",
              call. = FALSE)
    }

    rows <- sample.int(n_samples, nsim, replace = replace)
    draws <- draws[rows, , drop = FALSE]

    # add the batch size to the data list
    dag$set_tf_data_list("batch_size", as.integer(nsim))

    # pass these values in as the free state
    trace <- dag$trace_values(draws, trace_batch_size = trace_batch_size, flatten = FALSE)

    # hopefully values is already a list of the correct dimensions...


  } else {

    # for deterministic posterior prediction, just trace the target for each
    # chain
    values <- lapply(draws,
                     dag$trace_values,
                     trace_batch_size = trace_batch_size)

    # convert to a greta_mcmc_list object, retaining windowing info
    trace <- lapply(
      values,
      coda::mcmc,
      start = stats::start(draws),
      end = stats::end(draws),
      thin = coda::thin(draws)
    )
    trace <- coda::mcmc.list(trace)
    trace <- as_greta_mcmc_list(trace, model_info)
  }

  trace

}

calculate_list <- function(target, values, nsim, tf_float, env) {

  stochastic <- !is.null(nsim)

  fixed_greta_arrays <- list()

  if (!identical(values, list())) {

    # check the list of values makes sense, and return these and the corresponding
    # greta arrays (looked up by name in environment env)
    values_list <- check_values_list(values, env)
    fixed_greta_arrays <- values_list$fixed_greta_arrays
    values <- values_list$values

  }

  all_greta_arrays <- c(fixed_greta_arrays, target)
  # define the dag and TF graph
  dag <- dag_class$new(all_greta_arrays, tf_float = tf_float)

  # convert to nodes, and add tensor names to values
  fixed_nodes <- lapply(fixed_greta_arrays, get_node)
  names(values) <- vapply(fixed_nodes, dag$tf_name, FUN.VALUE = character(1))

  # change dag mode to sampling
  dag$mode <- "all_sampling"

  dag$define_tf()
  tfe <- dag$tf_environment

  # build and send a dict for the fixed values
  fixed_nodes <- lapply(fixed_greta_arrays,
                        get_node)

  names(values) <- vapply(fixed_nodes,
                          dag$tf_name,
                          FUN.VALUE = "")

  # check we can do the calculation
  if (stochastic) {

    # check there are no variables without distributions (or whose children have
    # distributions - for lkj & wishart) that aren't given fixed values
    variables <- dag$node_list[dag$node_types == "variable"]
    have_distributions <- vapply(variables, has_distribution, FUN.VALUE = logical(1))
    any_child_has_distribution <- function (variable) {
      have_distributions <- vapply(variable$children,
                                   has_distribution,
                                   FUN.VALUE = logical(1))
      any(have_distributions)
    }
    children_have_distributions <- vapply(variables,
                                          any_child_has_distribution,
                                          FUN.VALUE = logical(1))

    unsampleable <- !have_distributions & !children_have_distributions
    fixed_node_names <- vapply(fixed_nodes,
                               member,
                               "unique_name",
                               FUN.VALUE = character(1))
    unfixed <- !names(variables) %in% fixed_node_names

    if (any(unsampleable & unfixed)) {
      stop ("the target greta arrays are related to variables ",
            "that do not have distributions so cannot be sampled",
            call. = FALSE)
    }

  } else {

    # check there are no unspecified variables on which the target depends
    lapply(target, check_dependencies_satisfied, fixed_greta_arrays, dag, env)

  }

  # look up the tf names of the target greta arrays (under sampling)
  # create an object in the environment that's a list of these, and sample that
  target_nodes <- lapply(target, get_node)
  target_names_list <- lapply(target_nodes, dag$tf_name)
  target_tensor_list <- lapply(target_names_list, get, envir = tfe)
  assign("calculate_target_tensor_list", target_tensor_list, envir = tfe)

  # add the batch size to the data list
  batch_size <- ifelse(stochastic, as.integer(nsim), 1L)
  dag$set_tf_data_list("batch_size", batch_size)

  # add values or data not specified by the user
  data_list <- dag$get_tf_data_list()
  missing <- !names(data_list) %in% names(values)

  # send list to tf environment and roll into a dict
  values <- lapply(values, add_first_dim)
  values <- lapply(values, tile_first_dim, batch_size)

  dag$build_feed_dict(values, data_list = data_list[missing])

  # run the sampling
  dag$tf_sess_run("calculate_target_tensor_list", as_text = TRUE)

}

