# calculating values outside inference

#' @name calculate
#' @title calculate greta arrays given fixed values
#' @description Calculate the values that greta arrays would take, given
#'   temporary, or simulated values for the greta arrays on which they depend.
#'   This can be used to check the behaviour of your model, make predictions to
#'   new data after model fitting, or simulate datasets from either the prior or
#'   posterior of your model.
#'
#' @param target a greta array or list of greta arrays for which to calculate
#'   the value
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
#' @return Values of the greta array(s) in \code{target} calculated given values
#'   of the greeta arrays on which they depend ( either specified in
#'   \code{values} or sampled from their priors). Structured either as a numeric
#'   R array (if \code{target} is a single greta array), a named list of numeric
#'   R arrays (if \code{target} is a list of greta arrays), or a
#'   \code{\link[mcmc]{greta_mcmc_list}} object of posterior samples (if
#'   \code{values} is a \code{greta_mcmc_list} and \code{nsim = NULL}). If
#'   \code{nsim = NULL} the dimensions of returned numeric R arrays will be the
#'   same as the corresponding greta arrays, otherwise an additional dimension
#'   with \code{nsim} elements will be prepended, to represent multiple
#'   simulations.
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
#' calculate(y, list(x = c(0.1, 0.2, 0.3), a = 2))
#'
#' # by setting nsim, you can also sample values from their priors
#' calculate(y, nsim = 3)
#'
#' # you can combine sampling and fixed values
#' calculate(y, list(a = 2), nsim = 3)
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
#' calculate(list(alpha, beta, sigma), nsim = 100)
#' calculate(y, nsim = 100)
#'
#' # calculate intermediate greta arrays, given some parameter values (useful
#' # for debugging models)
#' calculate(mu[1:5], list(alpha = 1, beta = 2, sigma = 0.5))
#' calculate(mu[1:5], list(alpha = -1, beta = 0.2, sigma = 0.5))
#'
#' # simulate datasets given fixed parameter values
#' calculate(y, list(alpha = -1, beta = 0.2, sigma = 0.5), nsim = 10)
#'
#' # you can use calculate in conjunction with posterior samples from MCMC, e.g.
#' # sampling different observation datasets, given a random set of these
#' # posterior samples - useful for posterior predictive model checks
#' draws <- mcmc(m, n_samples = 500)
#' calculate(y, draws, nsim = 100)
#'
#' # you can use calculate on greta arrays created even after the inference on
#' # the model - e.g. to plot response curves
#' petal_length_plot <- seq(min(iris$Petal.Length),
#'                          max(iris$Petal.Length),
#'                          length.out = 100)
#' mu_plot <- alpha + petal_length_plot * beta
#' mu_plot_draws <- calculate(mu_plot, draws)
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
#' mu_plot_draws_1 <- calculate(mu_plot, draws, trace_batch_size = 1)
#' mu_plot_draws_10 <- calculate(mu_plot, draws, trace_batch_size = 10)
#' mu_plot_draws_inf <- calculate(mu_plot, draws, trace_batch_size = Inf)
#'
#' }
calculate <- function(target, values = list(),
                      nsim = NULL,
                      precision = c("double", "single"),
                      trace_batch_size = 100,
                      seed = NULL) {

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

  # need to enable lists of targets

  target_name <- deparse(substitute(target))
  tf_float <- switch(match.arg(precision),
                     double = "float64",
                     single = "float32")

  if (!inherits(target, "greta_array"))
    stop("'target' is not a greta array")

  if (inherits(values, "greta_mcmc_list")) {
    calculate_greta_mcmc_list(
      target = target,
      target_name = target_name,
      values = values,
      tf_float = tf_float,
      trace_batch_size = trace_batch_size
    )
  }
  else {
    calculate_list(target = target,
                   values = values,
                   tf_float = tf_float,
                   env = parent.frame())
  }

}

#' @importFrom coda thin
#' @importFrom stats start end
calculate_greta_mcmc_list <- function(target,
                                      target_name,
                                      values,
                                      tf_float,
                                      trace_batch_size) {

  # check trace_batch_size is valid
  trace_batch_size <- check_trace_batch_size(trace_batch_size)

  model_info <- get_model_info(values)

  # copy and refresh the dag
  dag <- model_info$model$dag$clone()
  dag$new_tf_environment()

  # set the precision in the dag
  dag$tf_float <- tf_float

  # extend the dag to include this node, as the target
  dag$build_dag(list(target))

  self <- dag  # mock for scoping
  self
  dag$define_tf()

  dag$target_nodes <- list(get_node(target))
  names(dag$target_nodes) <- target_name

  param <- dag$example_parameters(free = TRUE)
  param <- unlist_tf(param)
  param[] <- 0

  # raw draws are either an attribute, or this object
  model_info <- attr(values, "model_info")
  draws <- model_info$raw_draws

  # trace the target for each chain
  values <- lapply(draws, dag$trace_values, trace_batch_size = trace_batch_size)

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
  trace

}

calculate_list <- function(target, values, tf_float, env) {

  # check the list of values makes sense, and return these and the corresponding
  # greta arrays (looked up by name in environment env)
  values_list <- check_values_list(values, env)
  fixed_greta_arrays <- values_list$fixed_greta_arrays
  values <- values_list$values

  all_greta_arrays <- c(fixed_greta_arrays, list(target))

  # define the dag and TF graph
  dag <- dag_class$new(all_greta_arrays, tf_float = tf_float)
  dag$define_tf()
  tfe <- dag$tf_environment

  # build and send a dict for the fixed values
  fixed_nodes <- lapply(fixed_greta_arrays,
                        get_node)

  names(values) <- vapply(fixed_nodes,
                          dag$tf_name,
                          FUN.VALUE = "")

  # check that there are no unspecified variables on which the target depends
  check_dependencies_satisfied(target, fixed_greta_arrays, dag, env)

  # add values or data not specified by the user
  data_list <- dag$get_tf_data_list()
  missing <- !names(data_list) %in% names(values)

  # send list to tf environment and roll into a dict
  values <- lapply(values, add_first_dim)
  dag$build_feed_dict(values, data_list = data_list[missing])

  name <- dag$tf_name(get_node(target))
  result <- dag$tf_sess_run(name, as_text = TRUE)

  drop_first_dim(result)

}

