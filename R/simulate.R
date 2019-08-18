#' @title Simulate Responses From \code{greta_model} Object
#'
#' @description Simulate responses from a “greta_array” model object, either
#'   from the prior or conditioned on the posterior or some fixed values.
#'
#' @param object a \code{\link{greta_model}} object
#' @param nsim positive integer scalar - the number of responses to simulate
#' @param seed an optional seed to be used in set.seed immediately before the
#'   simulation so as to generate a reproducible sample
#' @param targets  an optional list of greta arrays for which to generate
#'   samples. If not specified, the greta arrays named in the call to model will
#'   be used.
#' @param values a named list giving fixed values of greta arrays in the model
#'   to hold constant, or an \code{mcmc.list} object returned by
#'   \code{\link{mcmc}}.
#' @param ... optional additional arguments, none are used at present
#'
#' @details If the values argument is not provided, \code{simulate()} draws
#'   \code{nsim} independent samples from the model prior for all of the target
#'   greta arrays - either those named in the call to \code{\link{model}} or
#'   those in \code{targets}, if provided.
#'
#'   If \code{values} are provided, samples of the target values will be
#'   independent, conditional on these provided values. If \code{values} is a
#'   named list of values for some greta arrays, each of the \code{nsim}
#'   independent samples of the target greta arrays will be drawn conditional on
#'   those fixed values. If an \code{mcmc.list} object (created by fitting a
#'   greta model with \code{\link{mcmc}}) is passed to values, then \code{nsim}
#'   samples of the target greta arrays will be returned, each of them
#'   conditioned on a different, randomly selected posterior sample from values.
#'
#' @return A named list of vectors, matrices or arrays containing independent
#'   samples of the named greta arrays. The number of samples will be prepended
#'   as the first dimension of the greta array, so that a vector fo samples is
#'   returned for each scalar greta array, and a matrix is returned for each
#'   vector greta array etc.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # build a greta model
#' n <- 10
#' y <- rnorm(n)
#' y <- as_data(y)
#'
#' library(greta)
#' sd <- lognormal(1, 2)
#' mu <- normal(0, 1, dim = n)
#' distribution(y) <- normal(mu, sd)
#' m <- model(mu, sd)
#'
#' # prior simulation:
#'
#' # simulate one random draw of mu and sd from the model prior:
#' sims <- simulate(m)
#'
#' # 100 simulations of mu and sd)
#' sims <- simulate(m, nsim = 100)
#'
#' # 100 simulations of y and mu (greta arrays names in targets)
#' # NB: y must be a greta array here
#' sims <- simulate(m, nsim = 100, targets = list(y, mu))
#'
#' # conditional prior simulation:
#'
#' # 100 simulations of y and mu, with sd fixed at 3
#' sims <- simulate(m, nsim = 100, targets = list(y, mu), values = list(sd = 3))
#'
#' # posterior simulation:
#'
#' # generate posterior samples from the model as usual
#' draws <- mcmc(m)
#'
#' # draw 100 simulations of y, conditioned on 100 random posterior
#' # samples of all the model parameters
#' sims <- simulate(m, nsim = 100, targets = list(y), values = draws)
#' }
simulate.greta_model <- function (
  object,
  nsim = 1,
  seed = NULL,
  targets = list(),
  values = list(),
  ...
) {

  # fetch the nodes for the target greta arrays
  if (!identical(targets, list())) {
    # get_target_greta_arrays does validity checks
    target_greta_arrays <- check_greta_arrays(targets, "simulate")
    target_nodes <- lapply(target_greta_arrays, get_node)
  } else {
    target_nodes <- object$dag$target_nodes
  }

  if (inherits(values, "mcmc.list")) {
    simulate_mcmc.list(object, nsim, seed, target_nodes, values)
  } else {
    simulate_list(object, nsim, seed, target_nodes, values, env = parent.frame())
  }

}


simulate_mcmc.list <- function(model, nsim, target_nodes, values, tf_float) {
  stop ("not implemented yet")
}

simulate_list <- function(model, nsim, target_nodes, values, tf_float, env) {

  # check the list of values makes sense, and return these and the corresponding
  # greta arrays (looked up by name in environment env)
  values_list <- check_values_list(values, env)
  fixed_greta_arrays <- values_list$fixed_greta_arrays
  values <- values_list$values

  # define the dag and TF graph
  dag <- model$dag
  dag$define_tf(mode = "sampling", target_nodes = target_nodes)
  tfe <- dag$tf_environment

  # build and send a dict for the fixed values
  fixed_nodes <- lapply(fixed_greta_arrays, get_node)

  names(values) <- vapply(
    fixed_nodes,
    dag$tf_name,
    mode = "sampling",
    FUN.VALUE = ""
  )

  # add values or data not specified by the user
  data_list <- tfe$data_list
  missing <- !names(data_list) %in% names(values)

  # send list to tf environment and roll into a dict
  values <- lapply(values, add_first_dim)
  dag$build_feed_dict(values, data_list = data_list[missing])

  # look up the tf names of the target greta arrays (under sampling)
  # create an object in the environment that's a list of these, and sample that
  stop ("not implemented yet")
  # name <- dag$tf_name(get_node(target))

  # run the sampling
  result_list <- dag$tf_sess_run(name, as_text = TRUE)

  # tidy up the results
  result_list <- lapply(result_list, drop_first_dim)
  names(result_list) <- names(targets)
  result_list
}
