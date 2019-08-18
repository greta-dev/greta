# calculating values outside inference

#' @name calculate
#' @title calculate greta arrays given fixed values
#' @description Calculate the values that greta arrays would take, given
#'   temporary values for the greta arrays on which they depend, and return them
#'   as numeric R arrays. This can be used to check the behaviour of your model
#'   or make predictions to new data after model fitting.
#'
#' @param target a greta array for which to calculate the value
#' @param values a named list giving temporary values of the greta arrays with
#'   which \code{target} is connected, or an \code{mcmc.list} object returned by
#'   \code{\link{mcmc}}.
#' @param precision the floating point precision to use when calculating values.
#'
#' @return A numeric R array with the same dimensions as \code{target}, giving
#'   the values it would take conditioned on the fixed values given by
#'   \code{values}.
#'
#' @details The greta arrays named in \code{values} need not be variables, they
#'   can also be other operations or even data.
#'
#'   At present, if \code{values} is a named list it must contain values for
#'   \emph{all} of the variable greta arrays with which \code{target} is
#'   connected, even values are given for intermediate operations, or the target
#'   doesn't depend on the variable. That may be relaxed in a future release.
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
#' mu <- alpha + iris$Petal.Length * beta
#' distribution(iris$Petal.Width) <- normal(mu, sigma)
#' m <- model(alpha, beta, sigma)
#'
#' # calculate intermediate greta arrays, given some parameter values
#' calculate(mu[1:5], list(alpha = 1, beta = 2, sigma = 0.5))
#' calculate(mu[1:5], list(alpha = -1, beta = 0.2, sigma = 0.5))
#'
#'
#' # fit the model then calculate samples at a new greta array
#' draws <- mcmc(m, n_samples = 500)
#' petal_length_plot <- seq(min(iris$Petal.Length),
#'                          max(iris$Petal.Length),
#'                          length.out = 100)
#' mu_plot <- alpha + petal_length_plot * beta
#' mu_plot_draws <- calculate(mu_plot, draws)
#'
#' # plot the draws
#' mu_est <- colMeans(mu_plot_draws[[1]])
#' plot(mu_est ~ petal_length_plot, type = "n",
#'      ylim = range(mu_plot_draws[[1]]))
#' apply(mu_plot_draws[[1]], 1, lines,
#'       x = petal_length_plot, col = grey(0.8))
#' lines(mu_est ~ petal_length_plot, lwd = 2)
#' }
#'
#'
calculate <- function(target, values = list(),
                      precision = c("double", "single")) {

  target_name <- deparse(substitute(target))
  tf_float <- switch(match.arg(precision),
                     double = "float64",
                     single = "float32")

  if (!inherits(target, "greta_array"))
    stop("'target' is not a greta array")

  if (inherits(values, "mcmc.list")) {
    calculate_mcmc.list(target, target_name, values, tf_float)
  } else {
    calculate_list(target, values, tf_float, env = parent.frame())
  }

}

# Begin Exclude Linting
calculate_mcmc.list <- function(target, target_name, values, tf_float) {
# End Exclude Linting

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

  param <- dag$example_parameters()
  param[] <- 0

  # raw draws are either an attribute, or this object
  model_info <- attr(values, "model_info")
  draws <- model_info$raw_draws

  # trace the target for each chain
  values <- lapply(draws, dag$trace_values)
  trace <- lapply(values, coda::mcmc)

  trace <- coda::mcmc.list(trace)
  attr(trace, "model_info") <- model_info
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

