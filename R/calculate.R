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
#' x = normal(0, 1, dim = 3)
#' a = lognormal(0, 1)
#' y <- sum(x ^ 2) + a
#' calculate(y, list(x = c(0.1, 0.2, 0.3), a = 2))
#'
#'
#' # define a model
#' alpha = normal(0, 1)
#' beta = normal(0, 1)
#' sigma = lognormal(1, 0.1)
#' mu <- alpha + iris$Petal.Length * beta
#' distribution(iris$Petal.Width) = normal(mu, sigma)
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
#' plot(mu_est ~ petal_length_plot, type = "n", ylim = range(mu_plot_draws[[1]]))
#' apply(mu_plot_draws[[1]], 1, lines, x = petal_length_plot, col = grey(0.8))
#' lines(mu_est ~ petal_length_plot, lwd = 2)
#' }
#'
#'
calculate <- function (target, values) {

  target_name <- deparse(substitute(target))

  if (!inherits(target, "greta_array"))
    stop ("greta_array is not a greta array")

  if (inherits(values, "mcmc.list"))
    calculate_mcmc.list(target, target_name, values)
  else
    calculate_list(target, values)

}

calculate_mcmc.list <- function (target, target_name, values) {

  model_info <- attr(values, "model_info")

  if (is.null(model_info)) {
    stop ("value is an mcmc.list object, but is not associated with any ",
          "model information, perhaps it wasn't created with ",
          "greta::model() or greta::raw() ?",
          call. = FALSE)
  }

  # copy and refresh the dag
  dag <- model_info$model$dag$clone()
  dag$tf_environment <- new.env()
  dag$tf_graph <- tf$Graph()

  # extend the dag to include this node, as the target
  dag$build_dag(list(target))
  self <- dag  # mock for scoping
  dag$define_tf(log_density = TRUE, gradients = TRUE)

  dag$target_nodes <- list(target$node)
  names(dag$target_nodes) <- target_name

  example_values <- dag$trace_values()
  n_trace <- length(example_values)

  # raw draws are either an attribute, or this object
  model_info <- attr(values, "model_info")
  draws <- model_info$raw_draws

  # trace the target for each draw in each chain
  trace <- list()
  for (i in seq_along(draws)) {

    samples <- apply(draws[[i]],
                     1,
                     function (x) {
                       dag$send_parameters(x)
                       dag$trace_values()
                     })

    samples <- as.matrix(samples)

    if (ncol(samples) != n_trace)
      samples <- t(samples)

    colnames(samples) <- names(example_values)

    trace[[i]] <- coda::mcmc(samples)

  }

  trace <- coda::mcmc.list(trace)
  attr(trace, "model_info") <- model_info
  # return this
  return (trace)

}

calculate_list <- function(target, values) {

  # get the values and their names
  names <- names(values)
  stopifnot(length(names) == length(values))

  # get the corresponding greta arrays
  fixed_greta_arrays <- lapply(names,
                               get,
                               envir = parent.frame(n = 2))

  # make sure that's what they are
  are_greta_arrays <- vapply(fixed_greta_arrays,
                             inherits,
                             "greta_array",
                             FUN.VALUE = FALSE)

  stopifnot(all(are_greta_arrays))

  # make sure the values have the correct dimensions
  values <- mapply(assign_dim,
                   values,
                   fixed_greta_arrays,
                   SIMPLIFY = FALSE)

  all_greta_arrays <- c(fixed_greta_arrays, list(target))

  # define the dag and TF graph
  dag <- dag_class$new(all_greta_arrays)
  dag$define_tf(log_density = FALSE, gradients = FALSE)

  # build and send a dict for the fixed values
  fixed_nodes <- lapply(fixed_greta_arrays,
                        member,
                        'node')

  names(values) <- vapply(fixed_nodes,
                          dag$tf_name,
                          FUN.VALUE = "")

  # check that all of the variables are set
  # list of variable tf names
  variable_nodes <- dag$node_list[dag$node_types == "variable"]
  variable_names <- vapply(variable_nodes,
                           dag$tf_name,
                           FUN.VALUE = "")

  if (!all(variable_names %in% names(values))) {
    stop ("values have not been provided for all variables",
          call. = FALSE)
  }

  # send it to tf
  assign("eval_list", values, envir = dag$tf_environment)
  ex <- expression(with_dict <- do.call(dict, eval_list))
  eval(ex, envir = dag$tf_environment)

  # evaluate the target there
  ex <- sprintf("sess$run(%s, feed_dict = with_dict)",
                dag$tf_name(target$node))
  result <- eval(parse(text = ex),
                 envir = dag$tf_environment)

  result

}

# coerce value to have the correct dimensions
assign_dim <- function (value, greta_array) {
  array <- strip_unknown_class(greta_array$node$value())
  if (length(array) != length(value)) {
    stop ("a provided value has different number of elements",
          " than the greta array", call. = FALSE)
  }
  array[] <- value
  array
}
