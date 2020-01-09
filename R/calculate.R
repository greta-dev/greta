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
#' @param trace_batch_size the number of posterior samples to process at a time when
#'   \code{target} is an \code{mcmc.list} object; reduce this to reduce memory
#'   demands
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
#'
#' # trace_batch_size can be changed to trade off speed against memory usage
#' # when calculating. These all produce the same result, but have increasing
#' # memory requirements:
#' mu_plot_draws_1 <- calculate(mu_plot, draws, trace_batch_size = 1)
#' mu_plot_draws_10 <- calculate(mu_plot, draws, trace_batch_size = 10)
#' mu_plot_draws_inf <- calculate(mu_plot, draws, trace_batch_size = Inf)
#' }
#'
#'
calculate <- function(target, values = list(),
                      precision = c("double", "single"),
                      trace_batch_size = 100) {

  target_name <- deparse(substitute(target))
  tf_float <- switch(match.arg(precision),
                     double = "float64",
                     single = "float32")

  if (!inherits(target, "greta_array"))
    stop("'target' is not a greta array")

  if (inherits(values, "mcmc.list")) {
    calculate_mcmc.list(
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
                   tf_float = tf_float)
  }

}

# Begin Exclude Linting
calculate_mcmc.list <- function(target, target_name, values, tf_float, trace_batch_size) {
# End Exclude Linting

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

  param <- dag$example_parameters()
  param[] <- 0

  # raw draws are either an attribute, or this object
  model_info <- attr(values, "model_info")
  draws <- model_info$raw_draws

  # trace the target for each chain
  values <- lapply(draws, dag$trace_values, trace_batch_size = trace_batch_size)
  trace <- lapply(values, coda::mcmc)

  trace <- coda::mcmc.list(trace)
  attr(trace, "model_info") <- model_info
  trace

}

calculate_list <- function(target, values, tf_float) {

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

  # find all the nodes depended on by this one
  dependencies <- get_node(target)$parent_names(recursive = TRUE)

  # find all the nodes depended on by the new values, and remove them from the
  # list
  complete_dependencies <- lapply(fixed_greta_arrays,
                                  function(x)
                                    get_node(x)$parent_names(recursive = TRUE))
  complete_dependencies <- unique(unlist(complete_dependencies))

  unmet_dependencies <- dependencies[!dependencies %in% complete_dependencies]

  # find all of the remaining nodes that are variables
  unmet_nodes <- dag$node_list[unmet_dependencies]
  is_variable <- vapply(unmet_nodes, node_type, FUN.VALUE = "") == "variable"

  # if there are any undefined variables
  if (any(is_variable)) {

    # try to find the associated greta arrays to provide a more informative
    # error message
    greta_arrays <- all_greta_arrays(parent.frame(2),
                                     include_data = FALSE)

    greta_array_node_names <- vapply(greta_arrays,
                                function(x) get_node(x)$unique_name,
                                FUN.VALUE = "")

    unmet_variables <- unmet_nodes[is_variable]

    matches <- names(unmet_variables) %in% greta_array_node_names


    unmet_names_idx <- greta_array_node_names %in% names(unmet_variables)
    unmet_names <- names(greta_array_node_names)[unmet_names_idx]

    # build the message
    msg <- paste("values have not been provided for all greta arrays on which",
                 "the target depends.")

    if (any(matches)) {
      names_text <- paste(unmet_names, collapse = ", ")
      msg <- paste(msg,
                   sprintf("Please provide values for the greta array%s: %s",
                           ifelse(length(matches) > 1, "s", ""),
                           names_text))
    } else {
      msg <- paste(msg,
                   "\nThe names of the missing greta arrays",
                   "could not be detected")
    }

    stop(msg,
         call. = FALSE)
  }

  # add values or data not specified by the user
  data_list <- tfe$data_list
  missing <- !names(data_list) %in% names(values)

  # send list to tf environment and roll into a dict
  values <- lapply(values, add_first_dim)
  dag$build_feed_dict(values, data_list = data_list[missing])

  name <- dag$tf_name(get_node(target))
  result <- dag$tf_sess_run(name, as_text = TRUE)

  drop_first_dim(result)

}

# coerce value to have the correct dimensions
assign_dim <- function(value, greta_array) {
  array <- strip_unknown_class(get_node(greta_array)$value())
  if (length(array) != length(value)) {
    stop("a provided value has different number of elements",
         " than the greta array", call. = FALSE)
  }
  array[] <- value
  array
}
