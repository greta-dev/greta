#' @title Simulate Responses From \code{greta_model} Object
#'
#' @description Simulate values of all named greta arrays associated with a
#'   greta model from the model priors, including the response variable.
#'
#' @param object a \code{\link{greta_model}} object
#' @param nsim positive integer scalar - the number of responses to simulate
#' @param precision the floating point precision to use when calculating values.
#' @param seed an optional seed to be used in set.seed immediately before the
#'   simulation so as to generate a reproducible sample
#' @param ... optional additional arguments, none are used at present
#'
#' @details This is essentially a wrapper around \code{\link{calculate()}} that
#'   inds all relevant greta arrays. See that function for more functionality,
#'   including simulation conditional on fixed values or posterior samples.
#'
#'   To simulate values of the response variable, it must be both a named object
#'   (in the calling environment) and be a greta array. If you don't see it
#'   showing up in the output, you may need to use \code{as_data} to convert it
#'   to a greta array before defining the model.
#'
#' @return A named list of vectors, matrices or arrays containing independent
#'   samples of the greta arrays associated with the model. The number of
#'   samples will be prepended as the first dimension of the greta array, so
#'   that a vector of samples is returned for each scalar greta array, and a
#'   matrix is returned for each vector greta array, etc.
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
#' # simulate one random draw of y, mu and sd from the model prior:
#' sims <- simulate(m)
#'
#' # 100 simulations of y, mu and sd
#' sims <- simulate(m, nsim = 100)
#'
#' }
simulate.greta_model <- function (
  object,
  nsim = 1,
  precision = c("double", "single"),
  seed = NULL,
  ...
) {

  # find all the greta arrays in the calling environment
  target_greta_arrays <- all_greta_arrays(parent.frame())

  # subset these to only those that are associated with the model
  target_nodes <- lapply(target_greta_arrays, get_node)
  target_node_names <- vapply(target_nodes,
                              member,
                              "unique_name",
                              FUN.VALUE = character(1))
  object_node_names <- vapply(object$dag$node_list,
                              member,
                              "unique_name",
                              FUN.VALUE = character(1))
  keep <- target_node_names %in% object_node_names
  target_greta_arrays <- target_greta_arrays[keep]

  calculate(
    target = target_greta_arrays,
    precision = precision,
    nsim = nsim,
    seed = seed
  )

#   # check the model can be sampled
#
#   # error if there are variables withut distributions that aren't named in values
#
#   # error if there are distributions without sampling methods
#
#   # fetch the nodes for the target greta arrays
#   if (!identical(targets, list())) {
#     # get_target_greta_arrays does validity checks
#     target_greta_arrays <- check_greta_arrays(targets, "simulate")
#     target_nodes <- lapply(target_greta_arrays, get_node)
#   } else {
#     target_nodes <- object$dag$target_nodes
#   }
#
#   # switch the dag to sampling mode for the duration of this function (on.exit
#   # reverts even if this function errors)
#   dag <- object$dag
#   old_mode <- dag$mode
#   on.exit(dag$mode <- old_mode, add = TRUE)
#   dag$mode <- "sampling"
#
#   # flush and redefine the tf sampling graph, so that the RNG seed can change between runs
#   sampling_tensors <- grep("^sampling_", ls(dag$tf_environment), value = TRUE)
#   sampling_tensors <- sampling_tensors[sampling_tensors != "sampling_data_list"]
#   do.call(rm, c(as.list(sampling_tensors), list(envir = dag$tf_environment)))
#   dag$define_tf(target_nodes = target_nodes)
#
#   # look up the tf names of the target greta arrays (under sampling)
#   # create an object in the environment that's a list of these, and sample that
#   target_names_list <- lapply(target_nodes, dag$tf_name)
#   target_tensor_list <- lapply(target_names_list, get, envir = dag$tf_environment)
#   assign("sampling_target_tensor_list", target_tensor_list, envir = dag$tf_environment)
#
#   if (inherits(values, "mcmc.list")) {
#
#     result_list <- simulate_mcmc.list(
#       dag = dag,
#       nsim = nsim,
#       values = values
#     )
#
#   } else {
#
#     result_list <- simulate_list(
#       dag = dag,
#       nsim = nsim,
#       values = values,
#       env = parent.frame()
#     )
#
#   }
#
#   # tidy up the results and return
#   result_list <- lapply(result_list, drop)
#   result_list

}


simulate_mcmc.list <- function(dag, nsim, values) {
  stop ("not implemented yet")
}

simulate_list <- function(dag, nsim, values, env) {

  # check and parse the values if needed
  if (!identical(values, list())) {

    # check the list of values makes sense, and return these and the corresponding
    # greta arrays (looked up by name in environment env)
    values_list <- check_values_list(values, env)
    fixed_greta_arrays <- values_list$fixed_greta_arrays
    values <- values_list$values

    # convert to nodes, and add tensor names to values
    fixed_nodes <- lapply(fixed_greta_arrays, get_node)
    names(values) <- vapply(fixed_nodes, dag$tf_name, FUN.VALUE = character(1))

  }

  # add values or data not specified by the user
  data_list <- dag$get_tf_data_list()
  missing <- !names(data_list) %in% names(values)

  # send list to tf environment and roll into a dict
  values <- lapply(values, add_first_dim)
  values <- c(values, list(batch_size = as.integer(nsim)))
  dag$build_feed_dict(values, data_list = data_list[missing])

  # run the sampling
  dag$tf_sess_run("sampling_target_tensor_list", as_text = TRUE)

}
