#' @title Simulate Responses From `greta_model` Object
#'
#' @description Simulate values of all named greta arrays associated with a
#'   greta model from the model priors, including the response variable.
#'
#' @param object a [`greta_model()`][greta::model] object
#' @param nsim positive integer scalar - the number of responses to simulate
#' @param seed an optional seed to be used in set.seed immediately before the
#'   simulation so as to generate a reproducible sample
#' @param precision the floating point precision to use when calculating values.
#' @param ... optional additional arguments, none are used at present
#'
#' @details This is essentially a wrapper around [calculate()] that
#'   finds all relevant greta arrays. See that function for more functionality,
#'   including simulation conditional on fixed values or posterior samples.
#'
#'   To simulate values of the response variable, it must be both a named object
#'   (in the calling environment) and be a greta array. If you don't see it
#'   showing up in the output, you may need to use `as_data` to convert it
#'   to a greta array before defining the model.
#'
#' @return A named list of vectors, matrices or arrays containing independent
#'   samples of the greta arrays associated with the model. The number of
#'   samples will be prepended as the first dimension of the greta array, so
#'   that a vector of samples is returned for each scalar greta array, and a
#'   matrix is returned for each vector greta array, etc.
#'
#' @importFrom stats simulate
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
#' }
#' # nolint start
simulate.greta_model <- function(object,
                                 nsim = 1,
                                 seed = NULL,
                                 precision = c("double", "single"),
                                 ...) {
  # nolint end
  # find all the greta arrays in the calling environment
  target_greta_arrays <- all_greta_arrays(parent.frame())

  # subset these to only those that are associated with the model
  target_nodes <- lapply(target_greta_arrays, get_node)
  target_node_names <- extract_unique_names(target_nodes)
  object_node_names <- extract_unique_names(object$dag$node_list)
  keep <- target_node_names %in% object_node_names
  target_greta_arrays <- target_greta_arrays[keep]

  other_args <- list(
    precision = precision,
    nsim = nsim,
    seed = seed
  )

  do.call(calculate, c(target_greta_arrays, other_args))
}
