# evaluating values outside inference

#' @name evaluate
#' @title evaluate greta arrays given fixed values
#' @description Evaluate greta arrays as numeric R arrays, given temporary
#'   values for the greta arrays on which they depend. This can be used to check
#'   the behaviour of your model or make predictions to new data after model
#'   fitting.
#'
#' @param target a greta array for which to evaluate the value
#' @param values a named list giving temporary values of the greta arrays with
#'   which \code{target} is connected.
#'
#' @return A numeric R array with the same dimensions as \code{target}, giving
#'   the values it would take conditioned on the fixed values given by
#'   \code{values}.
#'
#' @details The greta arrays named in \code{values} need not be variables, they
#'   can also be other operations or even data.
#'
#'   At present, \code{values} must contain values for \emph{all} of the
#'   variable greta arrays with which \code{target} is connected, even values are
#'   given for intermediate operations, or the target doesn't depend on the
#'   variable. That may be relaxed in a future release.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # define a variable greta array, and another that is calculated from it
#' # then evaluate what value y would take for different values of x
#' x = normal(0, 1, dim = 3)
#' a = lognormal(0, 1)
#' y <- sum(x ^ 2) + a
#' evaluate(y, list(x = c(0.1, 0.2, 0.3), a = 2))
#'
#' # define a simple model
#' alpha = normal(0, 1)
#' beta = normal(0, 1)
#' sigma = lognormal(1, 0.1)
#' mu <- alpha + iris$Petal.Length * beta
#' distribution(iris$Petal.Width) = normal(mu, sigma)
#' m <- model(alpha, beta, sigma)
#'
#' # work out what intermediate values would be, given some parameter values
#' evaluate(mu[1:5], list(alpha = 1, beta = 2))
#' evaluate(mu[1:5], list(alpha = -1, beta = 0.2))
#' }
evaluate <- function (target, values) {

  if (!inherits(target, "greta_array"))
    stop ("greta_array is not a greta array")

  # get the values and their names
  names <- names(values)
  stopifnot(length(names) == length(values))

  # get the corresponding greta arrays
  fixed_greta_arrays <- lapply(names,
                               get,
                               envir = parent.frame())

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

