#' @name data_values
#' @title Replace a model's data without rebuilding it
#'
#' @description Replace the values of a data greta array in a fitted or
#'   unfitted model, without reconstructing the model's TensorFlow graph. This
#'   makes it cheap to run the same model over many datasets, or to change the
#'   data between bursts of sampling.
#'
#' @param model a [greta model][model()]
#' @param target a data greta array created with [as_data_mutable()],
#'   belonging to `model`
#' @param value the replacement data. Must have the same dimensions as
#'   `target`, and must be something [as_data()] would accept: numeric,
#'   logical, or a data frame of those, with no missing or infinite values.
#'
#' @details Only greta arrays created with [as_data_mutable()] can be replaced.
#'   Everything else in a model - including data from [as_data()], and the
#'   numbers in prior specifications like `normal(0, 10)` - is held as a
#'   TensorFlow constant so it can be folded into the graph.
#'
#'   The dimensions cannot change. Changing them would mean rebuilding the
#'   graph, which is the thing this avoids.
#'
#'   The replacement applies to `model`. If the same greta array is also used
#'   by another model, that model keeps the previous values until it is next
#'   rebuilt.
#'
#'   **Draws taken earlier still point at `model`.** The draws themselves are
#'   plain numbers and keep their values, but [extra_samples()] resumes
#'   sampling against whatever data the model holds now. Resuming a chain
#'   against a different dataset does not error - it typically freezes, because
#'   the step size was tuned for the posterior the chain came from. Take what
#'   you need from each fit before replacing the data.
#'
#' @return `model`, invisibly. greta models are modified in place, so a loop
#'   over datasets can be plain R: assign, sample, repeat.
#'
#' @seealso `vignette("mutable_data", "greta")`, "Fitting one model to many
#'   datasets", which works this through end to end. [as_data_mutable()] to
#'   declare the data in the first place.
#'
#' @examples
#' \dontrun{
#' x <- as_data(attitude$complaints)
#' y <- as_data_mutable(attitude$rating)
#'
#' # ratings run from 40 to 85, so the intercept needs room to get there
#' int <- normal(0, 50)
#' coef <- normal(0, 10)
#' sd <- cauchy(0, 3, truncation = c(0, Inf))
#' distribution(y) <- normal(int + coef * x, sd)
#'
#' m <- model(int, coef, sd)
#' draws_first <- mcmc(m, n_samples = 100)
#'
#' # same model, different data, no rebuild. reversing the response breaks the
#' # relationship, so `coef` should collapse towards zero
#' data_values(m, y) <- rev(attitude$rating)
#' draws_second <- mcmc(m, n_samples = 100)
#' }
#' @export
`data_values<-` <- function(model, target, value) {
  check_if_greta_model(model)
  model$dag$set_data_value(
    target,
    value,
    arg = rlang::caller_arg(target),
    call = rlang::current_env()
  )
  invisible(model)
}

# the data nodes `data_values<-` is allowed to change. Everything else a model
# holds is a tf$constant, folded into the trace and not addressable afterwards
mutable_data_nodes <- function(dag) {
  data_nodes <- dag$node_list[dag$node_types == "data"]
  is_mutable <- vapply(data_nodes, \(node) node$mutable, logical(1))
  data_nodes[is_mutable]
}

new_data_variables <- function(values) {
  lapply(values, new_data_variable)
}

new_data_variable <- function(value) {
  tf$Variable(
    initial_value = value,
    dtype = tf_float(),
    # data is not a parameter: were it trainable, opt() would optimise the data
    # alongside the free state
    trainable = FALSE
  )
}

# refresh rather than replace, so that setting a node's value and rebuilding
# still changes what the graph computes. A new variable would leave the traced
# graph reading the old one, so the rebuild would silently have no effect - the
# two ways of changing data have to agree
refresh_data_variables <- function(variables, values) {
  to_refresh <- variables[names(values)]
  mapply(refresh_data_variable, to_refresh, values, SIMPLIFY = FALSE)
  invisible(NULL)
}

refresh_data_variable <- function(variable, value) {
  variable$assign(value)
}
