#' @name as_data_mutable
#' @title Declare data you intend to replace
#'
#' @description Like [as_data()], but marks the data so its values can be
#'   replaced later with [`data_values<-`] without rebuilding the model. Use it
#'   when you will fit the same model to more than one dataset, or change the
#'   data between bursts of sampling.
#'
#' @param x an R object to use as data in a greta model
#'
#' @details greta holds data as a TensorFlow constant, which lets TensorFlow
#'   fold the value into the model's graph. That is faster, but it means
#'   changing the data requires building the graph again, which is expensive.
#'
#'   `as_data_mutable()` instead holds the data in a TensorFlow variable, which
#'   the graph reads by reference. Replacing its value then changes what the
#'   model computes without rebuilding anything.
#'
#'   A variable is not free. TensorFlow folds a constant into the graph once,
#'   where a variable has to be read, and copied out to one batch per chain, on
#'   every evaluation. The gap is negligible for small arrays or a single
#'   chain, and grows with both. That is why this is opt-in: declare only the
#'   data you actually intend to replace, and everything else - including the
#'   numbers in prior specifications like `normal(0, 10)` - stays constant.
#'
#'   The **dimensions cannot change**. Replacing 100 observations with 200 would
#'   mean a different graph, which is the thing this avoids. Fit a new model
#'   instead.
#'
#' @return A `greta_array`, as [as_data()] returns, whose values can be
#'   replaced with [`data_values<-`].
#'
#' @seealso `vignette("mutable_data", "greta")`, "Fitting one model to many
#'   datasets", which works this through end to end. [as_data()] for data that
#'   will not change, and [`data_values<-`] to replace it.
#'
#' @examples
#' \dontrun{
#' n <- 30
#'
#' # the predictor is the same for every dataset, so it stays constant
#' x <- as_data(seq(0, 1, length.out = n))
#'
#' # the response is what changes, so declare it mutable
#' y <- as_data_mutable(rnorm(n))
#'
#' int <- normal(0, 10)
#' coef <- normal(0, 10)
#' sd <- cauchy(0, 3, truncation = c(0, Inf))
#' distribution(y) <- normal(int + coef * x, sd)
#'
#' m <- model(int, coef, sd)
#'
#' # the loop is plain R because there is nothing clever left to do: the graph
#' # was built once above, and `data_values<-` modifies `m` in place, so each
#' # pass samples from a different posterior without rebuilding anything
#' simulations <- lapply(1:3, function(i) rnorm(n, mean = 2 * i))
#'
#' # take what you need from each fit inside the loop, because `m` ends up
#' # holding the last dataset - see `?\`data_values<-\``
#' intercepts <- vapply(
#'   simulations,
#'   function(d) {
#'     data_values(m, y) <- d
#'     draws <- mcmc(m, n_samples = 100)
#'     mean(as.matrix(draws)[, "int"])
#'   },
#'   numeric(1)
#' )
#' }
#' @export
as_data_mutable <- function(x) {
  x <- as_data(x)
  node <- get_node(x)
  node$mutable <- TRUE
  x
}
