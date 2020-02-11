#' @name distribution
#' @aliases distribution
#' @title define a distribution over data
#'
#' @description \code{distribution} defines probability distributions over
#'   observed data, e.g. to set a model likelihood.
#'
#' @param greta_array a data greta array. For the assignment method it must not
#'   already have a probability distribution assigned
#'
#' @param value a greta array with a distribution (see
#'   \code{\link{distributions}})
#'
#' @details The extract method returns the greta array if it has a distribution,
#'   or \code{NULL} if it doesn't. It has no real use-case, but is included for
#'   completeness
#'
#' @export
#' @examples
#' \dontrun{
#'
#' # define a model likelihood
#'
#' # observed data and mean parameter to be estimated
#' # (explicitly coerce data to a greta array so we can refer to it later)
#' y <- as_data(rnorm(5, 0, 3))
#'
#' mu <- uniform(-3, 3)
#'
#' # define the distribution over y (the model likelihood)
#' distribution(y) <- normal(mu, 1)
#'
#' # get the distribution over y
#' distribution(y)
#' }
`distribution<-` <- function(greta_array, value) {  # Exclude Linting

  # stash the old greta array to return
  greta_array_tmp <- greta_array

  # coerce to a greta array (converts numerics to data arrays)
  greta_array <- as.greta_array(greta_array)

  node <- get_node(greta_array)

  # only for greta arrays without distributions
  if (has_distribution(node)) {
    stop("left hand side already has a distribution assigned",
         call. = FALSE)
  }

  # only for data greta arrays
  if (node_type(node) != "data") {
    stop("distributions can only be assigned to data greta arrays",
         call. = FALSE)
  }

  # can only assign with greta arrays ...
  if (!inherits(value, "greta_array")) {
    stop("right hand side must be a greta array",
         call. = FALSE)
  }

  # ... that have distributions
  value_node <- get_node(value)
  distribution_node <- value_node$distribution

  if (!inherits(distribution_node, "distribution_node")) {
    stop("right hand side must have a distribution",
         call. = FALSE)
  }

  # that aren't already fixed
  if (inherits(distribution_node$target, "data_node")) {
    stop("right hand side has already been assigned fixed values",
         call. = FALSE)
  }

  # if distribution isn't scalar, make sure it has the right dimensions
  if (!is_scalar(value)) {
    if (!identical(dim(greta_array), dim(value))) {
      stop("left and right hand sides have different dimensions. ",
           "The distribution must have dimension of either ",
           paste(dim(greta_array), collapse = " x "),
           " or 1 x 1, but instead has dimension ",
           paste(dim(value), collapse = " x "),
           call. = FALSE)
    }
  }

  # assign the new node as the distribution's target
  # also adds distribution_node as this node's distribution
  distribution_node$remove_target()
  distribution_node$add_target(node)

  # if possible, expand the dimensions of the distribution's parameters to match
  # the target
  distribution_node$expand_parameters_to(node$dim)

  # remove the distribution from the RHS variable greta array
  value_node$distribution <- NULL

  # return greta_array (pre-conversion to a greta array)
  greta_array_tmp

}

#' @rdname distribution
#' @export
distribution <- function(greta_array) {

  # only for greta arrays
  if (!inherits(greta_array, "greta_array")) {
    stop("not a greta array",
         call. = FALSE)
  }

  # if greta_array has a distribution, return this greta array
  if (inherits(get_node(greta_array)$distribution, "distribution_node")) {

    distrib <- greta_array

  } else {

    # otherwise return NULL
    distrib <- NULL

  }

  distrib

}
