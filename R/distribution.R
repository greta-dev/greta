#' @name distribution
#' @aliases distribution
#' @title define a distribution over data
#'
#' @description `distribution` defines probability distributions over
#'   observed data, e.g. to set a model likelihood.
#'
#' @param greta_array a data greta array. For the assignment method it must not
#'   already have a probability distribution assigned
#'
#' @param value a greta array with a distribution (see
#'   [distributions()])
#'
#' @details The extract method returns the greta array if it has a distribution,
#'   or `NULL` if it doesn't. It has no real use-case, but is included for
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
`distribution<-` <- function(greta_array, value) { # nolint

  # stash the old greta array to return
  greta_array_tmp <- greta_array

  # coerce to a greta array (converts numerics to data arrays)
  greta_array <- as.greta_array(greta_array)

  node <- get_node(greta_array)

  # TODO revisit checking functions here
  # only for greta arrays without distributions
  ## TODO provide more detail on the distribution already assigned
  ## This might come up when the user accidentally runs assignment
  ## of a distribution twice. Is there a way to avoid this, or perhaps
  ## remove a distribution in case the user wants to do reassign it?
  ## or perhaps we can recommend something?
  if (has_distribution(node)) {
    cli::cli_abort(
      "left hand side already has a distribution assigned"
    )
  }

  # only for data greta arrays
  if (node_type(node) != "data") {
    cli::cli_abort(
      "distributions can only be assigned to data {.cls greta array}s"
    )
  }

  # can only assign with greta arrays ...
  if (!is.greta_array(value)) {
    cli::cli_abort(
      "right hand side must be a {.cls greta_array}"
    )
  }

  # ... that have distributions
  value_node <- get_node(value)
  distribution_node <- value_node$distribution

  if (!is.distribution_node(distribution_node)) {
    cli::cli_abort(
      "right hand side must have a distribution"
    )
  }

  # that aren't already fixed
  if (is.data_node(distribution_node$target)) {
    cli::cli_abort(
      "right hand side has already been assigned fixed values"
    )
  }

  # if distribution isn't scalar, make sure it has the right dimensions
  ## TODO fix explaining variable
  if (!is_scalar(value)) {
    if (!identical(dim(greta_array), dim(value))) {
      cli::cli_abort(
        c(
          "left and right hand sides have different dimensions. ",
          "The distribution must have dimension of either \\
          {.val {paste(dim(greta_array), collapse = 'x')}} or {.val 1x1},\\
          but instead has dimension \\
          {.val {paste(dim(value), collapse = 'x')}}"
        )
      )
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
  check_is_greta_array(greta_array)

  # if greta_array has a distribution, return this greta array
  if (is.distribution_node(get_node(greta_array)$distribution)) {
    distrib <- greta_array
  } else {

    # otherwise return NULL
    distrib <- NULL
  }

  distrib
}
