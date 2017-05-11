# syntax definitions

#' @name greta-distribution
#' @aliases distribution likelihood
#' @title Define a Distribution Over a greta Array
#' @description \code{distribution} is used to link observed data, free
#'   parameters and other greta arrays with probability distributions. For
#'   example, \code{distribution} can be used to define the likelhood term for a
#'   model by using \code{distribution} on some observed data.
#'   \code{likelihood} is an alias for \code{distribution}. It is deprecated and
#'   will be removed in version 0.2.
#'
#' @param greta_array a greta array. For the assignment method it must be a
#'   greta array that doesn't already have a probability distribution.
#'
#' @param value a \emph{distribution} greta array (see
#'   \code{\link{greta-distributions}})
#'
#' @details The extract method returns a distribution greta array if one was
#'   assigned to \code{greta_array}, or if \code{greta_array} already was a
#'   distribution greta array. If \code{greta_array} has no distribution, it
#'   returns NULL.
#'
#' Distribution can also be used to create truncated distributions, by first
#' defining a greta array with constraints (the truncation) and then defining
#' the distribution on that greta array. See example for an example.
#'
#' @export
#' @examples
#'
#' # define a model likelihood
#'
#' # observed data and mean parameter to be estimated
#' # (explicitly coerce data to a greta array so we can refer to it later)
#' y = as_data(rnorm(5, 0, 3))
#' mu = free()
#' # define the distribution over y (the model likelihood)
#' distribution(y) = normal(mu, 1)
#'
#' # get the distribution over y
#' distribution(y)
#'
#' # define a truncated-positive standard normal random variable
#' tn = free(lower = 0)
#' distribution(tn) = normal(0, 1)
#'
`distribution<-` <- function (greta_array, value) {

  # stash the old greta array to return
  greta_array_tmp <- greta_array

  # coerce to a greta array (converts numerics to data arrays)
  greta_array <- as.greta_array(greta_array)

  if (!is.greta_array(value)) {
    stop ('right hand side must be a greta array',
          call. = FALSE)
  }

  # grab the distribution
  distribution_node <- value$node$distribution

  # only for greta arrays
  if (!is.greta_array(greta_array)) {
    stop ('left hand side must be a greta array or something that ',
          'can be passed to as_data',
          call. = FALSE)
  }

  if (inherits(greta_array$node, 'distribution_node')) {
    stop ('left hand side of distribution is already a distribution greta array',
          call. = FALSE)
  }

  if (!is.null(greta_array$node$distribution)) {
    stop ('greta_array already has a distribution assigned',
          call. = FALSE)
  }

  if (!inherits(distribution_node, 'distribution_node')) {

    stop ('right hand side must have a distribution',
          call. = FALSE)

  }

  # if distribution isn't scalar, make sure it has the right dimensions
  if (!is_scalar(value)) {
    if (!identical(dim(greta_array), dim(value))) {
      stop ('left- and right-hand side of distribution have different ',
            'dimensions. The distribution must have dimension of either ',
            paste(dim(greta_array), collapse = ' x '),
            ' or 1 x 1, but instead has dimension ',
            paste(dim(value), collapse = ' x '),
            call. = FALSE)
    }
  }

  # provide the data to the distribution and lock in the values in the
  # distribution

  # if the distribution already has a fixed value, error
  if (distribution_node$.fixed_value) {
    stop ('right hand side of distribution has already been assigned fixed values',
          call. = FALSE)
  }

  # assign the new node as the distribution's target
  # also adds distribution_node as this node's distribution
  distribution_node$replace_target(greta_array$node)

  # optionally set that as a fixed value
  if (inherits(greta_array$node, 'data_node'))
    distribution_node$.fixed_value <- TRUE

  # if the greta_array was a variable, add its constraints as truncation
  if (inherits(greta_array$node, 'variable_node')) {

    # check the distribution can handle truncation
    if (is.null(distribution_node$tf_cdf_function)) {

      stop('distribution cannot be truncated to the constraints of a free greta array',
           call. = FALSE)

    }

    distribution_node$truncation <- c(greta_array$node$lower, greta_array$node$upper)

  }

  # return greta_array (pre-conversion to a greta array)
  greta_array_tmp

}

#' @rdname greta-distribution
#' @export
distribution <- function (greta_array) {

  # only for greta arrays
  if (!is.greta_array(greta_array)) {
    stop ('not a greta array',
          call. = FALSE)
  }

  # if greta_array has a distribution, return this greta array
  if (inherits(greta_array$node$distribution, 'distribution_node')) {

    distrib <- greta_array

  } else {

    # otherwise return NULL
    distrib <- NULL

  }

  distrib

}

#' @rdname greta-distribution
#' @export
`likelihood<-` <- `distribution<-`
