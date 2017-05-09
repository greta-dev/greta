# syntax definitions

#' @name greta-distribution
#' @aliases distribution likelihood
#' @title Define a Distribution Over a greta Array
#' @description The distribution function is used to link observed data, free
#'   parameters and other greta arrays with probability distributions. For
#'   example, \code{distribution} can be used to define the likelhood term for a
#'   model by using \code{distribution} on some observed data. \code{likelihood}
#'   is an alias for \code{distribution}. It is deprecated and will be removed
#'   in version 0.2.
#' @param greta_array any greta array that doesn't already have a probability
#'   distribution.
#' @param value a greta array with a probability distribution (see
#'   \code{\link{greta-distributions}})
#'
#' @export
#' @examples
#' # observed data
#' y_ = rnorm(5, 0, 3)
#'
#' # explicitly coerce to a data greta array (so we can refer to it later)
#' y <- as_data(y_)
#' y
#'
#' # mean and variance parameters (with no priors)
#' mu = free()
#' sigma = exp(free())
#'
#' # define the distribution over y (the model likelihood)
#' distribution(y) = normal(mu, sigma)
#'
#' # get the distribution over y
#' distribution(y)
#'
`distribution<-` <- function (greta_array, value) {

  greta_array_tmp <- greta_array
  greta_array <- as_data(greta_array)
  distribution <- value

  if (!(is.greta_array(distribution) &&
        inherits(distribution$node, 'distribution_node'))) {

    stop ('right hand side of distribution must be a distribution greta array',
          call. = FALSE)

  }

  # if theta isn't scalar, make sure it has the right dimensions
  if (!is_scalar(distribution)) {
    if (!identical(dim(greta_array), dim(distribution))) {
      stop ('left- and right-hand side of distribution have different ',
            'dimensions. The distribution must have dimension of either ',
            paste(dim(greta_array), collapse = ' x '),
            ' or 1 x 1, but instead has dimension ',
            paste(dim(distribution), collapse = ' x '),
            call. = FALSE)
    }
  }

  # provide the data to the distribution and lock in the values in the
  # distribution
  distribution$node$value(greta_array$node$value())
  distribution$node$.fixed_value <- TRUE

  # give the distribution to the data (this will register the child
  # distribution)
  greta_array$node$set_distribution(distribution$node)

  # register the data node, with it's own name
  greta_array$node$register()

  greta_array_tmp

}

#' @rdname greta-distribution
#' @export
`likelihood<-` <- `distribution<-`

#' @rdname greta-distribution
#' @export
distribution <- function (greta_array) {

  # only for greta arrays
  if (!is.greta_array(greta_array)) {
    stop ('not a greta array',
          call. = FALSE)
  }

  # if greta_array *is* a distribution, return itself
  if (inherits(greta_array, 'distribution_node')) {

    distrib <- greta_array

  } else {

    # otherwise, get the array's assigned distribution node
    distrib <- greta_array$node$distribution

    # coerce to a greta array if it isn't missing
    if (!is.null(distrib))
      distrib <- ga(distrib)

  }

  distrib

}
