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
#' y = rnorm(100, 0, 3)
#'
#' # mean and variance parameters (with no priors)
#' mu = free()
#' sigma = exp(free())
#'
#' # define the likelihood
#' distribution(y) = normal(mu, sigma)
#'
`distribution<-` <- function (greta_array, value) {

  greta_array_tmp <- greta_array
  data <- as_data(greta_array)
  distribution <- value

  if (!(is.greta_array(distribution) &&
        inherits(distribution$node, 'distribution'))) {

    stop ('right hand side of distribution must be a stochastic greta array',
          call. = FALSE)

  }

  if (distribution$node$distribution_name == 'free') {

    stop ('free parameters do not have distributions, ',
          'so cannot be used to define a distribution',
          call. = FALSE)

  }

  # if theta isn't scalar, make sure it has the right dimensions
  if (!is_scalar(distribution)) {
    if (!identical(dim(data), dim(distribution))) {
      stop ('left- and right-hand side of distribution have different ',
            'dimensions. The distribution must have dimension of either ',
            paste(dim(data), collapse = ' x '),
            ' or 1 x 1, but instead has dimension ',

            paste(dim(distribution), collapse = ' x '),
            call. = FALSE)
    }
  }

  # provide the data to the distribution and lock in the values in the
  # distribution
  distribution$node$value(data$node$value())
  distribution$node$.fixed_value <- TRUE

  # give the distribution to the data (this will register the child
  # distribution)
  data$node$set_distribution(distribution$node)

  # register the data node, with it's own name
  data$node$register()

  greta_array

}

#' @rdname greta-distribution
#' @export
`likelihood<-` <- `distribution<-`


# need a way of setting a distribution for all node types

# need a has_distribution internal function to determine whether an array
# already has a distribution defined
