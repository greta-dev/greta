# syntax definitions

#' @name greta-likelihood
#' @aliases likelihood
#' @title Define a Likelihood over Data
#' @description The likelihood function is used to link observed data with
#'   random variables. This can be used to define the likelhood term for a
#'   model.
#' @param data either a data greta array (defined using \code{as_data()}), or
#'   some data that can be coereced to a data greta array
#' @param value a stochastic greta array, created using a distribution
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
#' likelihood(y) = normal(mu, sigma)
#'
`likelihood<-` <- function (data, value) {

  data_tmp <- data
  data <- as.greta_array(data)
  distribution <- value

  if (!(is.greta_array(distribution) &&
        inherits(distribution$node, 'distribution'))) {

    stop ('right hand side of likelihood must be a stochastic greta array',
          call. = FALSE)

  }

  if (distribution$node$distribution_name == 'free') {

    stop ('free parameters do not have distributions, ',
          'so cannot be used to define a likelihood',
          call. = FALSE)

  }

  # if theta isn't scalar, make sure it has the right dimensions
  if (!is_scalar(distribution)) {
    if (!identical(dim(data), dim(distribution))) {
      stop ('left- and right-hand side of likelihood have different ',
            'dimensions. The distribution must have dimension of either ',
            paste(dim(data), collapse = ' x '),
            ' or 1 x 1, but instead has dimension ',

            paste(dim(distribution), collapse = ' x '),
            call. = FALSE)
    }
  }

  # provide the data to the likelihood and lock in the values in the
  # distribution
  distribution$node$value(data$node$value())
  distribution$node$.fixed_value <- TRUE

  # give the distribution to the data as a likelihood (this will register the
  # child distribution)
  data$node$set_likelihood(distribution$node)

  # register the data node, with it's own name
  data$node$register()

  data_tmp

}
