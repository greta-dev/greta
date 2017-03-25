# syntax definitions


# special operator to create a data node in the parent environment,
# and assign it a likelihood

#' @name greta-likelihood
#' @title Create an Observed, Stochastic Greta Array
#' @description The likelihood operator is used to link observed data with
#'   random variables. This can be used to define the likelhood term for a
#'   model.
#' @param data a fixed greta array, defined using \code{observed()}
#' @param distribution a stochastic greta array, created using a distribution
#' @export
#' @examples
#' # observed data
#' y = observed(rnorm(10))
#'
#' # random variable
#' theta = normal(0, 1)
#'
#' # link them (i.e. we observed theta to have the values in y)
#' y %~% theta
`%~%` <- function (data, distribution) {

  if (!inherits(data$node, 'data_node'))
    stop ('left hand side of likelihood must be a data greta array')

  if (!inherits(distribution$node, 'distribution'))
    stop ('right hand side of likelihood must be a stochastic greta array')

  # provide the data to the likelihood and lock in the values in the
  # distribution
  distribution$node$value(data$node$value())
  distribution$node$.fixed_value <- TRUE

  # give the distribution to the data as a likelihood (this will register the
  # child distribution)
  data$node$set_likelihood(distribution)

  # register the data node, with it's own name
  data$node$register()

}
