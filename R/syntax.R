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
`%~%` <- function (y, theta) {

  if (!inherits(y$node, 'data_node'))
    stop ('left hand side of likelihood must be a data greta array')

  if (!inherits(theta$node, 'distribution'))
    stop ('right hand side of likelihood must be a stochastic greta array')

  # if theta isn't scalar, make sure it has the right dimensions
  if (!is_scalar(theta)) {
    if (!identical(dim(y), dim(theta))) {
      stop (sprintf('left- and right-hand side of likelihood have different dimensions. The distribution must have dimension of either %s or 1 x 1, but instead has dimension %s',
                    paste(dim(y), collapse = ' x '),
                    paste(dim(theta), collapse = ' x ')))
    }
  }

  # provide the data to the likelihood and lock in the values in the
  # distribution
  theta$node$value(y$node$value())
  theta$node$.fixed_value <- TRUE

  # give the distribution to the data as a likelihood (this will register the
  # child distribution)
  y$node$set_likelihood(theta$node)

  # register the data node, with it's own name
  y$node$register()

}
