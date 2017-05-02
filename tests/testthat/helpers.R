# test functions

set.seed(2017-05-01)

# evaluate a greta_array, node, or tensor
grab <- function (x) {

  if (inherits(x, 'greta_array'))
    x <- x$node

  if (inherits(x, 'node')) {
    x$define_tf(environment())
    x <- get(x$name)
  }

  tf$Session()$run(x)

}

compare_distribution <- function (greta_fun, r_fun, parameters, x) {
  # calculate the absolute difference in the log density of some data between
  # greta and a r benchmark.
  # 'greta_fun' is the greta distribution constructor function (e.g. normal())
  # 'r_fun' is the r density function, which must have argument 'log'
  # both of these functions must take the same parameters in the same order
  # 'parameters' is an optionally named list of numeric parameter values
  # x is the vector of values at which to evaluate the log density

  # define greta distribution, with fixed values
  dist <- do.call(greta_fun, c(parameters, dim = NROW(x)))
  likelihood(x) = dist

  stopifnot(dist$node$.fixed_value)

  # define the tensor in an environment
  env <- new.env()
  dist$node$define_tf(env = env)

  # get the log density as a vector
  tensor_name <- paste0(dist$node$name, '_density')
  tensor <- get(tensor_name, envir = env)
  greta_log_density <- as.vector(grab(tensor))

  # get R version
  r_log_density <- do.call(r_fun,
                           c(list(x), parameters, log = TRUE))

  # return absolute difference
  abs(greta_log_density - r_log_density)

}
