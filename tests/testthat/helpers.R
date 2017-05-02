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

  parameters_greta <- parameters
  # no dim for wishart
  if (!identical(names(parameters), c('df', 'Sigma')))
    parameters_greta <- c(parameters_greta, dim = NROW(x))

  dist <- do.call(greta_fun, parameters_greta)
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

# an array of random standard normals with the specificed dims
# e.g. randn(3, 2, 1)
randn <- function (...) {
  dim <- c(...)
  array(rnorm(prod(dim)), dim = dim)
}

# check a greta operation and the equivalent R operation give the same output
# e.g. check_op(sum, randn(100, 3))
check_op <- function (op, data) {
  r_out <- op(data)
  greta_array <- op(as_data(data))
  greta_out <- grab(greta_array)
  difference <- as.vector(abs(r_out - greta_out))
  expect_true(all(difference < 1e4))
}

