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
check_op <- function (op, a, b, greta_op = NULL) {

  if (is.null(greta_op))
    greta_op <- op

  if (missing(b)) {
    r_out <- op(a)
    greta_array <- greta_op(as_data(a))
  } else {
    r_out <- op(a, b)
    greta_array <- greta_op(as_data(a), as_data(b))
  }

  greta_out <- grab(greta_array)
  difference <- as.vector(abs(r_out - greta_out))
  expect_true(all(difference < 1e-4))
}

# take an expression, and execute it, converting the objects named in 'swap' to
# greta arrays

# call_to_text <- function (call)
#   paste(deparse(call), collapse = ' ')

# # can check with_greta works with this code:
# foo <- function (x) {
#   if(inherits(x, 'greta_array'))
#     stop ('noooo')
#   x
# }
# x <- randn(3)
# foo(3)
# with_greta(foo(3), swap = 'x')

# execute a call via greta, swapping the objects named in 'swap' to greta
# arrays, then converting the result back to R. 'swap_scope' tells eval() how
# many environments to go up to get the objects for the swap; 1 would be
# environment above the funct, 2 would be the environment above that etc.
with_greta <- function (call, swap = c('x'), swap_scope = 1) {

  swap_entries <- paste0(swap, ' = as_data(', swap, ')')
  swap_text <- paste0('list(',
                      paste(swap_entries, collapse = ', '),
                      ')')
  swap_list <- eval(parse(text = swap_text),
                    envir = parent.frame(n = swap_scope))

  greta_result <- with(swap_list,
                       eval(call))
  result <- grab(greta_result)

  # account for the fact that greta outputs are 1D arrays; convert them back to
  # R vectors
  if (is.array(result) && length(dim(result)) == 2 && dim(result)[2] == 1) {

    result <- as.vector(result)

  }

  result

}

# check an expression is equivalent when done in R, and when done on greta
# arrays with results ported back to R
# e.g. check_expr(a[1:3], swap = 'a')
check_expr <- function (expr, swap = c('x')) {
  call <- substitute(expr)

  r_out <- eval(expr)
  greta_out <- with_greta(call,
                          swap = swap,
                          swap_scope = 2)

  difference <- as.vector(abs(r_out - greta_out))
  expect_true(all(difference < 1e-4))

}

# generate a random string to describing a binary operation on two variables, do
# an op selected from 'ops' to an arg from 'args' and to init
add_op_string <- function (init = 'a',
                           args = c('a', 'b'),
                           ops = c('+', '-', '*', '/')) {

  op <- sample(ops, 1)
  arg <- sample(args, 1)
  sprintf('(%s %s %s)', arg, op, init)

}

# generate a random function that combines two variables together in a string of
# (n) increasingly bizarre operations
gen_opfun <- function (n, ops) {

  string <- 'a'
  for (i in seq_len(n))
    string <- add_op_string(string, ops = ops)

  fun_string <- sprintf('function(a, b) {%s}', string)

  eval(parse(text = fun_string))

}

# sample n values from a distribution by HMC, check they all have the correct support
# greta array is defined as astochastic in the call, like: sample_distribution(normal(0, 1))
sample_distribution <- function (greta_array, n = 10, lower = -Inf, upper = Inf) {
  m <- define_model(greta_array)
  draws <- mcmc(m, n_samples = n, warmup = 0)
  samples <- as.vector(draws[[1]])
  expect_true(all(samples >= lower & samples <= upper))
}


