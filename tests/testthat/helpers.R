# test functions

# set the seed before running tests
set.seed(2017-05-01)

# evaluate a greta_array, node, or tensor
grab <- function (x) {

  if (is.node(x))
    x <- as.greta_array(x)

  if (is.greta_array(x)) {
    dag <- dag_class$new(list(x))
    x$node$define_tf(dag)
    x <- get(dag$tf_name(x$node),
             envir = dag$tf_environment)
  }

  tf$Session()$run(x)

}

set_distribution <- function(dist, data) {
  # fix the value of dist
  distrib <- dist$node$distribution
  distrib$value(data$node$value())
  distrib$.fixed_value <- TRUE
  data$node$set_distribution(distrib)
  data$node$register()
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

  tf$reset_default_graph()

  parameters_greta <- parameters
  # no dim for wishart
  if (!identical(names(parameters), c('df', 'Sigma')))
    parameters_greta <- c(parameters_greta, dim = NROW(x))

  # evaluate greta distribution
  dist <- do.call(greta_fun, parameters_greta)

  # set density
  x_ <- as_data(x)
  distribution(x_) = dist

  # create dag
  dag <- greta:::dag_class$new(list(x_))

  # define the tensor in an environment
  dist$node$distribution$define_tf(dag)

  # get the log density as a vector
  tensor_name <- dag$tf_name(dist$node$distribution)
  tensor <- get(tensor_name, envir = dag$tf_environment)
  greta_log_density <- as.vector(grab(tensor))

  # get R version
  r_log_density <- log(do.call(r_fun, c(list(x), parameters)))

  # return absolute difference
  abs(greta_log_density - r_log_density)

}

# an array of random standard normals with the specificed dims
# e.g. randn(3, 2, 1)
randn <- function (...) {
  dim <- c(...)
  array(rnorm(prod(dim)), dim = dim)
}


# ditto for standard uniforms
randu <- function (...) {
  dim <- c(...)
  array(runif(prod(dim)), dim = dim)
}

# check a greta operation and the equivalent R operation give the same output
# e.g. check_op(sum, randn(100, 3))
check_op <- function (op, a, b, greta_op = NULL) {

  tf$reset_default_graph()

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

  tf$reset_default_graph()

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
  m <- model(greta_array)
  draws <- mcmc(m, n_samples = n, warmup = 1, verbose = FALSE)
  samples <- as.vector(draws[[1]])
  expect_true(all(samples >= lower & samples <= upper))
}

# R versions of dynamics module methods
it_lambda <- function (matrix, state, niter) {
  states <- list(state)
  for (i in seq_len(niter))
    states[[i + 1]] <- states[[i]] %*% matrix
  states[[niter + 1]][1] / states[[niter]][1]
}

it_state <- function (matrix, state, niter) {
  for (i in seq_len(niter))
    state <- state %*% matrix
  state[1, ]
}

compare_truncated_distribution <- function (greta_fun,
                                            which,
                                            parameters,
                                            truncation) {
  # calculate the absolute difference in the log density of some data between
  # greta and a r benchmark, for an implied truncated distribution 'greta_array'
  # is a greta array created from a distribution and a constrained free() greta
  # array. 'r_fun' is an r function returning the log density for the same
  # truncated distribution, taking x as its only argument.

  tf$reset_default_graph()

  require (truncdist)

  x <- do.call(truncdist::rtrunc,
               c(n = 100,
                 spec = which,
                 a = truncation[1],
                 b = truncation[2],
                 parameters))

  # create truncated R function and evaluate it
  r_fun <- truncfun(which, parameters, truncation)
  r_log_density <- log(r_fun(x))

  # create greta array for truncated distribution
  z <- free(truncation[1], truncation[2])
  dist = do.call(greta_fun, parameters)
  distribution(z) = dist

  # set data as the target
  x_ <- as_data(x)
  distribution(x_) = dist

  # create dag and define the density
  dag <- greta:::dag_class$new(list(x_))

  x_$node$distribution$define_tf(dag)

  # get the log density as a vector
  tensor_name <- dag$tf_name(dist$node$distribution)
  tensor <- get(tensor_name, envir = dag$tf_environment)
  greta_log_density <- as.vector(grab(tensor))


  # return absolute difference
  abs(greta_log_density - r_log_density)

}

# use the truncdist package to crete a truncated distribution function for use
# in compare_truncated_distribution
truncfun <- function (which = 'norm', parameters, truncation) {

  args <- c(spec = which,
                a = truncation[1],
                b = truncation[2],
                parameters)

  function (x) {
    arg_list <- c(x = list(x), args)
    do.call(truncdist::dtrunc, arg_list)
  }

}

# R distribution functions for the location-scale Student T distribution
dt_ls <- function (x, df, location, scale, log = FALSE) {
  ans <- stats::dt((x - location) / scale, df) / scale
  if (log)
    ans <- log(ans)
  ans
}

pt_ls <- function (q, df, location, scale, log.p = FALSE) {
  ans <- stats::pt((q - location) / scale, df)
  if (log.p)
    ans <- log(ans)
  ans
}

qt_ls <- function (p, df, location, scale, log.p = FALSE) {
  ans <- stats::qt(p, df) * scale + location
  if (log.p)
    ans <- log(ans)
  ans
}

# inverse gamma
dinvgamma <- MCMCpack::dinvgamma
pinvgamma <- function (q, shape, scale) {
  if (q < 0)
    0
  else
    1 - pgamma(1 / q, shape, scale)
}
qinvgamma <- function(p, shape, scale)
  1 / qgamma(1 - p, shape, scale)

# apparently testthat can't see these
dlaplace <- extraDistr::dlaplace
plaplace <- extraDistr::plaplace
qlaplace <- extraDistr::qlaplace

# mock up the paretos to have differently named parameters
preto <- function(a_, b_) pareto(a_, b_)
dpreto <- function(x, a_, b_) extraDistr::dpareto(x, a_, b_)
ppreto <- function(q, a_, b_) extraDistr::ppareto(q, a_, b_)
qpreto <- function(p, a_, b_) extraDistr::qpareto(p, a_, b_)


# mock up the progress bar to force its output to stdout for testing
cpb <- eval(parse(text = capture.output(dput(greta:::create_progress_bar))))
mock_create_progress_bar <- function(...)
  cpb(..., stream = stdout(), force = TRUE)

mock_mcmc <- function (n_samples = 101) {
  pb <- create_progress_bar('sampling', c(0, n_samples))
  iterate_progress_bar(pb, n_samples, rejects = 1)
}
