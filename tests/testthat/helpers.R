# test functions

library(tensorflow)

# set the seed and flush the graph before running tests
if (greta:::check_tf_version())
  tf$reset_default_graph()

set.seed(2018 - 05 - 30)

expect_ok <- function(expr)
  expect_error(expr, NA)

# evaluate a greta_array, node, or tensor
grab <- function(x, dag = NULL, ...) {

  dots <- list(...)

  if (inherits(x, "node"))
    x <- as.greta_array(x)

  if (inherits(x, "greta_array")) {
    node <- get_node(x)

    dag <- dag_class$new(list(x))
    node$define_tf(dag)
    x <- get(dag$tf_name(node),
             envir = dag$tf_environment)
  }

  dag$build_feed_dict(dots)
  out <- tf$Session()$run(x,
                          feed_dict = dag$tf_environment$feed_dict)
  drop_first_dim(out)

}

# get the value of the target greta array, by passing values for the named
# variable greta arrays via the free state parameter, optionally with batches
grab_via_free_state <- function(target, values, batches = 1) {
  dag <- dag_class$new(list(target))
  dag$define_tf()
  inits <- do.call(initials, values)
  inits_flat <- prep_initials(inits, 1, dag)[[1]]
  if (batches > 1) {
    inits_list <- replicate(batches, inits_flat, simplify = FALSE)
    inits_flat <- do.call(rbind, inits_list)
    vals <- dag$trace_values(inits_flat)[1, ]
  } else {
    vals <- dag$trace_values(inits_flat)
  }
  array(vals, dim = dim(target))
}

is.greta_array <- function(x)
  inherits(x, "greta_array")

set_distribution <- function(dist, data) {
  # fix the value of dist
  dist_node <- get_node(dist)
  data_node <- get_node(data)
  distrib <- dist_node$distribution
  distrib$value(data_node$value())
  distrib$.fixed_value <- TRUE
  data_node$set_distribution(distrib)
  data_node$register()
}

# evaluate the (unadjusted) density of distribution greta array at some data
get_density <- function(distrib, data) {

  x <- as_data(data)
  distribution(x) <- distrib

  # create dag and define the density
  dag <- greta:::dag_class$new(list(x))
  get_node(x)$distribution$define_tf(dag)

  # get the log density as a vector
  tensor_name <- dag$tf_name(get_node(distrib)$distribution)
  tensor <- get(tensor_name, envir = dag$tf_environment)
  as.vector(grab(tensor, dag))

}

compare_distribution <- function(greta_fun, r_fun, parameters, x,
                                 dim = NULL, multivariate = FALSE,
                                 tolerance = 1e-4) {
  # calculate the absolute difference in the log density of some data between
  # greta and a r benchmark.
  # 'greta_fun' is the greta distribution constructor function (e.g. normal())
  # 'r_fun' is the r density function, which must have argument 'log'
  # both of these functions must take the same parameters in the same order
  # 'parameters' is an optionally named list of numeric parameter values
  # x is the vector of values at which to evaluate the log density

  # define greta distribution, with fixed values
  greta_log_density <- greta_density(greta_fun, parameters, x,
                                     dim, multivariate)
  # get R version
  r_log_density <- log(do.call(r_fun, c(list(x), parameters)))

  # return absolute difference
  compare_op(r_log_density, greta_log_density, tolerance)

}

# evaluate the log density of x, given 'parameters' and a distribution
# constructor function 'fun'
greta_density <- function(fun, parameters, x,
                          dim = NULL, multivariate = FALSE) {

  if (is.null(dim))
    dim <- NROW(x)

  # add the output dimension to the arguments list
  dim_list <- list(dim = dim)

  # if it's a multivariate distribution name it n_realisations
  if (multivariate)
    names(dim_list) <- "n_realisations"

  # don't add it for wishart & lkj, which don't mave multiple realisations
  is_wishart <- identical(names(parameters), c("df", "Sigma"))
  is_lkj <- identical(names(parameters), c("eta", "dimension"))
  if (is_wishart | is_lkj)
    dim_list <- list()

  parameters <- c(parameters, dim_list)

  # evaluate greta distribution
  dist <- do.call(fun, parameters)

  distrib_node <- get_node(dist)$distribution

  # set density
  x_ <- as.greta_array(x)
  distrib_node$remove_target()
  distrib_node$add_target(get_node(x_))

  # create dag
  dag <- greta:::dag_class$new(list(x_))
  tfe <- dag$tf_environment

  # define the tensor in an environment
  distrib_node$define_tf(dag)

  # get the log density as a vector
  target <- get(dag$tf_name(get_node(x_)), envir = tfe)
  density <- get(dag$tf_name(distrib_node), envir = tfe)
  result <- density(target)
  as.vector(grab(result, dag))

}

# an array of random standard normals with the specificed dims
# e.g. randn(3, 2, 1)
randn <- function(...) {
  dim <- c(...)
  array(rnorm(prod(dim)), dim = dim)
}


# ditto for standard uniforms
randu <- function(...) {
  dim <- c(...)
  array(runif(prod(dim)), dim = dim)
}

# create a variable with the same dimensions as as_data(x)
as_variable <- function(x) {
  x <- as_2D_array(x)
  variable(dim = dim(x))
}

# check a greta operation and the equivalent R operation give the same output
# e.g. check_op(sum, randn(100, 3))
check_op <- function(op, a, b, greta_op = NULL,
                     other_args = list(),
                     tolerance = 1e-3,
                     only = c("data", "variable", "batched")) {

  if (is.null(greta_op))
    greta_op <- op

  r_out <- run_r_op(op, a, b, other_args)

  for (type in only) {
    # compare with ops on data greta arrays
    greta_out <- run_greta_op(greta_op, a, b, other_args, type)
    compare_op(r_out, greta_out, tolerance)
  }

}

compare_op <- function(r_out, greta_out, tolerance = 1e-4) {
  difference <- as.vector(abs(r_out - greta_out))
  expect_true(all(difference < tolerance))
}

run_r_op <- function(op, a, b, other_args) {

  arg_list <- list(a)
  if (!missing(b)) {
    arg_list <- c(arg_list, list(b))
  }
  arg_list <- c(arg_list, other_args)
  do.call(op, arg_list)

}

run_greta_op <- function(greta_op, a, b, other_args,
                         type = c("data", "variable", "batched")) {

  type <- match.arg(type)

  converter <- switch(type,
                      data = as_data,
                      variable = as_variable,
                      batched = as_variable)

  g_a <- converter(a)

  arg_list <- list(g_a)
  values <- list(g_a = a)

  if (!missing(b)) {
    g_b <- converter(b)
    arg_list <- c(arg_list, list(g_b))
    values <- c(values, list(g_b = b))
  }

  arg_list <- c(arg_list, other_args)
  out <- do.call(greta_op, arg_list)

  if (type == "data") {
    # data greta arrays should provide their own values
    result <- calculate(out, list())
  } else if (type == "variable") {
    result <- grab_via_free_state(out, values)
  } else if (type == "batched") {
    result <- grab_via_free_state(out, values, batches = 3)
  } else {
    result <- calculate(out, values)
  }

  result

}

# execute a call via greta, swapping the objects named in 'swap' to greta
# arrays, then converting the result back to R. 'swap_scope' tells eval() how
# many environments to go up to get the objects for the swap; 1 would be
# environment above the funct, 2 would be the environment above that etc.
with_greta <- function(call, swap = c("x"), swap_scope = 1) {

  swap_entries <- paste0(swap, " = as_data(", swap, ")")
  swap_text <- paste0("list(",
                      paste(swap_entries, collapse = ", "),
                      ")")
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
check_expr <- function(expr, swap = c("x"), tolerance = 1e-4) {

  call <- substitute(expr)

  r_out <- eval(expr)
  greta_out <- with_greta(call,
                          swap = swap,
                          swap_scope = 2)

  compare_op(r_out, greta_out, tolerance)

}

# generate a random string to describing a binary operation on two variables, do
# an op selected from 'ops' to an arg from 'args' and to init
add_op_string <- function(init = "a",
                          args = c("a", "b"),
                          ops = c("+", "-", "*", "/")) {

  op <- sample(ops, 1)
  arg <- sample(args, 1)
  sprintf("(%s %s %s)", arg, op, init)

}

# generate a random function that combines two variables together in a string of
# (n) increasingly bizarre operations
gen_opfun <- function(n, ops) {

  string <- "a"
  for (i in seq_len(n))
    string <- add_op_string(string, ops = ops)

  fun_string <- sprintf("function(a, b) {%s}", string)

  eval(parse(text = fun_string))

}

# sample n values from a distribution by HMC, check they all have the correct
# support greta array is defined as a stochastic in the call
sample_distribution <- function(greta_array, n = 10,
                                lower = -Inf, upper = Inf,
                                warmup = 1) {
  m <- model(greta_array, precision = "double")
  draws <- mcmc(m, n_samples = n, warmup = warmup, verbose = FALSE)
  samples <- as.vector(draws[[1]])
  expect_true(all(samples >= lower & samples <= upper))
}

compare_truncated_distribution <- function(greta_fun,
                                           which,
                                           parameters,
                                           truncation,
                                           tolerance = 1e-4) {
  # calculate the absolute difference in the log density of some data between
  # greta and a r benchmark, for an implied truncated distribution 'greta_array'
  # is a greta array created from a distribution and a constrained variable
  # greta array. 'r_fun' is an r function returning the log density for the same
  # truncated distribution, taking x as its only argument.

  require(truncdist)

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
  dist <- do.call(greta_fun, c(parameters,
                               list(dim = 1, truncation = truncation)))

  distrib_node <- get_node(dist)$distribution

  # set data as the target
  x_ <- as_data(x)
  distribution(x_) <- dist

  # create dag and define the density
  dag <- greta:::dag_class$new(list(x_))
  tfe <- dag$tf_environment

  distrib_node$define_tf(dag)

  # get the log density as a vector
  target <- get(dag$tf_name(get_node(x_)), envir = tfe)
  density <- get(dag$tf_name(distrib_node), envir = tfe)
  result <- density(target)
  greta_log_density <- as.vector(grab(result, dag))

  # return absolute difference
  compare_op(r_log_density, greta_log_density, tolerance)

}

# use the truncdist package to crete a truncated distribution function for use
# in compare_truncated_distribution
truncfun <- function(which = "norm", parameters, truncation) {

  args <- c(spec = which,
            a = truncation[1],
            b = truncation[2],
            parameters)

  function(x) {
    arg_list <- c(x = list(x), args)
    do.call(truncdist::dtrunc, arg_list)
  }

}

# R distribution functions for the location-scale Student T distribution
dt_ls <- function(x, df, location, scale, log = FALSE) {
  ans <- stats::dt( (x - location) / scale, df ) / scale
  if (log)
    ans <- log(ans)
  ans
}

pt_ls <- function(q, df, location, scale, log.p = FALSE) {
  ans <- stats::pt( (q - location) / scale, df )
  if (log.p)
    ans <- log(ans)
  ans
}

qt_ls <- function(p, df, location, scale, log.p = FALSE) {
  ans <- stats::qt(p, df) * scale + location
  if (log.p)
    ans <- log(ans)
  ans
}

# mock up the progress bar to force its output to stdout for testing
cpb <- eval(parse(text = capture.output(dput(greta:::create_progress_bar))))
mock_create_progress_bar <- function(...)
  cpb(..., stream = stdout())

# capture messages in testthat block to get the output of the progress bar;
# copied from progress' test suite:
# https://github.com/r-lib/progress/blob/master/tests/testthat/helper.R
get_output <- function(expr) {
  msgs <- character()
  i <- 0
  suppressMessages(withCallingHandlers(
    expr,
    message = function(e) msgs[[i <<- i + 1]] <<- conditionMessage(e)))
  paste0(msgs, collapse = "")
}

# mock up mcmc progress bar output for neurotic testing
mock_mcmc <- function(n_samples = 1010) {
  pb <- create_progress_bar("sampling", c(0, n_samples),
                            pb_update = 10, width = 50)
  iterate_progress_bar(pb, n_samples, rejects = 10, chains = 1)
}

# apparently testthat can't see these

dinvgamma <- extraDistr::dinvgamma
qinvgamma <- extraDistr::qinvgamma
pinvgamma <- extraDistr::pinvgamma

dlaplace <- extraDistr::dlaplace
plaplace <- extraDistr::plaplace
qlaplace <- extraDistr::qlaplace

dstudent <- extraDistr::dlst
pstudent <- extraDistr::plst
qstudent <- extraDistr::qlst

# mock up pareto to have differently named parameters (a and b are use for the
# truncation)
preto <- function(a_, b_, dim, truncation) pareto(a_, b_, dim, truncation)
dpreto <- function(x, a_, b_) extraDistr::dpareto(x, a_, b_)
ppreto <- function(q, a_, b_) extraDistr::ppareto(q, a_, b_)
qpreto <- function(p, a_, b_) extraDistr::qpareto(p, a_, b_)

# random lkj draws, code from the rethinking package (can't load the package
# because of stan*Travis*compiler issues)
rlkjcorr <- function (n, K, eta = 1) {

  stopifnot(is.numeric(K), K >= 2, K == as.integer(K))
  stopifnot(eta > 0)

  f <- function() {

    alpha <- eta + (K - 2) / 2
    r12 <- 2 * rbeta(1, alpha, alpha) - 1
    R <- matrix(0, K, K)
    R[1, 1] <- 1
    R[1, 2] <- r12
    R[2, 2] <- sqrt(1 - r12 ^ 2)

    if (K > 2) {
      for (m in 2:(K - 1)) {
        alpha <- alpha - 0.5
        y <- rbeta(1, m / 2, alpha)
        z <- rnorm(m, 0, 1)
        z <- z / sqrt(crossprod(z)[1])
        R[1:m, m + 1] <- sqrt(y) * z
        R[m + 1, m + 1] <- sqrt(1 - y)
      }
    }

    crossprod(R)

  }

  R <- replicate(n, f())

  if (dim(R)[3] == 1) {
    R <- R[, , 1]
  } else {
    R <- aperm(R, c(3, 1, 2))
  }

  R

}

# is this a release candidate?
skip_if_not_release <- function() {
  if (identical(Sys.getenv("RELEASE_CANDIDATE"), "true")) {
    return(invisible(TRUE))
  }
  skip("Not a Release Candidate")
}

# run a geweke test on a greta model, providing: 'sampler' a greta sampler (e.g.
# hmc()), a greta 'model', a 'data' greta array used as the target in the model,
# the two IID random number generators for the data generating function
# ('p_theta' = generator for the prior, 'p_x_bar_theta' = generator for the
# likelihood), 'niter' the number of MCMC samples to compare
check_geweke <- function(sampler, model, data,
                         p_theta, p_x_bar_theta,
                         niter = 2000, title = "Geweke test") {

  # sample independently
  target_theta <- p_theta(niter)

  # sample with Markov chain
  greta_theta <- p_theta_greta(niter = niter,
                               model = model,
                               data = data,
                               p_theta = p_theta,
                               p_x_bar_theta = p_x_bar_theta,
                               sampler = sampler)

  # visualise correspondence
  quants <- (1:99) / 100
  q1 <- quantile(target_theta, quants)
  q2 <- quantile(greta_theta, quants)
  plot(q2, q1)
  abline(0, 1)

  # do a formal hypothesis test
  suppressWarnings(stat <- ks.test(target_theta, greta_theta))
  testthat::expect_gte(stat$p.value, 0.01)

}

# sample from a prior on theta the long way round, fro use in a Geweke test:
# gibbs sampling the posterior p(theta | x) and the data generating function p(x
# | theta). Only retain the samples of theta from the joint distribution,
p_theta_greta <- function(niter, model, data,
                          p_theta, p_x_bar_theta,
                          sampler = hmc()) {

  # set up and initialize trace
  theta <- rep(NA, niter)
  theta[1] <- p_theta(1)

  # set up and tune sampler
  draws <- mcmc(model,
                n_samples = 1,
                chains = 1,
                sampler = sampler,
                verbose = FALSE)

  # now loop through, sampling and updating x and returning theta
  for (i in 2:niter) {

    # sample x given theta
    x <- p_x_bar_theta(theta[i - 1])

    # put x in the data list
    dag <- model$dag
    target_name <- dag$tf_name(get_node(data))
    x_array <- array(x, dim = c(1, dim(data)))
    dag$tf_environment$data_list[[target_name]] <- x_array

    # put theta in the free state
    sampler <- attr(draws, "model_info")$samplers[[1]]
    sampler$free_state <- as.matrix(theta[i - 1])

    draws <- extra_samples(draws,
                           n_samples = 1,
                           verbose = FALSE)

    theta[i] <- tail(as.numeric(draws[[1]]), 1)

  }

  theta

}

# test mcmc for models with analytic posteriors

not_finished <- function(draws, target_samples = 5000) {
  neff <- coda::effectiveSize(draws)
  rhats <- coda::gelman.diag(draws, multivariate = FALSE)$psrf[, 1]
  converged <- all(rhats < 1.01)
  enough_samples <- all(neff >= target_samples)
  !(converged & enough_samples)
}

new_samples <- function(draws, target_samples = 5000) {
  neff <- min(coda::effectiveSize(draws))
  efficiency <- neff / coda::niter(draws)
  1.2 * (target_samples - neff) / efficiency
}

not_timed_out <- function(start_time, time_limit = 300) {
  elapsed <- Sys.time() - start_time
  elapsed < time_limit
}

get_enough_draws <- function(model,
                             sampler = sampler,
                             n_effective = 5000,
                             time_limit = 300,
                             verbose = TRUE,
                             one_by_one = FALSE) {

  start_time <- Sys.time()
  draws <- mcmc(model,
                sampler = sampler,
                verbose = verbose,
                one_by_one = one_by_one)

  while (not_finished(draws, n_effective) &
         not_timed_out(start_time, time_limit)) {

    n_samples <- new_samples(draws, n_effective)
    draws <- extra_samples(draws, n_samples,
                           verbose = verbose,
                           one_by_one = one_by_one)

  }

  if (not_finished(draws, n_effective)) {
    stop("could not draws enough effective samples within the time limit")
  }

  draws

}

# Monte Carlo standard error (using batch means)
mcse <- function(draws) {
  n <- nrow(draws)
  b <- floor(sqrt(n))
  a <- floor(n / b)

  group <- function(k) {
    idx <- ( (k - 1) * b + 1 ):(k * b)
    colMeans(draws[idx, , drop = FALSE])
  }

  bm <- vapply(seq_len(a),
               group,
               draws[1, ])

  if (is.null(dim(bm)))
    bm <- t(bm)

  mu_hat <- as.matrix(colMeans(draws))
  ss <- sweep(t(bm), 2, mu_hat, "-") ^ 2
  var_hat <- b * colSums(ss) / (a - 1)
  sqrt(var_hat / n)
}

# absolute error of the estimate, scaled by the Monte Carlo standard error
scaled_error <- function(draws, expectation) {
  draws <- as.matrix(draws)
  se <- mcse(draws)
  est <- colMeans(draws)
  abs(est - expectation) / se
}

# given a sampler (e.g. hmc()) and minimum number of effective samples, ensure
# that the sampler can draw correct samples from a bivariate normal distribution
check_mvn_samples <- function(sampler, n_effective = 3000) {

  # get multivariate normal samples
  mu <- as_data(t(rnorm(2, 0, 5)))
  Sigma <- rWishart(1, 3, diag(2))[, , 1]
  x <- multivariate_normal(mu, Sigma)
  m <- model(x, precision = "single")

  draws <- get_enough_draws(m,
                            sampler = sampler,
                            n_effective = n_effective,
                            verbose = FALSE)

  # get MCMC samples for statistics of the samples (value, variance and
  # correlation of error wrt mean)
  err <- x - mu
  var <- (err) ^ 2
  corr <- prod(err) / prod(sqrt(diag(Sigma)))
  err_var_corr <- c(err, var, corr)
  stat_draws <- calculate(err_var_corr, draws)

  # get true values of these - on average the error should be 0, and the
  # variance and correlation of the errors should encoded in Sigma
  stat_truth <- c(rep(0, 2),
                  diag(Sigma),
                  cov2cor(Sigma)[1, 2])

  # get absolute errors between posterior means and true values, and scale them
  # by time-series Monte Carlo standard errors (the expected amount of
  # uncertainty in the MCMC estimate), to give the number of standard errors
  # away from truth. There's a 1/100 chance of any one of these scaled errors
  # being greater than qnorm(0.99) if the sampler is correct
  errors <- scaled_error(stat_draws, stat_truth)
  expect_lte(max(errors), qnorm(0.99))

}

# sample values of greta array 'x' (which must follow a distribution), and
# compare the samples with iid samples returned by iid_function (which takes the
# number of arguments as its sole argument), producing a labelled qqplot, and
# running a KS test for differences between the two samples
check_samples <- function(x,
                          iid_function,
                          sampler = hmc(),
                          n_effective = 3000,
                          title = NULL,
                          one_by_one = FALSE) {

  m <- model(x, precision = "single")
  draws <- get_enough_draws(m,
                            sampler = sampler,
                            n_effective = n_effective,
                            verbose = FALSE,
                            one_by_one = one_by_one)

  neff <- coda::effectiveSize(draws)
  iid_samples <- iid_function(neff)
  mcmc_samples <- as.matrix(draws)

  # plot
  if (is.null(title)) {
    distrib <- get_node(x)$distribution$distribution_name
    sampler_name <- class(sampler)[1]
    title <- paste(distrib, "with", sampler_name)
  }

  qqplot(mcmc_samples, iid_samples, main = title)
  abline(0, 1)

  # do a formal hypothesis test
  suppressWarnings(stat <- ks.test(mcmc_samples, iid_samples))
  testthat::expect_gte(stat$p.value, 0.01)

}
