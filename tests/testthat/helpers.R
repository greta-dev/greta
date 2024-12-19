# test functions

# set the seed and flush the graph before running tests
if (check_tf_version()) {
  tensorflow::tf$compat$v1$reset_default_graph()
}

set.seed(2020 - 02 - 11)

rng_seed <- function() {
  get(".Random.seed", envir = .GlobalEnv)
}

# evaluate a greta_array, node, or tensor
grab <- function(x, dag = NULL) {
  if (inherits(x, "node")) {
    x <- as.greta_array(x)
  }

  if (inherits(x, "greta_array")) {
    node <- get_node(x)
    dag <- dag_class$new(list(x))
  }

  dag$tf_environment$batch_size <- 1L
  node$define_tf(dag)
  x_name <- dag$tf_name(node)
  out <- dag$tf_environment[[x_name]]
  out <- as.array(out)
  drop_first_dim(out)
}

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
  dag <- dag_class$new(list(x))
  get_node(x)$distribution$define_tf(dag)

  # get the log density as a vector
  tensor_name <- dag$tf_name(get_node(distrib)$distribution)
  tensor <- get(tensor_name, envir = dag$tf_environment)
  as.vector(grab(tensor, dag))
}

compare_distribution <- function(
  greta_fun,
  r_fun,
  parameters,
  x,
  dim = NULL,
  multivariate = FALSE,
  tolerance = 1e-4
) {
  # calculate the absolute difference in the log density of some data between
  # greta and a r benchmark.
  # 'greta_fun' is the greta distribution constructor function (e.g. normal())
  # 'r_fun' is the r density function, which must have argument 'log'
  # both of these functions must take the same parameters in the same order
  # 'parameters' is an optionally named list of numeric parameter values
  # x is the vector of values at which to evaluate the log density

  # define greta distribution, with fixed values
  greta_log_density <- greta_density(
    greta_fun,
    parameters,
    x,
    dim,
    multivariate
  )
  # get R version
  r_log_density <- log(do.call(r_fun, c(list(x), parameters)))

  # return absolute difference
  compare_op(r_log_density, greta_log_density, tolerance)
}

# evaluate the log density of x, given 'parameters' and a distribution
# constructor function 'fun'
greta_density <- function(
  fun,
  parameters,
  x,
  dim = NULL,
  multivariate = FALSE
) {

  dim <- dim %||% NROW(x)

  # add the output dimension to the arguments list
  dim_list <- list(dim = dim)

  # if it's a multivariate distribution name it n_realisations
  if (multivariate) {
    names(dim_list) <- "n_realisations"
  }

  # don't add it for wishart & lkj, which don't mave multiple realisations
  is_wishart <- identical(names(parameters), c("df", "Sigma"))
  is_lkj <- identical(names(parameters), c("eta", "dimension"))
  if (is_wishart | is_lkj) {
    dim_list <- list()
  }

  parameters <- c(parameters, dim_list)

  # evaluate greta distribution
  dist <- do.call(fun, parameters)
  distrib_node <- get_node(dist)$distribution

  # set density
  x_ <- as.greta_array(x)
  distrib_node$remove_target()
  distrib_node$add_target(get_node(x_))

  # create dag
  dag <- dag_class$new(list(x_))

  dag$tf_environment$batch_size <- 1L
  distrib_node$define_tf(dag)

  # get the log density as a vector
  result <- dag$evaluate_density(distrib_node, get_node(x_))
  as.vector(result)
}

# execute a call via greta, swapping the objects named in 'swap' to greta
# arrays, then converting the result back to R. 'swap_scope' tells eval() how
# many environments to go up to get the objects for the swap; 1 would be
# environment above the funct, 2 would be the environment above that etc.
with_greta <- function(call, swap = c("x"), swap_scope = 1) {
  swap_entries <- paste0(swap, " = as_data(", swap, ")")
  swap_text <- paste0(
    "list(",
    paste(swap_entries, collapse = ", "),
    ")"
  )
  swap_list <- eval(
    parse(text = swap_text),
    envir = parent.frame(n = swap_scope)
  )

  greta_result <- with(
    swap_list,
    eval(call)
  )
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
  greta_out <- with_greta(
    call,
    swap = swap,
    swap_scope = 2
  )

  compare_op(r_out, greta_out, tolerance)
}

# generate a random string to describing a binary operation on two variables, do
# an op selected from 'ops' to an arg from 'args' and to init
add_op_string <- function(
  init = "a",
  args = c("a", "b"),
  ops = c("+", "-", "*", "/")
) {
  op <- sample(ops, 1)
  arg <- sample(args, 1)
  sprintf("(%s %s %s)", arg, op, init)
}

# generate a random function that combines two variables together in a string of
# (n) increasingly bizarre operations
gen_opfun <- function(n, ops) {
  string <- "a"
  for (i in seq_len(n)) {
    string <- add_op_string(string, ops = ops)
  }

  fun_string <- sprintf("function(a, b) {%s}", string)

  eval(parse(text = fun_string))
}

# sample n values from a distribution by HMC, check they all have the correct
# support greta array is defined as a stochastic in the call
sample_distribution <- function(
  greta_array,
  n = 10,
  lower = -Inf,
  upper = Inf,
  warmup = 1
) {
  m <- model(greta_array, precision = "double")
  draws <- mcmc(m, n_samples = n, warmup = warmup, verbose = FALSE)
  samples <- as.matrix(draws)
  vectorised <- length(lower) > 1 | length(upper) > 1

  if (vectorised) {
    above_lower <- sweep(samples, 2, lower, `>=`)
    below_upper <- sweep(samples, 2, upper, `<=`)
  } else {
    above_lower <- samples >= lower
    below_upper <- samples <= upper
  }

  expect_true(all(above_lower & below_upper))
}

compare_truncated_distribution <- function(
  greta_fun,
  which,
  parameters,
  truncation,
  tolerance = 1e-4
) {
  # calculate the absolute difference in the log density of some data between
  # greta and a r benchmark, for an implied truncated distribution 'greta_array'
  # is a greta array created from a distribution and a constrained variable
  # greta array. 'r_fun' is an r function returning the log density for the same
  # truncated distribution, taking x as its only argument.

  x <- do.call(
    truncdist::rtrunc,
    c(
      n = 100,
      spec = which,
      a = truncation[1],
      b = truncation[2],
      parameters
    )
  )

  # create truncated R function and evaluate it
  r_fun <- truncfun(which, parameters, truncation)
  r_log_density <- log(r_fun(x))

  greta_log_density <- greta_density(
    fun = greta_fun,
    parameters = c(parameters, list(truncation = truncation)),
    x = x,
    dim = 1
  )

  # return absolute difference
  compare_op(r_log_density, greta_log_density, tolerance)
}

# use the truncdist package to crete a truncated distribution function for use
# in compare_truncated_distribution
truncfun <- function(which = "norm", parameters, truncation) {
  args <- c(
    spec = which,
    a = truncation[1],
    b = truncation[2],
    parameters
  )

  function(x) {
    arg_list <- c(x = list(x), args)
    do.call(truncdist::dtrunc, arg_list)
  }
}

# R distribution functions for the location-scale Student T distribution
dt_ls <- function(x, df, location, scale, log = FALSE) {
  ans <- stats::dt((x - location) / scale, df) / scale
  if (log) {
    ans <- log(ans)
  }
  ans
}

pt_ls <- function(q, df, location, scale, log.p = FALSE) {
  ans <- stats::pt((q - location) / scale, df)
  if (log.p) {
    ans <- log(ans)
  }
  ans
}

qt_ls <- function(p, df, location, scale, log.p = FALSE) {
  ans <- stats::qt(p, df) * scale + location
  if (log.p) {
    ans <- log(ans)
  }
  ans
}

# mock up the progress bar to force its output to stdout for testing
cpb <- eval(parse(text = capture.output(dput(create_progress_bar))))
mock_create_progress_bar <- function(...) {
  cpb(..., stream = stdout())
}

# capture messages in testthat block to get the output of the progress bar;
# copied from progress' test suite:
# https://github.com/r-lib/progress/blob/master/tests/testthat/helper.R
get_output <- function(expr) {
  msgs <- character()
  i <- 0
  suppressMessages(withCallingHandlers(
    expr,
    message = function(e) msgs[[i <<- i + 1]] <<- conditionMessage(e)
  ))
  paste0(msgs, collapse = "")
}

# mock up mcmc progress bar output for neurotic testing
mock_mcmc <- function(n_samples = 1010) {
  pb <- create_progress_bar(
    "sampling",
    c(0, n_samples),
    pb_update = 10,
    width = 50
  )
  iterate_progress_bar(pb, n_samples, rejects = 10, chains = 1)
}

# random lkj draws, code from the rethinking package (can't load the package
# because of stan*Travis*compiler issues)
rlkjcorr <- function(n, eta = 1, dimension = 2) {
  k <- dimension
  stopifnot(is.numeric(k), k >= 2, k == as.integer(k))
  stopifnot(eta > 0)

  f <- function() {
    alpha <- eta + (k - 2) / 2
    r12 <- 2 * stats::rbeta(1, alpha, alpha) - 1
    r <- matrix(0, k, k)
    r[1, 1] <- 1
    r[1, 2] <- r12
    r[2, 2] <- sqrt(1 - r12^2)

    if (k > 2) {
      for (m in 2:(k - 1)) {
        alpha <- alpha - 0.5
        y <- rbeta(1, m / 2, alpha)
        z <- rnorm(m, 0, 1)
        z <- z / sqrt(crossprod(z)[1])
        r[1:m, m + 1] <- sqrt(y) * z
        r[m + 1, m + 1] <- sqrt(1 - y)
      }
    }

    crossprod(r)
  }

  r <- replicate(n, f())

  if (dim(r)[3] == 1) {
    r <- r[, , 1]
  } else {
    r <- aperm(r, c(3, 1, 2))
  }

  r
}

# normalising component of lkj (depends only on eta and dimension)
# NOTE
# we may need to find a better way to do the normalising of this/find a better
# reference equation. E.g., looking in
# potentially our implementation here is incorrect. This is not impacting
# IID sampling or MCMC, but is currently causing some tests to fail where we
# are ensuring our densities are accurate (in test_distributions.R)
lkj_log_normalising <- function(eta, n) {
  log_pi <- log(pi)
  ans <- 0
  for (k in 1:(n - 1)) {
    ans <- ans + log_pi * (k / 2)
    ans <- ans + lgamma(eta + (n - 1 - k) / 2)
    ans <- ans - lgamma(eta + (n - 1) / 2)
  }
  ans
}

dlkj_correlation_unnormalised <- function(x, eta, log = FALSE, dimension = NULL) {
  res <- (eta - 1) * log(det(x))
  if (!log) {
    res <- exp(res)
  }
  res
}

# lkj density
dlkj_correlation <- function(x, eta, log = FALSE, dimension = NULL) {
  res <- (eta - 1) * log(det(x)) - lkj_log_normalising(eta, ncol(x))
  if (!log) {
    res <- exp(res)
  }
  res
}

# helper RNG functions
rmvnorm <- function(n, mean, Sigma) { # nolint
  mvtnorm::rmvnorm(n = n, mean = mean, sigma = Sigma)
}

rwish <- function(n, df, Sigma) { # nolint
  draws <- stats::rWishart(n = n, df = df, Sigma = Sigma)
  aperm(draws, c(3, 1, 2))
}

rmulti <- function(n, size, prob) {
  draws <- stats::rmultinom(n = n, size = size, prob = prob)
  t(draws)
}

rcat <- function(n, prob) {
  rmulti(n, 1, prob)
}

rtnorm <- function(n, mean, sd, truncation) {
  truncdist::rtrunc(
    n,
    "norm",
    a = truncation[1],
    b = truncation[2],
    mean = mean,
    sd = sd
  )
}

rtlnorm <- function(n, meanlog, sdlog, truncation) {
  truncdist::rtrunc(
    n,
    "lnorm",
    a = truncation[1],
    b = truncation[2],
    meanlog = meanlog,
    sdlog = sdlog
  )
}

rtweibull <- function(n, shape, scale, truncation) {
  truncdist::rtrunc(
    n,
    "weibull",
    a = truncation[1],
    b = truncation[2],
    shape = shape,
    scale = scale
  )
}

rtf <- function(n, df1, df2, truncation) {
  truncdist::rtrunc(
    n,
    "f",
    a = truncation[1],
    b = truncation[2],
    df1 = df1,
    df2 = df2
  )
}


# joint testing functions
joint_normals <- function(...) {
  params_list <- list(...)
  components <- lapply(params_list, function(par) do.call(normal, par))
  do.call(joint, components)
}

rjnorm <- function(n, ...) {
  params_list <- list(...)
  args_list <- lapply(params_list, function(par) c(n, par))
  sims <- lapply(args_list, function(par) do.call(stats::rnorm, par))
  do.call(cbind, sims)
}

rjtnorm <- function(n, ...) {
  params_list <- list(...)
  args_list <- lapply(params_list, function(par) c(n, par))
  sims <- lapply(args_list, function(par) do.call(rtnorm, par))
  do.call(cbind, sims)
}

# mixture testing functions
mixture_normals <- function(...) {
  args <- list(...)
  is_weights <- names(args) == "weights"
  params_list <- args[!is_weights]
  components <- lapply(params_list, function(par) do.call(normal, par))
  do.call(mixture, c(components, args[is_weights]))
}

mixture_multivariate_normals <- function(...) {
  args <- list(...)
  is_weights <- names(args) == "weights"
  params_list <- args[!is_weights]
  components <- lapply(
    params_list,
    function(par) {
      do.call(multivariate_normal, par)
    }
  )
  do.call(mixture, c(components, args[is_weights]))
}

rmixnorm <- function(n, ...) {
  args <- list(...)
  is_weights <- names(args) == "weights"
  params_list <- args[!is_weights]
  weights <- args[[which(is_weights)]]
  args_list <- lapply(params_list, function(par) c(n, par))
  sims <- lapply(args_list, function(par) do.call(rnorm, par))
  draws <- do.call(cbind, sims)
  components <- sample.int(length(sims), n, prob = weights, replace = TRUE)
  idx <- cbind(seq_len(n), components)
  draws[idx]
}

rmixtnorm <- function(n, ...) {
  args <- list(...)
  is_weights <- names(args) == "weights"
  params_list <- args[!is_weights]
  weights <- args[[which(is_weights)]]
  args_list <- lapply(params_list, function(par) c(n, par))
  sims <- lapply(args_list, function(par) do.call(rtnorm, par))
  draws <- do.call(cbind, sims)
  components <- sample.int(length(sims), n, prob = weights, replace = TRUE)
  idx <- cbind(seq_len(n), components)
  draws[idx]
}

rmixmvnorm <- function(n, ...) {
  args <- list(...)
  is_weights <- names(args) == "weights"
  params_list <- args[!is_weights]
  weights <- args[[which(is_weights)]]
  args_list <- lapply(params_list, function(par) c(n, par))
  sims <- lapply(args_list, function(par) do.call(rmvnorm, par))

  components <- sample.int(length(sims), n, prob = weights, replace = TRUE)

  # loop through the n observations, pulling out the corresponding slice
  draws_out <- array(NA, dim(sims[[1]]))
  for (i in seq_len(n)) {
    draws_out[i, ] <- sims[[components[i]]][i, ]
  }
  draws_out
}

# a form of two-sample chi squared test for discrete multivariate distributions
combined_chisq_test <- function(x, y) {
  stats::chisq.test(
    x = colSums(x),
    y = colSums(y)
  )
}

# flatten unique part of a symmetric matrix
get_upper_tri <- function(x, diag) {
  x[upper.tri(x, diag = diag)]
}

# compare iid samples from a greta distribution (using calculate) against a
# comparison R RNG function
compare_iid_samples <- function(
  greta_fun,
  r_fun,
  parameters,
  nsim = 200,
  p_value_threshold = 0.001
) {
  greta_array <- do.call(greta_fun, parameters)

  # get information about distribution
  distribution <- get_node(greta_array)$distribution
  multivariate <- distribution$multivariate
  discrete <- distribution$discrete
  name <- distribution$distribution_name

  greta_samples <- calculate(greta_array, nsim = nsim)[[1]]
  r_samples <- do.call(r_fun, c(n = nsim, parameters))

  # reshape to matrix or vector
  if (multivariate) {
    # if it's a symmetric matrix, take only a triangle and flatten it
    if (name %in% c("wishart", "lkj_correlation")) {
      include_diag <- name == "wishart"
      t_greta_samples <- apply(greta_samples, 1, get_upper_tri, include_diag)
      t_r_samples <- apply(r_samples, 1, get_upper_tri, include_diag)
      greta_samples <- t(t_greta_samples)
      r_samples <- t(t_r_samples)
    } else {
      dim <- dim(greta_samples)
      new_dim <- c(dim[1], dim[2] * dim[3])
      dim(greta_samples) <- new_dim
      dim(r_samples) <- new_dim
    }
  } else {
    greta_samples <- as.vector(greta_samples)
  }

  # find a vaguely appropriate test
  if (discrete) {
    test <- ifelse(multivariate, combined_chisq_test, stats::chisq.test)
  } else {
    test <- ifelse(multivariate, cramer::cramer.test, stats::ks.test)
  }

  # do Kolmogorov Smirnov test on samples
  suppressWarnings(test_result <- test(greta_samples, r_samples))
  testthat::expect_gte(test_result$p.value, p_value_threshold)
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
check_geweke <- function(
  sampler,
  model,
  data,
  p_theta,
  p_x_bar_theta,
  niter = 2000,
  warmup = 1000,
  thin = 1
) {
  # sample independently
  target_theta <- p_theta(niter)

  # sample with Markov chain
  greta_theta <- p_theta_greta(
    niter = niter,
    model = model,
    data = data,
    p_theta = p_theta,
    p_x_bar_theta = p_x_bar_theta,
    sampler = sampler,
    warmup = warmup
  )

  geweke_checks <- list(
    target_theta = do_thinning(target_theta, thin),
    greta_theta = do_thinning(greta_theta, thin)
  )

  geweke_checks

}

geweke_qq <- function(geweke_checks, title){
  # visualise correspondence
  quants <- (1:99) / 100
  q1 <- stats::quantile(geweke_checks$target_theta, quants)
  q2 <- stats::quantile(geweke_checks$greta_theta, quants)
  plot(q2, q1, main = title)
  graphics::abline(0, 1)

}

geweke_ks <- function(geweke_checks){
  # do a formal hypothesis test
  suppressWarnings(stat <- stats::ks.test(geweke_checks$target_theta,
                                          geweke_checks$greta_theta))
  stat
}

# sample from a prior on theta the long way round, fro use in a Geweke test:
# gibbs sampling the posterior p(theta | x) and the data generating function p(x
# | theta). Only retain the samples of theta from the joint distribution,
p_theta_greta <- function(
  niter,
  model,
  data,
  p_theta,
  p_x_bar_theta,
  sampler = hmc(),
  warmup = 1000
) {
  # set up and initialize trace
  theta <- rep(NA, niter)
  theta[1] <- p_theta(1)

  # set up and tune sampler
  draws <- mcmc(
    model,
    warmup = warmup,
    n_samples = 1,
    chains = 1,
    sampler = sampler,
    verbose = FALSE
  )

  # set up a progress bar and do a first increment
  cli::cli_progress_bar("Geweke test iterations", total = niter)
  cli::cli_progress_update()

  # now loop through, sampling and updating x and returning theta
  for (i in 2:niter) {

    # update the progress bar
    cli::cli_progress_update()

    # sample x given theta
    x <- p_x_bar_theta(theta[i - 1])

    # replace x in the node
    dag <- model$dag
    x_node <- get_node(data)
    x_node$value(as.matrix(x))

    # rewrite the log prob tf function, and the tf function for the posterior
    # samples, now using this value of x (slow, but necessary in eager mode)
    dag$tf_log_prob_function <- NULL
    dag$define_tf_log_prob_function()
    sampler <- attr(draws, "model_info")$samplers[[1]]
    sampler$define_tf_evaluate_sample_batch()

    # take anoteher sample
    draws <- extra_samples(
      draws,
      n_samples = 1,
      verbose = FALSE
    )

    # trace the sample
    theta[i] <- tail(as.numeric(draws[[1]]), 1)

  }

  # kill the progress_bar
  cli::cli_progress_done()

  theta
}

# test mcmc for models with analytic posteriors

need_more_samples <- function(draws, target_samples = 5000) {
  neff <- coda::effectiveSize(draws)
  rhats <- coda::gelman.diag(
    x = draws,
    multivariate = FALSE,
    autoburnin = FALSE
  )
  rhats <- rhats$psrf[, 1]
  converged <- all(rhats < 1.01)
  enough_samples <- all(neff >= target_samples)
  !(converged & enough_samples)
}

new_samples <- function(draws, target_samples = 5000) {
  neff <- min(coda::effectiveSize(draws))
  efficiency <- neff / coda::niter(draws)
  1.2 * (target_samples - neff) / efficiency
}

still_have_time <- function(start_time, time_limit = 300) {
  elapsed <- Sys.time() - start_time
  elapsed < time_limit
}

get_enough_draws <- function(
  model,
  sampler = sampler,
  n_effective = 5000,
  time_limit = 300,
  verbose = TRUE,
  one_by_one = FALSE
) {
  start_time <- Sys.time()
  draws <- mcmc(
    model,
    sampler = sampler,
    verbose = verbose,
    one_by_one = one_by_one
  )

  while (need_more_samples(draws, n_effective) &&
         still_have_time(start_time, time_limit)) {
    n_samples <- new_samples(draws, n_effective)
    draws <- extra_samples(
      draws,
      n_samples,
      verbose = verbose,
      one_by_one = one_by_one
    )
  }

  if (need_more_samples(draws, n_effective)) {
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
    idx <- ((k - 1) * b + 1):(k * b)
    colMeans(draws[idx, , drop = FALSE])
  }

  bm <- vapply(
    seq_len(a),
    group,
    draws[1, ]
  )

  if (is.null(dim(bm))) {
    bm <- t(bm)
  }

  mu_hat <- as.matrix(colMeans(draws))
  ss <- sweep(t(bm), 2, mu_hat, "-")^2
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
  sigma <- stats::rWishart(1, 3, diag(2))[, , 1]
  x <- multivariate_normal(mu, sigma)
  m <- model(x, precision = "single")

  draws <- get_enough_draws(
    m,
    sampler = sampler,
    n_effective = n_effective,
    verbose = FALSE
  )

  # get MCMC samples for statistics of the samples (value, variance and
  # correlation of error wrt mean)
  err <- x - mu
  var <- (err)^2
  corr <- prod(err) / prod(sqrt(diag(sigma)))
  err_var_corr <- c(err, var, corr)
  stat_draws <- calculate(err_var_corr, values = draws)

  # get true values of these - on average the error should be 0, and the
  # variance and correlation of the errors should encoded in Sigma
  stat_truth <- c(
    rep(0, 2),
    diag(sigma),
    cov2cor(sigma)[1, 2]
  )

  # get absolute errors between posterior means and true values, and scale them
  # by time-series Monte Carlo standard errors (the expected amount of
  # uncertainty in the MCMC estimate), to give the number of standard errors
  # away from truth. There's a 1/100 chance of any one of these scaled errors
  # being greater than qnorm(0.99) if the sampler is correct
  errors <- scaled_error(stat_draws, stat_truth)
  errors
}

do_thinning <- function(x, thinning = 1) {
  idx <- seq(1, length(x), by = thinning)
  x[idx]
}


get_distribution_name <- function(x){
  x_node <- get_node(x)
  if (inherits(x_node, "operation_node")){
    dist_name <- x_node$parents[[1]]$distribution$distribution_name
  } else {
    dist_name <- get_node(x)$distribution$distribution_name
  }
  dist_name
}


# sample values of greta array 'x' (which must follow a distribution), and
# compare the samples with iid samples returned by iid_function (which takes the
# number of arguments as its sole argument), producing a labelled qqplot, and
# running a KS test for differences between the two samples
check_samples <- function(
  x,
  iid_function,
  sampler = hmc(),
  n_effective = 3000,
  title = NULL,
  one_by_one = FALSE,
  time_limit = 300
) {
  m <- model(x, precision = "single")
  draws <- get_enough_draws(
    model = m,
    sampler = sampler,
    n_effective = n_effective,
    verbose = TRUE,
    one_by_one = one_by_one,
    time_limit = time_limit
  )

  neff <- coda::effectiveSize(draws)
  iid_samples <- iid_function(neff)
  mcmc_samples <- as.matrix(draws)

  thin_amount <- find_thinning(draws)

  mcmc_samples <- do_thinning(mcmc_samples, thin_amount)
  iid_samples <- do_thinning(iid_samples, thin_amount)

  list(
    mcmc_samples = mcmc_samples,
    iid_samples = iid_samples,
    distrib = get_distribution_name(x),
    sampler_name = class(sampler)[1]
  )
}

qqplot_checked_samples <- function(checked_samples, title){

  distrib <- checked_samples$distrib
  sampler_name <- checked_samples$sampler_name
  title <- paste(distrib, "with", sampler_name)

  mcmc_samples <- checked_samples$mcmc_samples
  iid_samples <- checked_samples$iid_samples

  stats::qqplot(
    x = mcmc_samples,
    y = iid_samples,
    main = title
    )

  graphics::abline(0, 1)
}

## helpers for running Kolmogorov-Smirnov test for MCMC samples vs IID samples
ks_test_mcmc_vs_iid <- function(checked_samples){
  # do a formal hypothesis test
  suppressWarnings(stat <- ks.test(checked_samples$mcmc_samples,
                                   checked_samples$iid_samples))
  stat
}

## helpers for looping through optimisers
run_opt <- function(
  m,
  optmr,
  max_iterations = 200
) {
  opt(
    m,
    optimiser = optmr(),
    max_iterations = max_iterations
  )
}

possibly_run_opt <- purrr::possibly(.f = run_opt, otherwise = "error")

opt_df_run <- function(optimisers, m, x) {
  opt_df <- tibble::enframe(
    x = optimisers,
    name = "opt",
    value = "opt_fn"
  ) %>%
    dplyr::mutate(
      result = lapply(opt_fn, possibly_run_opt, m = m),
      x_val = list(x)
    )

  opt_df
}

tidy_optimisers <- function(opt_df, tolerance = 1e-2) {
  opt_df %>%
    dplyr::select(-opt_fn) %>%
    tidyr::unnest_wider(col = c(result)) %>%
    dplyr::mutate(
      par = unname(purrr::flatten(par)),
      par_x_diff = purrr::map2(
        .x = par,
        .y = x_val,
        .f = function(.x, .y){
          abs(.y - .x)
      }),
      close_to_truth = purrr::map_lgl(
        par_x_diff,
        function(x) all(x < tolerance)
      )
    ) %>%
    dplyr::relocate(
      close_to_truth,
      par_x_diff,
      iterations,
      convergence,
      .after = opt
    )
}


# find a thinning rate that sufficiently reduces autocorrelation in the samples
# example
# x <- normal(0, 1)
# m <- model(x)
# draws <- mcmc(m)
# find_thinning(draws)
find_thinning <- function(draws, max_thin = 100, autocorr_threshold = 0.01) {
  autocorr_list <- coda::autocorr(draws, lags = seq_len(max_thin))
  autocorrs <- do.call(cbind, autocorr_list)
  mean_autocorr <- rowMeans(autocorrs)
  smallest_thin <- which(mean_autocorr < autocorr_threshold)[1]
  if (is.na(smallest_thin)) {
    smallest_thin <- max_thin
    cli::cli_warn(
      c(
        "Could not find a thinning value that reduces mean autocorrelation \\
        below the threshold, {.val {autocorr_threshold}}.",
        "Using the maximum thinning amount: {.val {max_thin}}"
      )
    )
  }
  smallest_thin
}
