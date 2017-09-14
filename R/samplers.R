hmc <- function (dag,
                 init,
                 n_samples,
                 thin,
                 verbose,
                 pb,
                 tune = FALSE,
                 stash = FALSE,
                 control = list(Lmin = 10,
                                Lmax = 20,
                                epsilon = 0.005,
                                block_slice = TRUE)) {

  # unpack options
  Lmin <- control$Lmin
  Lmax <- control$Lmax
  epsilon <- control$epsilon

  # tuning parameters
  accept_group = 50
  target_acceptance = 0.651
  kappa = 0.75
  gamma = 0.1

  numerical_rejections <- 0

  # start the progress bar
  if (verbose)
    iterate_progress_bar(pb = pb, it = 0, rejects = 0)

  # set initial location, log joint density and gradients
  x <- init
  dag$send_parameters(x)
  grad <- dag$gradients()
  logprob <- dag$log_density()

  if (tune)
    epsilon_trace <- rep(NA, n_samples)

  # set up trace store (grab values of target variables from graph to get
  # dimension and names)
  init_trace <- dag$trace_values()
  n_target <- length(init_trace)
  trace <- matrix(NA,
                  nrow = n_samples %/% thin,
                  ncol = n_target)
  colnames(trace) <- names(init_trace)

  # if anything goes awry, stash the trace so far
  if (stash)
    on.exit(stash_trace(trace))

  # track acceptance
  accept_trace <- rep(0, n_samples)

  # get free parameter dimension
  npar <- length(x)

  accept_count <- 0

  # loop through iterations
  for (i in 1:n_samples) {

    # copy old state
    x_old <- x
    logprob_old <- logprob
    grad_old <- grad
    p <- p_old <- rnorm(npar)

    # start leapfrog steps
    reject <- FALSE
    # p <- p_old + 0.5 * epsilon * grad
    n_steps <- base::sample(Lmin:Lmax, 1)
    for (l in seq_len(n_steps)) {

      # step
      p <- p + 0.5 * epsilon * grad
      x <- x + epsilon * p

      # send parameters
      dag$send_parameters(x)
      grad <- dag$gradients()

      # check gradients are finite
      if (any(!is.finite(grad))) {
        reject <- TRUE
        break()
      }

      p <- p + 0.5 * epsilon * grad

    }

    # if the step was bad, reject it out of hand
    if (reject) {

      numerical_rejections <- numerical_rejections + 1
      x <- x_old
      logprob <- logprob_old
      grad <- grad_old

    } else {

      # otherwise do the Metropolis accept/reject step

      # inner products
      p_prod <- 0.5 * sum(p ^ 2)
      p_prod_old <- 0.5 * sum(p_old ^ 2)

      # acceptance ratio
      logprob <- dag$log_density()
      log_accept_ratio = logprob - p_prod - logprob_old + p_prod_old
      log_u = log(runif(1))

      if (log_u < log_accept_ratio) {

        # on acceptance, iterate the counter and leave the parameters in the dag
        # to be put in the trace
        accept_count <- accept_count + 1
        accept_trace[i] <- 1

      } else {

        # on rejection, reset all the parameters and push old parameters to the
        # graph for the trace
        x <- x_old
        logprob <- logprob_old
        grad <- grad_old

      }

    }

    # either way, store density and location of target parameters straight from the graph
    # reset dag parameters for extracting the trace
    if (i %% thin == 0) {
      dag$send_parameters(x)
      trace[i / thin, ] <- dag$trace_values()
    }

    if (verbose)
      iterate_progress_bar(pb = pb, it = i, rejects = numerical_rejections)

    # optionally tune epsilon
    if (tune) {

      # acceptance rate over the last accept_group runs
      start <- max(1, i - accept_group)
      end <- i
      accept_rate <- mean(accept_trace[start:end], na.rm = TRUE)

      # decrease the adaptation rate as we go
      adapt_rate <- min(1, gamma * i ^ (-kappa))

      # shift epsilon in the right direction, making sure it never goes negative
      epsilon <- epsilon + pmax(-(epsilon + sqrt(.Machine$double.eps)),
                                adapt_rate * (accept_rate - target_acceptance))

      # keep track of epsilon
      epsilon_trace[i] <- epsilon

    }

  }

  # store the tuned epsilon as the mean of the last half
  if (tune) {
    start <- floor(n_samples/2)
    end <- n_samples
    control$epsilon <- mean(epsilon_trace[start:end], na.rm = TRUE)
  }

  attr(trace, 'last_x') <- x
  attr(trace, 'control') <- control
  trace

}

slice <- function(dag,
                  init,
                  n_samples,
                  thin,
                  verbose,
                  pb,
                  tune = FALSE,
                  stash = FALSE,
                  control = list(w_size = 1.0,
                                 max_iter = 10000,
                                 slice_eps = 0.0001,
                                 block_slice = TRUE)) {
  # setup progress bar
  if (verbose) {
    greta:::iterate_progress_bar(pb = pb, it = 0, rejects = 0)
  }

  # initialise parameters
  params <- init
  variable_nodes <- dag$node_tf_names[grep("variable", dag$node_tf_names)]
  if (dag$discrete) {
    discrete_vars <- sapply(dag$node_list[names(variable_nodes)],
                            function(x)
                              x$distribution$discrete)
    discrete_names <- variable_nodes[which(discrete_vars)]
    discrete <- grepl(paste(discrete_names, collapse = "|"), names(params))
  }

  # unpack options
  w_size <- control$w_size
  max_iter <- control$max_iter
  slice_eps <- control$slice_eps

  # setup continuous sampler tuning parameters
  numerical_rejections <- 0

  # set up trace store (grab values of target variables from graph to get
  # dimension and names)
  init_trace <- dag$trace_values()
  n_target <- length(init_trace)
  trace <- matrix(NA,
                  nrow = n_samples %/% thin,
                  ncol = n_target)
  colnames(trace) <- names(init_trace)

  # if anything goes awry, stash the trace so far
  if (stash)
    on.exit(greta:::stash_trace(trace))

  # setup bounds for each variable
  var_nodes <- dag$node_list[dag$node_types == "variable"]
  var_dims <- apply(sapply(var_nodes,
                           function(x) x$dim),
                    2, prod)
  lower_bounds <- rep(sapply(var_nodes, function(x) x$sampler_lower), times = var_dims)
  upper_bounds <- rep(sapply(var_nodes, function(x) x$sampler_upper), times = var_dims)

  for (i in 1:n_samples) {

    # update all parameters using a slice sampler
    if (block_slice) {
      stop("Block slice sampler is not implemented for continuous variables",
           call. = FALSE)
    } else {
      for (j in seq_along(params)) {

        # parameter-wise slice sampler
        x0 <- params[j]
        dag$send_parameters(params)
        logy <- dag$log_density()

        # generate auxiliary variable
        logz <- logy - rexp(1)

        # generate random interval of width w_size
        L <- x0 - runif(1) * w_size
        R <- L + w_size

        # stepping out algorithm to find interval width
        L <- ifelse(L > lower_bounds[j], L, lower_bounds[j])
        params[j] <- ifelse(discrete[j], floor(L), L)
        dag$send_parameters(params)
        logt <- dag$log_density()
        while((L > lower_bounds[j]) & (logt > logz)) {
          L <- L - w_size
          L <- ifelse(L > lower_bounds[j], L, lower_bounds[j])
          params[j] <- ifelse(discrete[j], floor(L), L)
          dag$send_parameters(params)
          logt <- dag$log_density()
        }
        R <- ifelse(R < upper_bounds[j], R, upper_bounds[j])
        params[j] <- ifelse(discrete[j], floor(R), R)
        dag$send_parameters(params)
        logt <- dag$log_density()
        while((R < upper_bounds[j]) & (logt > logz)) {
          R <- R + w_size
          R <- ifelse(R < upper_bounds[j], R, upper_bounds[j])
          params[j] <- ifelse(discrete[j], floor(R), R)
          dag$send_parameters(params)
          logt <- dag$log_density()
        }

        # make sure interval is within range
        r0 <- max(L, lower_bounds[j])
        r1 <- min(R, upper_bounds[j])

        xs <- x0
        for (k in seq_len(max_iter)) {
          xs <- runif(1, r0, r1)
          params[j] <- ifelse(discrete[j], floor(xs), xs)
          dag$send_parameters(params)
          logt <- dag$log_density()
          if (logt > logz)
            break
          if (xs < x0) {
            r0 <- xs
          } else {
            r1 <- xs
          }
          if ((r1 - r0) < slice_eps) {
            xs <- r0
            break
          }
        }
        x1 <- xs
        params[j] <- ifelse(discrete[j], floor(x1), x1)
        dag$send_parameters(params)
        logy <- dag$log_density()
      }
    }

    # either way, store density and location of target parameters straight from the graph
    # reset dag parameters for extracting the trace
    if (i %% thin == 0) {
      dag$send_parameters(params)
      trace[i / thin, ] <- dag$trace_values()
    }

    if (verbose)
      greta:::iterate_progress_bar(pb = pb, it = i, rejects = numerical_rejections)

  }

  # return samples
  attr(trace, 'last_x') <- params
  attr(trace, 'control') <- control
  trace
}

hybrid <- function(dag,
                   init,
                   n_samples,
                   thin,
                   verbose,
                   pb,
                   tune = FALSE,
                   stash = FALSE,
                   control = list(Lmin = 10,
                                  Lmax = 20,
                                  w_size = 1.0,
                                  max_iter = 10000,
                                  epsilon = 0.005,
                                  slice_eps = 0.0001,
                                  block_slice = TRUE)) {
  # setup progress bar
  if (verbose) {
    greta:::iterate_progress_bar(pb = pb, it = 0, rejects = 0)
  }

  # initialise parameters
  # initialise parameters
  params <- init
  variable_nodes <- dag$node_tf_names[grep("variable", dag$node_tf_names)]
  if (dag$discrete) {
    discrete_vars <- sapply(dag$node_list[names(variable_nodes)],
                            function(x)
                              x$distribution$discrete)
    discrete_names <- variable_nodes[which(discrete_vars)]
    discrete <- grepl(paste(discrete_names, collapse = "|"), names(params))
  }

  # unpack options
  Lmin <- control$Lmin
  Lmax <- control$Lmax
  epsilon <- control$epsilon
  w_size <- control$w_size
  max_iter <- control$max_iter
  slice_eps <- control$slice_eps

  # setup continuous sampler tuning parameters
  accept_group = 50
  target_acceptance = 0.651
  kappa = 0.75
  gamma = 0.1
  numerical_rejections <- 0

  # get initial gradients
  params <- init
  dag$send_parameters(params)
  logprob <- dag$log_density()
  if (dag$discrete) {
    grad <- dag$gradients()[!discrete]
  } else {
    grad <- dag$gradients()
  }

  if (tune)
    epsilon_trace <- rep(NA, n_samples)

  # set up trace store (grab values of target variables from graph to get
  # dimension and names)
  init_trace <- dag$trace_values()
  n_target <- length(init_trace)
  trace <- matrix(NA,
                  nrow = n_samples %/% thin,
                  ncol = n_target)
  colnames(trace) <- names(init_trace)

  # track acceptance
  accept_trace <- rep(0, n_samples)

  # if anything goes awry, stash the trace so far
  if (stash)
    on.exit(greta:::stash_trace(trace))

  # setup bounds for each variable
  var_nodes <- dag$node_list[dag$node_types == "variable"]
  var_dims <- apply(sapply(var_nodes,
                           function(x) x$dim),
                    2, prod)
  lower_bounds <- rep(sapply(var_nodes, function(x) x$sampler_lower), times = var_dims)
  upper_bounds <- rep(sapply(var_nodes, function(x) x$sampler_upper), times = var_dims)

  accept_count <- 0

  # get free parameter dimension for continuous params
  npar <- ifelse(dag$discrete, sum(!discrete), length(params))

  for (i in seq_len(n_samples)) {
    # update continuous parameters
    # send parameters and get log density back
    if (dag$discrete) {
      x_old <- params[!discrete]
    } else {
      x_old <- params
    }
    logprob_old <- logprob
    grad_old <- grad
    p <- p_old <- rnorm(npar)

    # start leapfrog steps
    reject <- FALSE
    n_steps <- base::sample(Lmin:Lmax, 1)
    for (l in seq_len(n_steps)) {
      # step
      p <- p + 0.5 * epsilon * grad
      if (dag$discrete) {
        params[!discrete] <- params[!discrete] + epsilon * p
      } else {
        params <- params + epsilon * p
      }

      # send parameters
      dag$send_parameters(params)
      if (dag$discrete) {
        grad <- dag$gradients()[!discrete]
      } else {
        grad <- dag$gradients()
      }

      # check gradients are finite
      if (any(!is.finite(grad))) {
        reject <- TRUE
        break
      }
      p <- p + 0.5 * epsilon * grad
    }

    # if the step was bad, reject it out of hand
    if (reject) {
      numerical_rejections <- numerical_rejections + 1
      if (dag$discrete) {
        params[!discrete] <- x_old
      } else {
        params <- x_old
      }
      logprob <- logprob_old
      grad <- grad_old
    } else {
      # otherwise do the Metropolis accept/reject step
      # inner products
      p_prod <- 0.5 * sum(p ^ 2)
      p_prod_old <- 0.5 * sum(p_old ^ 2)

      # acceptance ratio
      logprob <- dag$log_density()
      log_accept_ratio = logprob - p_prod - logprob_old + p_prod_old
      log_u = log(runif(1))

      if (log_u < log_accept_ratio) {
        # on acceptance, iterate the counter and leave the parameters in the dag
        # to be put in the trace
        accept_count <- accept_count + 1
        accept_trace[i] <- 1
      } else {
        # on rejection, reset all the parameters and push old parameters to the
        # graph for the trace
        if (dag$discrete) {
          params[!discrete] <- x_old
        } else {
          params <- x_old
        }
        logprob <- logprob_old
        grad <- grad_old
      }
    }

    # update discrete parameters using a factor slice sampler
    if (block_slice) {

      n_dim <- sum(discrete)

      # initialise mean and covariance arrays
      state_mean <- array(0, c(n_dim, 1))
      state_sample_cov <- array(0, c(n_dim, n_dim))

      # define parameters for block slice sampler
      slice_par <- list(n_expand = array(0, c(n_dim, 1)),
                        n_shrink = array(0, c(n_dim, 1)),
                        w_size = array(w_size, c(n_dim, 1)),
                        factors = diag(n_dim),
                        n_proposal = 0,
                        n_iter_adapt_slice = 1,
                        n_iter_adapt_factor = max(floor(n_samples / 5), 1))

      for (i in 1:n_samples) {

        # run update on factors
        update <- block_slice_internal(params = params,
                                       discrete = discrete,
                                       dag = dag,
                                       slice_par = slice_par)
        params <- update$params
        slice_par <- update$slice_par

        # update mean and covariance arrays
        state_mean <- state_mean + params[discrete]
        state_sample_cov <- state_sample_cov +
          params[discrete] %*% t(params[discrete])
        slice_par$n_store <- slice_par$n_store + 1

        # optionally tune slice sampler parameters
        if (tune) {
          if ((slice_par$n_iter_adapt_slice %% i) == 0)
            slice_par <- tune_slice_par(slice_par)

          if ((slice_par$n_iter_adapt_factor %% i) == 0) {
            slice_par <- tuneFactors(slice_par, state_mean, state_sample_cov)

            # reset factor parameters
            state_mean <- array(0, c(n_dim, 1))
            state_mean <- array(0, c(n_dim, n_dim))
          }
        }
      }
    } else {
      for (j in seq_len(sum(discrete))) {

        # parameter-wise slice sampler
        x0 <- params[discrete][j]
        dag$send_parameters(params)
        logy <- dag$log_density()

        # generate auxiliary variable
        logz <- logy - rexp(1)

        # generate random interval of width w_size
        L <- x0 - runif(1) * w_size
        R <- L + w_size

        # stepping out algorithm to find interval width
        L <- ifelse(L > lower_bounds[discrete][j], L, lower_bounds[discrete][j])
        params[discrete][j] <- floor(L)
        dag$send_parameters(params)
        logt <- dag$log_density()
        while((L > lower_bounds[discrete][j]) & (logt > logz)) {
          L <- L - w_size
          L <- ifelse(L > lower_bounds[discrete][j], L, lower_bounds[discrete][j])
          params[discrete][j] <- floor(L)
          dag$send_parameters(params)
          logt <- dag$log_density()
        }
        R <- ifelse(R < upper_bounds[discrete][j], R, upper_bounds[discrete][j])
        params[discrete][j] <- floor(R)
        dag$send_parameters(params)
        logt <- dag$log_density()
        while((R < upper_bounds[discrete][j]) & (logt > logz)) {
          R <- R + w_size
          R <- ifelse(R < upper_bounds[discrete][j], R, upper_bounds[discrete][j])
          params[discrete][j] <- floor(R)
          dag$send_parameters(params)
          logt <- dag$log_density()
        }

        # make sure interval is within range
        r0 <- max(L, lower_bounds[discrete][j])
        r1 <- min(R, upper_bounds[discrete][j])

        xs <- x0
        for (k in seq_len(max_iter)) {

          xs <- runif(1, r0, r1)
          params[discrete][j] <- floor(xs)
          dag$send_parameters(params)
          logt <- dag$log_density()
          if (logt > logz)
            break
          if (xs < x0) {
            r0 <- xs
          } else {
            r1 <- xs
          }
          if ((r1 - r0) < slice_eps) {
            xs <- r0
            break
          }
        }
        x1 <- xs
        params[discrete][j] <- floor(x1)
        dag$send_parameters(params)
        logy <- dag$log_density()

      }
    }

    # either way, store density and location of target parameters straight from the graph
    # reset dag parameters for extracting the trace
    if (i %% thin == 0) {
      dag$send_parameters(params)
      trace[i / thin, ] <- dag$trace_values()
    }

    # optionally tune epsilon
    if (tune) {
      # acceptance rate over the last accept_group runs
      start <- max(1, i - accept_group)
      end <- i
      accept_rate <- mean(accept_trace[start:end], na.rm = TRUE)

      # decrease the adaptation rate as we go
      adapt_rate <- min(1, gamma * i ^ (-kappa))

      # shift epsilon in the right direction, making sure it never goes negative
      epsilon <- epsilon + pmax(-(epsilon + sqrt(.Machine$double.eps)),
                                adapt_rate * (accept_rate - target_acceptance))

      # keep track of epsilon
      epsilon_trace[i] <- epsilon
    }

    if (verbose)
      greta:::iterate_progress_bar(pb = pb, it = i, rejects = numerical_rejections)

  }

  # store the tuned epsilon as the mean of the last half
  if (tune) {
    start <- floor(n_samples / 2)
    end <- n_samples
    control$epsilon <- mean(epsilon_trace[start:end], na.rm = TRUE)
  }

  # return samples
  attr(trace, 'last_x') <- params
  attr(trace, 'control') <- control
  trace
}

# factor slice sampler
block_slice_internal <- function(params, discrete, dag, slice_par) {

  state <- params[discrete]
  for (i in seq_along(state)) {
    width <- slice_par$w_size[i]
    factor <- slice_par$factors[, i]

    # sample random slice
    dag$send_parameters(params)
    logy <- dag$log_density()
    logt <- logy - rexp(1) # height

    # approximate slice width
    lower <- -1.0 * w_size * runif(1)
    upper <- lower + w_size;

    # step out to increase slice width
    x0 <- state + lower * factor
    params[discrete] <- floor(x0)
    dag$send_parameters(params)
    logz <- dag$log_density()
    while (logt < logy) {
      slice_par$n_expand[i] <- slice_par$n_expand[i] + 1
      lower <- lower - w_size
      x0 <- state + lower * factor
      params[discrete] <- floor(x0)
      dag$send_parameters(params)
      logz <- dag$log_density()
    }

    x0 <- state + upper * factor
    params[discrete] <- floor(x0)
    dag$send_parameters(params)
    logz <- dag$log_density()
    while (logt < logz) {
      slice_par$n_expand[i] <- slice_par$n_expand[i] + 1
      upper <- upper + w_size
      x0 <- state + upper * factor
      params[discrete] <- floor(x0)
      dag$send_parameters(params)
      logz <- dag$log_density()
    }

    # update state using estimated slice
    xs <- lower + runif(1) * (upper - lower)

    # break is xs is in slice
    x0 <- state + xs * factor
    params[discrete] <- floor(x0)
    dag$send_parameters(params)
    logz <- dag$log_density()
    if (logt < logz) {
      state <- state + xs * factor
      params[discrete] <- floor(state)
      break
    }

    # set state to current state if rejected
    params[discrete] <- floor(state)

    # shrink interval if proposal rejected
    slice_par$n_shrink[i] <- slice_par$n_shrink[i] + 1

    lower <- ifelse(xs < 0, xs, lower)
    upper <- ifelse(xs > 0, xs, upper)
  }

  out <- list(params = params, slice_par = slice_par)
  out
}

# tune factors for factor slice sampler
tune_factor <- function(slice_par, sample_mean, sample_cov) {
  sample_cov_norm <- (1.0 / (slice_par$n_store - 1)) *
    (sample_cov - sample_mean %*% t(sample_mean) * (1.0 / slice_par$n_store))

  # update factors using eigenvectors of sample covariance
  slice_par$factors <- eigen(sample_cov_norm)$vectors

  #  reset counters
  slice_par$n_expand <- array(0, c(length(sample_mean), 1))
  slice_par$n_shrink <- array(0, c(length(sample_mean), 1))

  slice_par$n_iter <- 1
  slice_par$n_store <- 0

  slice_par
}

# tune slice width for factor slice sampler
tune_slice_par <- function (slice_par, target = 0.5) {

  for (i in seq_along(slice_par$n_expand)) {
    denom <- slice_par$n_expand[i] + slice_par$n_shrink[i]

    if (denom > 0) {
      ratio <- slice_par$n_expand[i] / denom

      if (ratio == 0.0)
        ratio <- 1.0 / denom

      multiplier <- (ratio / target)

      # modify slice width
      slice_par$w_size[i] <-
        slice_par$w_size[i] * multiplier
    }
  }

  # reset counters
  n_dim <- dim(slice_par$n_expand)[1]
  slice_par$n_expand <- array(0, c(n_dim, 1))
  slice_par$n_shrink <- array(0, c(n_dim, 1))

  # increase the time to next adaptation
  slice_par$n_iter <- slice_par$n_iter * 2

  slice_par
}

samplers_module <- module(hmc, slice, hybrid)
