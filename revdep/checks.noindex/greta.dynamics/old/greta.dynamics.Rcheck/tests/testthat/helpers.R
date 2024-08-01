# test functions

library(tensorflow)

# set the seed before running tests
set.seed(2017 - 05 - 01)

randu <- function(...) {
  dim <- c(...)
  array(runif(prod(dim)), dim = dim)
}

# R versions of dynamics module methods
r_iterate_matrix <- function(matrix, state, niter = 100, tol = 1e-6) {
  states <- list(state)

  i <- 0L
  diff <- Inf

  while (i < niter & diff > tol) {
    i <- i + 1
    states[[i + 1]] <- matrix %*% states[[i]]
    growth <- states[[i + 1]] / states[[i]]
    diffs <- growth - mean(growth)
    diff <- max(abs(diffs))
  }

  lambda <- states[[i]][1] / states[[i - 1]][1]
  stable_distribution <- states[[i]]
  stable_distribution <- stable_distribution / sum(stable_distribution)
  all_states <- matrix(0, ncol(matrix), niter)
  states_keep <- states[-1]
  all_states[, seq_along(states_keep)] <- t(do.call(rbind, states_keep))

  list(
    lambda = lambda,
    stable_distribution = stable_distribution,
    all_states = all_states,
    converged = diff < tol,
    max_iter = i
  )
}

# a midpoint solver for use in deSolve, from the vignette p8
rk_midpoint <- deSolve::rkMethod(
  ID = "midpoint",
  varstep = FALSE,
  A = c(0, 1 / 2),
  b1 = c(0, 1),
  c = c(0, 1 / 2),
  stage = 2,
  Qerr = 1
)
