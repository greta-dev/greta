#' @name samplers
#'
#' @title MCMC samplers
#' @description Functions to set up MCMC samplers and change the startng values
#'   of their parameters, for use in \code{\link{mcmc}()}.
#'
#' @details During the warmup iterations of \code{mcmc}, some of these
#'   sampler parameters will be tuned to improve the efficiency of the sampler,
#'   so the values provided here are used as starting values.
#'
#' @return a \code{sampler} object that can be passed to \code{\link{mcmc}}.

NULL

#' @rdname samplers
#' @export
#'
#' @param Lmin minimum number of leapfrog steps (positive integer, Lmin > Lmax)
#' @param Lmax maximum number of leapfrog steps (positive integer, Lmax > Lmin)
#' @param epsilon leapfrog stepsize hyperparameter (positive, will be tuned)
#' @param diag_sd estimate of the posterior marginal standard deviations
#'   (positive, will be tuned).
#'
#' @details For \code{hmc()}, the number of leapfrog steps at each iteration is
#'   selected uniformly at random from between \code{Lmin} and \code{Lmax}.
#'   \code{diag_sd} is used to rescale the parameter space to make it more
#'   uniform, and make sampling more efficient.
hmc <- function (Lmin = 10,
                 Lmax = 20,
                 epsilon = 0.005,
                 diag_sd = 1) {

  obj <- list(parameters = list(Lmin = Lmin,
                                Lmax = Lmax,
                                epsilon = epsilon,
                                diag_sd = diag_sd),
              class = hmc_sampler)
  class(obj) <- c("hmc sampler", "sampler")
  obj

}

#' @noRd
#' @export
print.sampler <- function (x, ...) {

  values_text <- paste(names(x$parameters),
                           prettyNum(x$parameters),
                           sep = " = ",
                           collapse = ", ")

  parameters_text <- sprintf("parameters:\n  %s",
                             values_text)

  msg <- sprintf("%s object with %s",
                 class(x)[1],
                 parameters_text)

  cat(msg)
}



# if (tune) {
#
#   # when to start and stop each type of tuning, in fractions of the tuning period
#   epsilon_periods <- list(c(0, 0.1), c(0.4, 1))
#   diag_sd_periods <- list(c(0.1, 0.4))
#
#   # epsilon tuning parameters
#   accept_group <- 50
#   target_acceptance <- 0.651
#   kappa <- 0.75
#   gamma <- 0.1
#
#   # initialise welford accumulator for marginal variance
#   diag_sd_update_rate <- 5
#   welford_m <- 0
#   welford_m2 <- 0
#
#   epsilon_trace <- rep(NA, n_samples)
#
# }
# optionally tune epsilon
# if (tune) {
#
#   adapt_epsilon <- in_periods(i, n_samples, epsilon_periods)
#   if (adapt_epsilon) {
#
#     # acceptance rate over the last accept_group runs
#     start <- max(1, i - accept_group)
#     end <- i
#     accept_rate <- mean(accept_trace[start:end], na.rm = TRUE)
#
#     # decrease the adaptation rate as we go
#     adapt_rate <- min(1, gamma * i ^ (-kappa))
#
#     # shift epsilon in the right direction, making sure it never goes negative
#     epsilon <- epsilon + pmax(-(epsilon + sqrt(.Machine$double.eps)),
#                               adapt_rate * (accept_rate - target_acceptance))
#
#     # keep track of epsilon
#     epsilon_trace[i] <- epsilon
#
#   }
#
#   # only adapt diag_sd in the first third of tuning, so that epsilon can
#   # settle in during the second half
#   adapt_diag_sd <- in_periods(i, n_samples, diag_sd_periods)
#   if (adapt_diag_sd) {
#
#     # update welford accumulator for posterior variance
#     n_accepted <- sum(accept_trace)
#
#     # update only if this step was accepted
#     if (accept_trace[i] == 1) {
#       welford_delta <- x - welford_m
#       welford_m <- welford_m + welford_delta / n_accepted
#       welford_m2 <- welford_m2 + welford_delta * (x - welford_m)
#     }
#
#     # if there are samples, and we want to adapt, get the sample posterior
#     # variance and shrink it
#     if (n_accepted > 1 & (i %% diag_sd_update_rate == 0)) {
#       sample_var <- welford_m2 / (n_accepted - 1)
#       var_shrinkage <- 1 / (n_accepted + 5)
#       var_shrunk <- n_accepted * var_shrinkage * sample_var + 5e-3 * var_shrinkage
#       diag_sd <- sqrt(var_shrunk)
#     }
#
#   }
#
# }
# # store the tuned epsilon as the mean of the last half
# if (tune) {
#   start <- floor(n_samples/2)
#   end <- n_samples
#   control$epsilon <- mean(epsilon_trace[start:end], na.rm = TRUE)
#   control$diag_sd <- diag_sd
# }
