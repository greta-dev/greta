#' @name samplers
#'
#' @title MCMC samplers
#' @description Functions to set up MCMC samplers and change the starting values
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
hmc <- function (Lmin = 5,
                 Lmax = 10,
                 epsilon = 0.1,
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
