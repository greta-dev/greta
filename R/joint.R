#' @name joint
#' @title define joint distributions
#'
#' @description \code{joint} combines univariate probability distributions
#'   together into a multivariate (and \emph{a priori} independent between
#'   dimensions) joint distribution, either over a variable, or for fixed data.
#'
#' @param ... variable greta arrays following probability distributions (see
#'   \code{\link{distributions}}); the components of the joint distribution.
#'
#' @param dim the dimensions of the greta array to be returned, either a scalar
#'   or a vector of positive integers. The final dimension of the greta array
#'   returned will be determined by the number of component distributions
#'
#' @details The component probability distributions must all be either
#'   continuous or discrete, and must have the same dimensions.
#'
#'   This functionality is unlikely to be useful in most models, since the same
#'   result can usually be achieved by combining variables with separate
#'   distributions. It is included for situations where it is more convenient to
#'   consider these as a single distribution, e.g. for use with
#'   \code{distribution} or \code{mixture}.
#'
#' @export
#' @examples
#' \dontrun{
#' # an uncorrelated bivariate normal
#' x <- joint(normal(-3, 0.5), normal(3, 0.5))
#' m <- model(x)
#' plot(mcmc(m, n_samples = 500))
#'
#' # joint distributions can be used to define densities over data
#' x <- cbind(rnorm(10, 2, 0.5), rbeta(10, 3, 3))
#' mu <- normal(0, 10)
#' sd <- normal(0, 3, truncation = c(0, Inf))
#' a <- normal(0, 3, truncation = c(0, Inf))
#' b <- normal(0, 3, truncation = c(0, Inf))
#' distribution(x) <- joint(normal(mu, sd), beta(a, b),
#'                          dim = 10)
#' # # this is equivalent to:
#' # distribution(x[, 1]) <- normal(mu, sd)
#' # distribution(x[, 2]) <- beta(a, b)
#' m <- model(mu, sd, a, b)
#' plot(mcmc(m))
#' }
joint <- function(..., dim = NULL)
  distrib("joint", list(...), dim)

joint_distribution <- R6Class(
  "joint_distribution",
  inherit = distribution_node,
  public = list(

    initialize = function(dots, dim) {

      n_distributions <- length(dots)

      if (n_distributions < 2) {
        stop("joint must be passed at least two distributions",
             call. = FALSE)
      }

      # check the dimensions of the variables in dots
      dim <- do.call(check_dims, c(dots, target_dim = dim))

      # add the joint dimension as the last dimension
      ndim <- length(dim)
      if (dim[ndim] == 1) {
        dim[ndim] <- n_distributions
      } else {
        dim <- c(dim, n_distributions)
      }

      dot_nodes <- lapply(dots, get_node)

      # get the distributions and strip away their variables
      distribs <- lapply(dot_nodes, member, "distribution")
      lapply(distribs, function(x) x$remove_target())

      # also strip the distributions from the variables
      lapply(dot_nodes, function(x) x$distribution <- NULL)

      # check the distributions are all either discrete or continuous
      discrete <- vapply(distribs, member, "discrete", FUN.VALUE = FALSE)

      if (!all(discrete) & !all(!discrete)) {
        stop("cannot construct a joint distribution from a combination of discrete ",
             "and continuous distributions",
             call. = FALSE)
      }

      # for any discrete ones, tell them they are fixed

      super$initialize("joint", dim, discrete = discrete[1])

      for (i in seq_len(n_distributions)) {
        self$add_parameter(distribs[[i]],
                           paste("distribution", i))
      }

    },

    tf_distrib = function(parameters, dag) {

      densities <- parameters
      names(densities) <- NULL

      log_prob <- function(x) {

        # split x on the joint dimension, and loop through computing the densities
        last_dim <- length(dim(x)) - 1L
        x_vals <- tf$split(x, length(densities), axis = last_dim)
        log_probs <- list()
        for (i in seq_along(densities)) {
          log_probs[[i]] <- densities[[i]](x_vals[[i]])
        }

        # sum them elementwise
        tf$add_n(log_probs)

      }

      list(log_prob = log_prob, cdf = NULL, log_cdf = NULL)

    },

    tf_cdf_function = NULL,
    tf_log_cdf_function = NULL

  )
)

joint_module <- module(joint_distribution = joint_distribution)
