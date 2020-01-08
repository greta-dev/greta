#' @name mixture
#' @title mixtures of probability distributions
#'
#' @description \code{mixture} combines other probability distributions into a
#'   single mixture distribution, either over a variable, or for fixed data.
#'
#' @param ... variable greta arrays following probability distributions (see
#'   \code{\link{distributions}}); the component distributions in a mixture
#'   distribution.
#'
#' @param weights a column vector or array of mixture weights, which must be
#'   positive, but need not sum to one. The first dimension must be the number
#'   of distributions, the remaining dimensions must either be 1 or match the
#'   distribution dimension.
#'
#' @param dim the dimensions of the greta array to be returned, either a scalar
#'   or a vector of positive integers.
#'
#' @details The \code{weights} are rescaled to sum to one along the first
#'   dimension, and are then used as the mixing weights of the distribution.
#'   I.e. the probability density is calculated as a weighted sum of the
#'   component probability distributions passed in via \code{\dots}
#'
#'   The component probability distributions must all be either continuous or
#'   discrete, and must have the same dimensions.
#'
#' @export
#' @examples
#' \dontrun{
#' # a scalar variable following a strange bimodal distibution
#' weights <- uniform(0, 1, dim = 3)
#' a <- mixture(normal(-3, 0.5),
#'              normal(3, 0.5),
#'              normal(0, 3),
#'              weights = weights)
#' m <- model(a)
#' plot(mcmc(m, n_samples = 500))
#'
#' # simulate a mixture of poisson random variables and try to recover the
#' # parameters with a Bayesian model
#' x <- c(rpois(800, 3),
#'        rpois(200, 10))
#'
#' weights <- uniform(0, 1, dim = 2)
#' rates <- normal(0, 10, truncation = c(0, Inf), dim = 2)
#' distribution(x) <- mixture(poisson(rates[1]),
#'                            poisson(rates[2]),
#'                            weights = weights)
#' m <- model(rates)
#' draws_rates <- mcmc(m, n_samples = 500)
#'
#' # check the mixing probabilities after fitting using calculate()
#' # (you could also do this within the model)
#' normalized_weights <- weights / sum(weights)
#' draws_weights <- calculate(normalized_weights, draws_rates)
#'
#' # get the posterior means
#' summary(draws_rates)$statistics[, "Mean"]
#' summary(draws_weights)$statistics[, "Mean"]
#'
#' # weights can also be an array, giving different mixing weights
#' # for each observation (first dimension must be number of components)
#' dim <- c(5, 4)
#' weights <- uniform(0, 1, dim = c(2, dim))
#' b <- mixture(normal(1, 1, dim = dim),
#'              normal(-1, 1, dim = dim),
#'              weights = weights)
#' }
mixture <- function(..., weights, dim = NULL)
  distrib("mixture", list(...), weights, dim)

mixture_distribution <- R6Class(
  "mixture_distribution",
  inherit = distribution_node,
  public = list(

    weights_is_log = FALSE,

    initialize = function(dots, weights, dim) {

      n_distributions <- length(dots)

      if (n_distributions < 2) {
        stop("mixture must be passed at least two distributions",
             call. = FALSE)
      }

      # check the dimensions of the variables in dots
      dim <- do.call(check_dims, c(dots, target_dim = dim))

      weights <- as.greta_array(weights)
      weights_dim <- dim(weights)

      # use log weights if available
      if (has_representation(weights, "log")) {
        weights <- representation(weights, "log")
        self$weights_is_log <- TRUE
      }

      # weights should have n_distributions as the first dimension
      if (weights_dim[1] != n_distributions) {
        stop("the first dimension of weights must be the number of ",
             "distributions in the mixture (", n_distributions, "), ",
             "but was ", weights_dim[1],
             call. = FALSE)
      }

      # drop a trailing 1 from dim, so user doesn't need to deal with it
      # Ugh, need to get rid of column vector thing soon.
      weights_extra_dim <- dim
      n_extra_dim <- length(weights_extra_dim)
      if (weights_extra_dim[n_extra_dim] == 1) {
        weights_extra_dim <- weights_extra_dim[-n_extra_dim]
      }

      # remainder should be 1 or match weights_extra_dim
      w_dim <- weights_dim[-1]
      dim_1 <- length(w_dim) == 1 && w_dim == 1
      dim_same <- all(w_dim == weights_extra_dim)
      if (!(dim_1 | dim_same)) {
        stop("the dimension of weights must be either ", n_distributions,
             " x 1 or ", n_distributions, " x ", paste(dim, collapse = " x "),
             " but was ", paste(weights_dim, collapse = " x "),
             call. = FALSE)
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
        stop("cannot construct a mixture from a combination of discrete ",
             "and continuous distributions",
             call. = FALSE)
      }

      # for any discrete ones, tell them they are fixed
      super$initialize("mixture", dim, discrete = discrete[1])

      for (i in seq_len(n_distributions)) {
        self$add_parameter(distribs[[i]],
                           paste("distribution", i))
      }

      self$add_parameter(weights, "weights")
    },

    tf_distrib = function(parameters, dag) {

      densities <- parameters[names(parameters) != "weights"]
      names(densities) <- NULL
      weights <- parameters$weights

      # use log weights if available
      if (self$weights_is_log) {
        log_weights <- weights
      } else {
        log_weights <- tf$math$log(weights)
      }

      # normalise weights on log scale
      log_weights_sum <- tf$reduce_logsumexp(
        log_weights,
        axis = 1L,
        keepdims = TRUE
      )
      log_weights <- log_weights - log_weights_sum

      log_prob <- function(x) {

        # get component densities in an array
        log_probs <- lapply(densities, do.call, list(x))
        log_probs_arr <- tf$stack(log_probs, 1L)

        # massage log_weights into the same shape as log_probs_arr
        dim_weights <- dim(log_weights)
        extra_dims <- unlist(dim(log_probs_arr)[-seq_along(dim_weights)])
        for (dim in extra_dims) {
          ndim <- length(dim(log_weights))
          log_weights <- tf$expand_dims(log_weights, ndim)
          if (dim > 1L) {
            tiling <- c(rep(1L, ndim), dim)
            tf_tiling <- tf$constant(tiling, shape = list(ndim + 1))
            log_weights <- tf$tile(log_weights, tf_tiling)
          }
        }

        # do elementwise addition, then collapse along the mixture dimension
        log_probs_weighted_arr <- log_probs_arr + log_weights
        tf$reduce_logsumexp(log_probs_weighted_arr, axis = 1L)
      }

      list(log_prob = log_prob, cdf = NULL, log_cdf = NULL)

    },

    tf_cdf_function = NULL,
    tf_log_cdf_function = NULL

  )
)

mixture_module <- module(mixture_distribution = mixture_distribution)
