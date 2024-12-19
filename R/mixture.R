#' @name mixture
#' @title mixtures of probability distributions
#'
#' @description `mixture` combines other probability distributions into a
#'   single mixture distribution, either over a variable, or for fixed data.
#'
#' @param ... variable greta arrays following probability distributions (see
#'   [distributions()]); the component distributions in a mixture
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
#' @details The `weights` are rescaled to sum to one along the first
#'   dimension, and are then used as the mixing weights of the distribution.
#'   I.e. the probability density is calculated as a weighted sum of the
#'   component probability distributions passed in via `\dots`
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
#'   normal(3, 0.5),
#'   normal(0, 3),
#'   weights = weights
#' )
#' m <- model(a)
#' plot(mcmc(m, n_samples = 500))
#'
#' # simulate a mixture of poisson random variables and try to recover the
#' # parameters with a Bayesian model
#' x <- c(
#'   rpois(800, 3),
#'   rpois(200, 10)
#' )
#'
#' weights <- uniform(0, 1, dim = 2)
#' rates <- normal(0, 10, truncation = c(0, Inf), dim = 2)
#' distribution(x) <- mixture(poisson(rates[1]),
#'   poisson(rates[2]),
#'   weights = weights
#' )
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
#'   normal(-1, 1, dim = dim),
#'   weights = weights
#' )
#' }
mixture <- function(..., weights, dim = NULL) {
  distrib("mixture", list(...), weights, dim)
}

mixture_distribution <- R6Class(
  "mixture_distribution",
  inherit = distribution_node,
  public = list(
    weights_is_log = FALSE,
    initialize = function(dots, weights, dim) {
      n_distributions <- length(dots)

      check_num_distributions(n_distributions, at_least = 2, name = "mixture")

      # check the dimensions of the variables in dots
      dim <- do.call(check_dims, c(dots, target_dim = dim))

      weights <- as.greta_array(weights)
      weights_dim <- dim(weights)

      # use log weights if available
      if (has_representation(weights, "log")) {
        weights <- representation(weights, "log")
        self$weights_is_log <- TRUE
      }

      # drop a trailing 1 from dim, so user doesn't need to deal with it
      # Ugh, need to get rid of column vector thing soon.
      # TODO get rid of column vector thing?
      check_weights_dim(weights_dim, dim, n_distributions)

      dot_nodes <- lapply(dots, get_node)

      # get the distributions and strip away their variables
      distribs <- lapply(dot_nodes, member, "distribution")
      lapply(distribs, function(x) x$remove_target())

      # also strip the distributions from the variables
      lapply(dot_nodes, function(x) x$distribution <- NULL)

      # check the distributions are all either discrete or continuous
      discrete <- vapply(distribs, member, "discrete", FUN.VALUE = logical(1))

      check_not_discrete_continuous(discrete, name = "mixture")

      # check the distributions are all either multivariate or univariate
      multivariate <- vapply(distribs,
        member,
        "multivariate",
        FUN.VALUE = logical(1)
      )

      check_not_multivariate_univariate(multivariate)

      # ensure the support and bounds of each of the distributions is the same
      truncations <- lapply(distribs, member, "truncation")
      bounds <- lapply(distribs, member, "bounds")

      truncated <- !are_null(truncations)
      supports <- bounds
      supports[truncated] <- truncations[truncated]

      check_distribution_support(supports)

      # get the maximal bounds for all component distributions
      bounds <- c(
        do.call(min, bounds),
        do.call(max, bounds)
      )

      # if the support is smaller than this, treat the distribution as truncated
      support <- supports[[1]]
      if (identical(support, bounds)) {
        truncation <- NULL
      } else {
        truncation <- support
      }

      self$bounds <- support

      # for any discrete ones, tell them they are fixed
      super$initialize("mixture",
        dim,
        discrete = discrete[1],
        multivariate = multivariate[1]
      )

      for (i in seq_len(n_distributions)) {
        self$add_parameter(distribs[[i]],
          glue::glue("distribution {i}"),
          shape_matches_output = FALSE
        )
      }

      self$add_parameter(weights, "weights")
    },
    create_target = function(truncation) {
      vble(self$bounds, dim = self$dim)
    },
    tf_distrib = function(parameters, dag) {

      # get information from the *nodes* for component distributions, not the tf
      # objects passed in here

      # get tfp distributions, truncations, & bounds of component distributions
      distribution_nodes <- self$parameters[names(self$parameters) != "weights"]
      truncations <- lapply(distribution_nodes, member, "truncation")
      bounds <- lapply(distribution_nodes, member, "bounds")
      tfp_distributions <- lapply(distribution_nodes, dag$get_tfp_distribution)
      names(tfp_distributions) <- NULL

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
        log_probs <- mapply(
          dag$tf_evaluate_density,
          tfp_distribution = tfp_distributions,
          truncation = truncations,
          bounds = bounds,
          MoreArgs = list(tf_target = x),
          SIMPLIFY = FALSE
        )
        log_probs_arr <- tf$stack(log_probs, 1L)

        # massage log_weights into the same shape as log_probs_arr
        dim_weights <- dim(log_weights)
        extra_dims <- unlist(dim(log_probs_arr)[-seq_along(dim_weights)])
        for (dim in extra_dims) {
          ndim <- n_dim(log_weights)
          log_weights <- tf$expand_dims(log_weights, ndim)
          if (dim > 1L) {
            tiling <- c(rep(1L, ndim), dim)
            tf_tiling <- tf$constant(tiling, shape = list(as.integer(ndim + 1)))
            log_weights <- tf$tile(log_weights, tf_tiling)
          }
        }

        # do elementwise addition, then collapse along the mixture dimension
        log_probs_weighted_arr <- log_probs_arr + log_weights
        tf$reduce_logsumexp(log_probs_weighted_arr, axis = 1L)
      }

      sample <- function(seed) {

        # draw samples from each component
        samples <- lapply(distribution_nodes, dag$draw_sample)
        names(samples) <- NULL

        ndim <- length(self$dim)

        # in univariate case, tile log_weights to match dim, so each element can
        # be selected independently (otherwise each row is kept together)
        log_weights <- tf$squeeze(log_weights, 2L)

        if (!self$multivariate) {
          for (i in seq_len(ndim)) {
            log_weights <- tf$expand_dims(log_weights, 1L)
          }
          log_weights <- tf$tile(log_weights, c(1L, self$dim, 1L))
        }

        # for each observation, select a random component to sample from
        cat <- tfp$distributions$Categorical(logits = log_weights)
        indices <- cat$sample(seed = seed)

        # how many dimensions to consider a batch differs beetween multivariate
        # and univariate
        collapse_axis <- ndim + 1L
        n_batches <- ifelse(self$multivariate, 1L, collapse_axis)

        # combine the random components on an extra last dimension
        samples_padded <- lapply(samples, tf$expand_dims, axis = collapse_axis)
        samples_array <- tf$concat(samples_padded, axis = collapse_axis)

        # extract the relevant component
        indices <- tf$expand_dims(indices, n_batches)
        draws <- tf$gather(samples_array,
          indices,
          axis = collapse_axis,
          batch_dims = n_batches
        )
        draws <- tf$squeeze(draws, collapse_axis)

        draws
      }

      list(log_prob = log_prob, sample = sample)
    }
  )
)

mixture_module <- module(mixture_distribution = mixture_distribution)
