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

      # use log representation if available
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

      # ensure the support and bounds of each of the distributions is the same
      truncations <- lapply(distribs, member, "truncation")
      bounds <- lapply(distribs, member, "bounds")

      truncated <- !vapply(truncations, is.null, logical(1))
      supports <- bounds
      supports[truncated] <- truncations[truncated]

      n_supports <- length(unique(supports))
      if (n_supports != 1) {
        supports_text <- vapply(
          X = unique(supports),
          FUN = paste,
          collapse = " to ",
          FUN.VALUE = character(1)
        )
        stop("component distributions have different support: ",
              paste(supports_text, collapse = " vs. "))
      }

      # get the maximal bounds for all component distributions
      bounds <- c(do.call(min, bounds),
                 do.call(max, bounds))

      # if the support is smaller than this, treat the distribution as truncated
      support <- supports[[1]]
      if (identical(support, bounds)) {
        truncation <- NULL
      } else {
        truncation <- support
      }

      # for any discrete ones, tell them they are fixed
      super$initialize("mixture", dim, discrete = discrete[1],
                       truncation = truncation)

      self$bounds <- bounds

      for (i in seq_len(n_distributions)) {
        self$add_parameter(distribs[[i]],
                           paste("distribution", i),
                           expand_scalar_to = NULL)
      }

      self$add_parameter(weights, "weights")
    },

    tf_distrib = function(parameters, dag) {

      # use log weights - no need to normalise then
      if (self$weights_is_log) {
        log_weights <- parameters$weights
      } else {
        log_weights <- tf$math$log(parameters$weights)
      }

      # get parameter *nodes*, truncations, and bounds of component distributions
      distribution_nodes <- self$parameters[names(self$parameters) != "weights"]
      truncations <- lapply(distribution_nodes, member, "truncation")
      bounds <- lapply(distribution_nodes, member, "bounds")
      distribution_parameters <-
        lapply(distribution_nodes, member, "parameters")

      # 'parameters' is a list of functions to construct tfp distributions, so
      # evaluate them on their own parameters to get the distribution objects
      constructors <- parameters[names(parameters) != "weights"]

      tfp_distributions <- list()

      for (i in seq_along(constructors)) {

        # get the tensors for the parameters of this component distribution
        tf_parameter_list <-
          lapply(distribution_parameters[[i]], dag$get_tf_object)

        # match the batch dimension with weights
        tf_parameter_list <- match_batches(
          c(list(log_weights), tf_parameter_list)
        )[-1]

        # use them to construct the tfp distribution object
        tfp_distributions[[i]] <- constructors[[i]](
          parameters = tf_parameter_list,
          dag = dag
        )

      }

      # match batches on log_weights too
      log_weights <- match_batches(
        list(log_weights, tf_parameter_list[[1]])
      )[[1]]

      # There's no dispatching on the dimensions of the categorical and
      # component distributions, so we need to explicitly expand weights to
      # match the dimensions of the components, but with the number of
      # components in an additional final dimension

      # transpose weights so the number of components (second dim) is last,
      # rather than first
      weights_dim <- dim(log_weights)
      permutation <- seq_along(weights_dim) - 1L
      permutation <- c(permutation[-2], permutation[2])
      log_weights <- tf$transpose(log_weights, permutation)

      # pad the log weights to have one more dimensions than the components, but
      # with the batch dimension first, the number of components last, and the
      # intermediate ones padded with ones
      component_dim <- tfp_distributions[[1]]$batch_shape$as_list()
      weights_dim <- dim(log_weights)
      dims_needed <- length(component_dim) + 1 - length(weights_dim)
      for (i in seq_len(dims_needed)) {
        log_weights <- tf$expand_dims(log_weights, 1L)
      }

      # If necessary, tile this to match the dimensions of the component
      # distributions
      expandable <- vapply(
        X = dim(log_weights),
        FUN = identical,
        y = 1L,
        FUN.VALUE = logical(1)
      )
      expansion <- c(1L, component_dim[-1], 1L)
      expansion[!expandable] <- 1L

      if(!all(expansion == 1L)) {
        log_weights <- tf$tile(log_weights, expansion)
      }

      # build a tfp categorical distribution for the weights
      cat <- tfp$distributions$Categorical(logits = log_weights)

      # build a tfp mixture distribution
      tfp$distributions$Mixture(
        cat = cat,
        components = tfp_distributions
      )

    }

  )
)

mixture_module <- module(mixture_distribution = mixture_distribution)
