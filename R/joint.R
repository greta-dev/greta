#' @name joint
#' @title define joint distributions
#'
#' @description `joint` combines univariate probability distributions
#'   together into a multivariate (and *a priori* independent between
#'   dimensions) joint distribution, either over a variable, or for fixed data.
#'
#' @param ... scalar variable greta arrays following probability distributions
#'   (see [distributions()]); the components of the joint
#'   distribution.
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
#'   `distribution` or `mixture`.
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
#'   dim = 10
#' )
#' m <- model(mu, sd, a, b)
#' plot(mcmc(m))
#' }
joint <- function(..., dim = NULL) {
  distrib("joint", list(...), dim)
}

joint_distribution <- R6Class(
  "joint_distribution",
  inherit = distribution_node,
  public = list(
    initialize = function(dots, dim) {
      n_distributions <- length(dots)

      check_num_distributions(n_distributions, at_least = 2, name = "joint")

      # check the dimensions of the variables in dots
      single_dim <- do.call(check_dims, c(dots, target_dim = dim))

      # add the joint dimension as the last dimension
      dim <- single_dim
      ndim <- length(dim)
      last_dim_1d <- dim[ndim] == 1
      if (last_dim_1d) {
        dim[ndim] <- n_distributions
      } else {
        dim <- c(dim, n_distributions)
      }

      dot_nodes <- lapply(dots, get_node)

      check_dot_nodes_scalar(dot_nodes)

      # get the distributions and strip away their variables
      distribs <- lapply(dot_nodes, member, "distribution")
      lapply(distribs, function(x) x$remove_target())

      # also strip the distributions from the variables
      lapply(dot_nodes, function(x) x$distribution <- NULL)

      # check the distributions are all either discrete or continuous
      discrete <- vapply(distribs, member, "discrete", FUN.VALUE = FALSE)

      check_not_discrete_continuous(discrete, "joint")

      n_components <- length(dot_nodes)

      # work out the support of the resulting distribution, and add as the
      # bounds of this one, to use when creating target variable
      lower <- lapply(dot_nodes, member, "lower")
      lower <- lapply(lower, array, dim = single_dim)
      upper <- lapply(dot_nodes, member, "upper")
      upper <- lapply(upper, array, dim = single_dim)

      self$bounds <- list(
        lower = do.call(abind::abind, lower),
        upper = do.call(abind::abind, upper)
      )

      super$initialize("joint", dim, discrete = discrete[1])

      for (i in seq_len(n_distributions)) {
        self$add_parameter(distribs[[i]],
          glue::glue("distribution {i}"),
          shape_matches_output = FALSE
        )
      }
    },
    create_target = function(truncation) {
      vble(self$bounds, dim = self$dim)
    },
    tf_distrib = function(parameters, dag) {

      # get information from the *nodes* for component distributions, not the tf
      # objects passed in here

      # get tfp distributions, truncations, & bounds of component distributions
      distribution_nodes <- self$parameters
      truncations <- lapply(distribution_nodes, member, "truncation")
      bounds <- lapply(distribution_nodes, member, "bounds")
      tfp_distributions <- lapply(distribution_nodes, dag$get_tfp_distribution)
      names(tfp_distributions) <- NULL

      log_prob <- function(x) {

        # split x on the joint dimension, and loop through computing the
        # densities
        last_dim <- n_dim(x) - 1L
        x_vals <- tf$split(x, length(tfp_distributions), axis = last_dim)

        log_probs <- mapply(
          dag$tf_evaluate_density,
          tfp_distributions,
          x_vals,
          truncations,
          bounds,
          SIMPLIFY = FALSE
        )

        # sum them elementwise
        tf$add_n(log_probs)
      }

      sample <- function(seed) {
        samples <- lapply(distribution_nodes, dag$draw_sample)
        names(samples) <- NULL
        tf$concat(samples, axis = 2L)
      }

      list(log_prob = log_prob, sample = sample)
    }
  )
)

joint_module <- module(joint_distribution = joint_distribution)
