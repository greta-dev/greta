# marginaliser constructors

#' @name marginalisers
#'
#' @title marginalisation methods
#' @description Functions to set up marginalisers (which explicitly integrate
#'   out random variables) and modify their behaviour, for use in
#'   \code{\link{marginalise}()}.
#'
#' @return a \code{marginaliser} object that can be passed to
#'   \code{marginalise}.
NULL

#' @rdname marginalisers
#' @export
#'
#' @param values an R vector giving values at which to evaluate the function for a
#'   discrete marginalisation
#'
#' @details \code{discrete_marginalisation} can only be used with discrete
#'   probability distributions, e.g. those defined with \code{poisson()} and
#'   \code{binomial()}. For discrete distributions with finite support (such as
#'   \code{bernoulli()}) the marginalisation will be exact, so long as
#'   \code{values} includes all possible values of the variable. For discrete
#'   distributions with non-finite support (such as \code{poisson()}, which has
#'   no upper bound), the marginalisation can only ever be approximate. However
#'   if \code{values} cover a range of values with sufficiently high support in
#'   the distribution, that approximation error will be minimal.
discrete_marginalisation <- function(values) {

  if (!is.vector(values) | !is.numeric(values)) {
    msg <- "'values' must be an R numeric vector"

    if (inherits(values, "greta_array")) {
      msg <- paste0(msg, ", not a greta array")
    }

    stop (msg)
  }

  # define the marginalisation function
  tf_marginaliser <- function(tf_conditional_density_fun,
                              tf_distribution_log_pdf,
                              other_args) {

    # convert these into a list of constant tensors with the correct dimensions
    # and float types
    values_list <- as.list(values)
    values_list <- lapply(values_list, as_2D_array)
    values_list <- lapply(values_list, add_first_dim)
    values_list <- lapply(values_list, fl)

    # 1. get weights from the distribution log pdf
    # assuming values is a list, get tensors for the weights
    weights_list <- lapply(values_list, tf_distribution_log_pdf)
    weights_list <- lapply(weights_list, tf_sum)

    # convert to a vector of discrete probabilities and make them sum to 1
    weights_vec <- tf$concat(weights_list, axis = 1L)
    weights_vec <- tf$exp(weights_vec)
    weights_vec <- weights_vec / tf_sum(weights_vec)
    log_weights_vec <- tf$log(weights_vec)

    # 2. compute the conditional joint density for each value (passing in
    # other_args)
    log_density_list <- list()
    for (i in seq_along(values_list)) {
      args <- c(list(values_list[[i]]), other_args)
      log_density_list[[i]] <- do.call(tf_conditional_density_fun, args)
    }

    log_density_list <- lapply(log_density_list, tf$expand_dims, 1L)
    log_density_list <- lapply(log_density_list, tf$expand_dims, 2L)
    log_density_vec <- tf$concat(log_density_list, axis = 1L)

    # 3. compute a weighted sum
    log_density_weighted_vec <- log_density_vec + log_weights_vec
    tf$reduce_logsumexp(log_density_weighted_vec, axis = 1L)

  }

  as_marginaliser(name = "discrete",
                  tf_marginaliser = tf_marginaliser,
                  parameters = list(values = values),
                  distribution_check = discrete_check)

}

# check that the distribution is discrete
discrete_check <- function(distrib) {
  if (!distrib$discrete) {
    stop ("this marginalisation method can only be used ",
          "with discrete distributions",
          call. = FALSE)
  }
}

# helper to contruct marginalisers
as_marginaliser <- function (name, tf_marginaliser, parameters, distribution_check) {

  obj <- list(name = name,
              tf_marginaliser = tf_marginaliser,
              parameters = parameters,
              distribution_check = distribution_check)

  class_name <- paste0(name, "_marginaliser")
  class(obj) <- c(class_name, "marginaliser")
  obj

}

#' @noRd
#' @export
print.marginaliser <- function(x, ...) {
  msg <- paste(x$name, "marginaliser object")
  cat(msg)
}
