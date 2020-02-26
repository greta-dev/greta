# marginaliser constructors

#' @name marginalisers
#'
#' @title marginalisation methods
#' @description Functions to set up marginalisers (which explicitly integrate
#'   out random variables) and modify their behaviour, for use in
#'   \code{\link{marginalise}()}.
#'
#'   \code{discrete_marginalisation} marginalises discrete random variables via
#'   a Riemann sum over a range of plausible values.
#'
#'   \code{laplace_approximation} marginalises random variables (with
#'   multivariate normal priors) approximately, using by Newton-Raphson
#'   optimisation and assuming the posterior follows a multivariate normal
#'   distribution.
#'
#' @return a \code{marginaliser} object that can be passed to
#'   \code{marginalise}.
NULL

#' @rdname marginalisers
#' @export
#'
#' @param values an R vector giving values at which to evaluate the function for
#'   a discrete marginalisation
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

    stop(msg)
  }

  # convert values to a list of data greta arrays
  values_list <- as.list(values)
  values_list <- lapply(values_list, as_data)
  names(values_list) <- paste0("value_", seq_along(values_list))

  # function to compute the parameter (log probabilities) of the marginalisation
  tf_compute_log_weights <- function(tfp_distribution, ...) {

    # and compute log probabilities for the values
    values_list <- list(...)
    log_weights_list <- lapply(values_list, tfp_distribution$log_prob)
    log_weights_list <- lapply(log_weights_list, tf_sum)

    # convert to a vector of discrete probabilities and make them sum to 1,
    # whilst staying on the log scale
    log_weights <- tf$concat(log_weights_list, axis = 1L)

    # normalise weights on log scale
    log_weights_sum <- tf$reduce_logsumexp(
      log_weights,
      axis = 1L,
      keepdims = TRUE
    )

    log_weights - log_weights_sum

  }

  # return a named list of operation greta arrays for the marginalisation
  # parameters
  compute_parameters <- function(conditional_density_fun,
                                 distribution_node,
                                 dots,
                                 marginaliser) {

    # list of parameters in a mock greta array (just representing a tf list)
    args <- c(operation = "marginalisation_log_weights",
              distribution_node,
              values_list,
              tf_operation = "tf_compute_log_weights",
              list(dim = c(length(values_list), 1L)))
    log_weights <- do.call(op, args)

    list(log_weights = log_weights)

  }

  # compute the marginal joint density
  tf_marginalisation_density <- function(parameters,
                                         tf_conditional_density_fun,
                                         dots,
                                         other_args) {

    # unpack the parameters
    log_weights <- parameters$log_weights
    values_list <- parameters[names(parameters) != "log_weights"]

    # compute the conditional joint density for each value (passing in
    # dots too)
    log_density_list <- list()
    for (i in seq_along(values_list)) {
      args <- c(list(values_list[[i]]), dots)
      log_density_list[[i]] <- do.call(tf_conditional_density_fun, args)
    }

    # expand and combine the densities
    log_density_list <- lapply(log_density_list, tf$expand_dims, 1L)
    log_density_list <- lapply(log_density_list, tf$expand_dims, 2L)
    log_density_vec <- tf$concat(log_density_list, axis = 1L)

    # compute a weighted sum
    log_density_weighted_vec <- log_density_vec + log_weights
    density <- tf$reduce_logsumexp(log_density_weighted_vec, axis = 1L)
    density

  }

  return_list_function <- function(parameters) {
    list(probabilities = exp(parameters$log_weights))
  }

  as_marginaliser(name = "discrete",
                  compute_parameters = compute_parameters,
                  tf_marginalisation_density = tf_marginalisation_density,
                  return_list = return_list_function,
                  distribution_check = discrete_check,
                  other_parameters = values_list)

}

#' @rdname marginalisers
#' @export
#'
#' @param tolerance the (positive) numerical convergence tolerance (in units of
#'   the log conditional posterior density) of the Newton-Raphson optimisation
#'   algorithm
#'
#' @param max_iterations the (positive) integer-like maximum number of
#'   iterations of the Newton-Raphson optimisation algorithm
#'
#' @param diagonal_hessian whether the Hessian matrix should be assumed to be
#'   diagonal, to speed up computations. See Details.
#'
#' @details \code{laplace_approximation} can only be used to marginalise
#'   variables following a multivariate normal distribution.
#'
#'  The argument \code{diagonal_hessian} can be used to state that the
#'  conditional density factorises along the elements of the variable being
#'  marginalised, and therefore the Hessian matrix of this function can be
#'  assumed to be diagonal. A conditional density function factorises if each
#'  observation in the conditional density depends only on the corresponding per
#'  element of the variable being marginalised. If this is not the case and you
#'  set \code{diagonal_hessian = TRUE}, your inferences will be incorrect.
#'
laplace_approximation <- function(tolerance = 1e-6,
                                  max_iterations = 50,
                                  diagonal_hessian = FALSE) {

  # in future:
  #  - enable warm starts for subsequent steps of the outer inference algorithm
  #  - enable a non-factorising version (have the user say whether it is
  # factorising)
  #  - handle an iid normal distribution too.

  if (!(is.numeric(tolerance) &&
        is.vector(tolerance) &&
        length(tolerance) == 1 &&
        tolerance > 0)) {
    stop("'tolerance' must be a positive, scalar numeric value")
  }

  max_iterations <- as.integer(max_iterations)
  if (!(is.vector(max_iterations) &&
        length(max_iterations) == 1 &&
        max_iterations > 0)) {
    stop("'max_iterations' must be a positive, scalar integer value")
  }

  # function to compute the parameter (log probabilities) of the marginalisation
  # define the marginalisation function
  tf_compute_laplace_parameters <- function(mu,
                                            sigma,
                                            ...,
                                            tf_conditional_density_fun,
                                            diagonal_hessian) {

    dots <- list(...)

    # simpler interface to conditional density. If reduce = TRUE, it does
    # reduce_sum on component densities
    d0 <- function(z, reduce = TRUE) {
      # transpose z to a row vector, which the dag is expecting
      t_z <- tf_transpose(z)
      args <- c(list(t_z), dots, list(reduce = reduce))
      do.call(tf_conditional_density_fun, args)
    }

    # get the vectors of first and second derivatives of the conditional density
    # function, w.r.t. the variable being marginalised
    if (diagonal_hessian) {
      derivs <- function(z) {
        y <- d0(z, reduce = FALSE)
        d1 <- tf$gradients(y, z)[[1]]
        d2 <- tf$gradients(d1, z)[[1]]
        list(d1, d2)
      }
    } else {

      stop("inference with non-diagonal hessians is not yet implemented",
           call. = FALSE)

      derivs <- function(z) {
        y <- d0(z, reduce = FALSE)
        d1 <- tf$gradients(y, z)[[1]]
        d2 <- tf$hessians(y, z)[[1]]  # this won't work!
        list(d1, d2)
      }
    }

    # negative log-posterior for the current value of z under MVN assumption
    psi <- function(a, z, mu) {
      p1 <- tf$matmul(tf_transpose(a), z - mu)
      fl(0.5) * tf$squeeze(p1, 1:2) - d0(z)
    }

    # tensors for parameters of MVN distribution (make mu column vector now)
    mu <- tf_transpose(mu)

    # dimension of the MVN distribution
    n <- dim(mu)[[2]]

    # here z is a *column vector* to simplify later calculations, it needs to be
    # transposed to a row vector before feeding into the likelihood function(s)
    z <- tf$identity(mu)

    # Newton-Raphson parameters
    tol <- tf$constant(tolerance, tf_float(), shape(1))
    obj_old <- tf$constant(Inf, tf_float(), shape(1))
    iter <- tf$constant(0L)
    maxiter <- tf$constant(max_iterations)

    # other objects
    a_value <- add_first_dim(as_2d_array(rep(0, n)))
    a <- fl(a_value)
    u_value <- add_first_dim(diag(n))
    u <- fl(u_value)
    eye <- fl(add_first_dim(diag(n)))

    # match batches on everything going into the loop that will have a batch
    # dimension later
    objects <- list(mu, sigma, z, a, u, obj_old, tol)
    objects <- greta:::match_batches(objects)
    mu <- objects[[1]]
    sigma <- objects[[2]]
    z <- objects[[3]]
    a <- objects[[4]]
    u <- objects[[5]]
    obj_old <- objects[[6]]
    tol <- objects[[7]]

    obj <- -d0(z)

    # get the batch dim explicitly, for the step size optimisation
    batch_dim <- tf$shape(mu)[[0]]
    batch_dim <- tf$expand_dims(batch_dim, 0L)

    # tensorflow while loop to do Newton-Raphson iterations
    body <- function(z, a, u, obj_old, obj, tol, iter, maxiter) {

      obj_old <- obj

      deriv <- derivs(z)
      d1 <- deriv[[1]]
      d2 <- deriv[[2]]

      # curvature of the likelihood
      w <- -d2
      rw <- sqrt(w)

      # decentred values of z
      cf <- z - mu

      # approximate posterior covariance & cholesky factor
      mat1 <- tf$matmul(rw, tf_transpose(rw)) * sigma + eye
      u <- tf$cholesky(mat1)
      l <- tf_transpose(u)

      # compute Newton-Raphson update direction
      b <- w * cf + d1
      mat2 <- rw * tf$matmul(sigma, b)
      mat3 <- tf$matrix_triangular_solve(u, mat2)
      adiff <- b - rw * tf$matrix_triangular_solve(l, mat3, lower = FALSE) - a

      # use golden section search to find the optimum distance to step in this
      # direction, for each batch simultaneously
      psiline <- function(s) {
        s <- tf$expand_dims(s, 1L)
        s <- tf$expand_dims(s, 2L)
        a_new <- a + s * adiff
        z_new <- tf$matmul(sigma, a_new) + mu
        psi(a_new, z_new, mu)
      }

      ls_results <- gss(psiline, batch_dim, upper = 2)
      stepsize <- ls_results$minimum
      stepsize <- tf$expand_dims(stepsize, 1L)
      stepsize <- tf$expand_dims(stepsize, 2L)

      # do the update and compute new z and objective
      a_new <- a + stepsize * adiff
      z_new <- tf$matmul(sigma, a_new) + mu
      obj <- psi(a_new, z_new, mu)

      list(z_new, a_new, u, obj_old, obj, tol, iter + 1L, maxiter)

    }

    cond <- function(z, a, u, obj_old, obj, tol, iter, maxiter) {
      not_all_converged <- tf$reduce_any(tf$less(tol, obj_old - obj))
      in_time <- tf$less(iter, maxiter)
      in_time & not_all_converged
    }

    values <- list(z,
                   a,
                   u,
                   obj_old,
                   obj,
                   tol,
                   iter,
                   maxiter)

    # run the Newton-Raphson optimisation to find the posterior mode of z
    out <- tf$while_loop(cond, body, values)

    a <- out[[2]]

    # apparently we need to redefine z and u here, or the backprop errors

    # lots of duplicated code; this could be tidied up, but I ran out of time!
    z <- tf$matmul(sigma, a) + mu

    # curvature of the likelihood at the mode
    deriv <- derivs(z)
    d2 <- deriv[[2]]
    w <- -d2
    rw <- sqrt(w)

    # approximate posterior covariance & cholesky factor
    mat1 <- tf$matmul(rw, tf_transpose(rw)) * sigma + eye
    u <- tf$cholesky(mat1)

    # convergence information
    iter <- out[[7]]
    converged <- tf$less(iter, maxiter)

    # return a list of these things
    list(z = z,
         mu = mu,
         a = a,
         u = u,
         iterations = iter,
         converged = converged)

  }

  # named list of operation greta arrays for the marginalisation parameters
  compute_parameters <- function(conditional_density_fun,
                                 distribution_node,
                                 dots,
                                 marginaliser) {

    # get greta arrays for parameters of distribution node
    mean <- distribution_node$parameters$mean
    sigma <- distribution_node$parameters$sigma

    # run the laplace approximation fitting and get a list of parameters in a
    # mock greta array (just representing a tf list)
    args <- c(operation = "marginalisation_parameters",
              mu = mean,
              sigma = sigma,
              dots,
              list(
                operation_args = list(
                  tf_conditional_density_fun = conditional_density_fun,
                  diagonal_hessian = marginaliser$other_args$diagonal_hessian
                ),
                tf_operation = "tf_compute_laplace_parameters",
                dim = 1
              ))

    parameter_list <- do.call(op, args)

    # extract the elements to operation greta arrays with the correct shapes
    z <- op("z",
            parameter_list,
            dim = dim(mean),
            tf_operation = "get_element",
            operation_args = list("z"))

    a <- op("a",
            parameter_list,
            dim = dim(mean),
            tf_operation = "get_element",
            operation_args = list("a"))

    mu <- op("mu",
            parameter_list,
            dim = dim(mean),
            tf_operation = "get_element",
            operation_args = list("mu"))

    u <- op("chol_sigma",
            parameter_list,
            dim = dim(sigma),
            tf_operation = "get_element",
            operation_args = list("u"))

    iterations <- op("iterations",
                     parameter_list,
                     tf_operation = "get_element",
                     operation_args = list("iterations"))

    converged <- op("converged",
                    parameter_list,
                    tf_operation = "get_element",
                    operation_args = list("converged"))

    # pull out the elements
    list(z = z,
         a = a,
         mu = mu,
         u = u,
         iterations = iterations,
         converged = converged)

  }

  tf_marginalisation_density <- function(parameters,
                                         tf_conditional_density_fun,
                                         dots,
                                         other_args) {

    diagonal_hessian <- other_args$diagonal_hessian

    # simpler interface to conditional density. If reduce = TRUE, it does
    # reduce_sum on component densities
    d0 <- function(z, reduce = TRUE) {
      # transpose z to a row vector, which the dag is expecting
      t_z <- tf_transpose(z)
      args <- c(list(t_z), dots, list(reduce = reduce))
      do.call(tf_conditional_density_fun, args)
    }

    # negative log-posterior for the current value of z under MVN assumption
    psi <- function(a, z, mu) {
      p1 <- tf$matmul(tf_transpose(a), z - mu)
      fl(0.5) * tf$squeeze(p1, 1:2) - d0(z)
    }

    mu <- parameters$mu
    u <- parameters$u
    z <- parameters$z
    a <- parameters$a

    # the approximate marginal conditional posterior
    u_diag <- tf$matrix_diag_part(u)
    logdet <- tf_sum(tf$log(u_diag))
    nmcp <- psi(a, z, mu) + tf$squeeze(logdet, 1)

    -nmcp

  }

  return_list_function <- function(parameters) {

    list(mean = t(parameters$z),
         sigma = chol2symm(parameters$u),
         iterations = parameters$iterations,
         converged = parameters$converged)

  }

  as_marginaliser(name = "laplace",
                  compute_parameters = compute_parameters,
                  tf_marginalisation_density = tf_marginalisation_density,
                  return_list = return_list_function,
                  distribution_check = multivariate_normal_check,
                  other_args = list(diagonal_hessian = diagonal_hessian))

}

# check that the distribution is discrete
discrete_check <- function(distrib) {
  if (!distrib$discrete) {
    stop("this marginalisation method can only be used ",
         "with discrete distributions",
         call. = FALSE)
  }
}

# check that the distribution is multiariate normal
multivariate_normal_check <- function(distrib) {
  if (distrib$distribution_name != "multivariate_normal") {
    stop("the Laplace approximation can only be used ",
         "with a multivariate normal distribution",
         call. = FALSE)
  }
}

# helper to contruct marginalisers
as_marginaliser <- function(name,
                            compute_parameters,
                            tf_marginalisation_density,
                            distribution_check,
                            return_list,
                            other_parameters = list(),
                            other_args = list()) {

  obj <- list(name = name,
              compute_parameters = compute_parameters,
              tf_marginalisation_density = tf_marginalisation_density,
              distribution_check = distribution_check,
              return_list = return_list,
              other_args = other_args,
              other_parameters = other_parameters)

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
