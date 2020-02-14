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

  # define the marginalisation function
  tf_marginaliser <- function(tf_conditional_density_fun,
                              tf_distribution_log_pdf,
                              other_args,
                              dag,
                              distribution_node) {

    # convert these into a list of constant tensors with the correct dimensions
    # and float types
    values_list <- as.list(values)
    values_list <- lapply(values_list, as_2d_array)
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
#' @details \code{laplace_approximation} can only be used to marginalise
#'   variables following a multivariate normal distribution. In addition, the
#'   function to be marginalised must \emph{factorise}; ie. it must return a
#'   vector-valued density with as many elements as the vector variable being
#'   marginalised, and each of element of the density must depend only on the
#'   corresponding element of the variable vector. This is the responsibility of
#'   the user, and is not checked.
laplace_approximation <- function(tolerance = 1e-6,
                                  max_iterations = 50) {

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

  # define the marginalisation function
  tf_marginaliser <- function(tf_conditional_density_fun,
                              tf_distribution_log_pdf,
                              other_args,
                              dag,
                              distribution_node) {

    # simpler interface to conditional density. If reduce = TRUE, it does
    # reduce_sum on component densities
    d0 <- function(z, reduce = TRUE) {
      # transpose z to a row vector, which the dag is expecting
      t_z <- tf_transpose(z)
      args <- c(list(t_z), other_args, list(reduce = reduce))
      do.call(tf_conditional_density_fun, args)
    }

    # get the vectors of first and second derivatives of the conditional density
    # function, w.r.t. the variable being marginalised
    derivs <- function(z) {
      y <- d0(z, reduce = FALSE)
      d1 <- tf$gradients(y, z)[[1]]
      d2 <- tf$gradients(d1, z)[[1]]
      list(d1, d2)
    }

    # negative log-posterior for the current value of z under MVN assumption
    psi <- function(a, z, mu) {
      p1 <- tf$matmul(tf_transpose(a), z - mu)
      fl(0.5) * tf$squeeze(p1, 1:2) - d0(z)
    }

    # tensors for parameters of MVN distribution (make mu column vector now)
    mu_node <- distribution_node$parameters$mean
    mu <- dag$tf_environment[[dag$tf_name(mu_node)]]
    mu <- tf_transpose(mu)
    sigma_node <- distribution_node$parameters$Sigma
    sigma <- dag$tf_environment[[dag$tf_name(sigma_node)]]

    # dimension of the MVN distribution
    n <- dim(mu)[[2]]

    # randomly initialise z, and expand to batch (warm starts will need some TF
    # trickery)

    # here z is a *column vector* to simplify later calculations, it needs to be
    # transposed to a row vector before feeding into the likelihood function(s)
    z_value <- add_first_dim(as_2d_array(rnorm(n)))
    z <- tf$constant(z_value, dtype = tf_float())

    # Newton-Raphson parameters
    tol <- tf$constant(tolerance, tf_float(), shape(1))
    obj_old <- tf$constant(Inf, tf_float(), shape(1))
    iter <- tf$constant(0L)
    maxiter <- tf$constant(max_iterations)

    # other objects
    a_value <- add_first_dim(as_2d_array(rep(0, n)))
    a <- tf$constant(a_value, dtype = tf_float())
    u_value <- add_first_dim(diag(n))
    u <- tf$constant(u_value, tf_float())
    eye <- tf$constant(add_first_dim(diag(n)),
                       dtype = tf_float())

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
        z_new <- tf$matmul(sigma, a) + mu
        psi(a_new, z_new, mu)
      }

      ls_results <- gss(psiline, batch_dim)
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

    # the approximate marginal conditional posterior
    logdet <- tf_sum(tf$log(tf$matrix_diag_part(u)))
    nmcp <- psi(a, z, mu) + tf$squeeze(logdet, 1)

    -nmcp

  }

  as_marginaliser(name = "laplace",
                  tf_marginaliser = tf_marginaliser,
                  parameters = list(),
                  distribution_check = multivariate_normal_check)

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
                            tf_marginaliser,
                            parameters,
                            distribution_check) {

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
