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
                              other_args,
                              dag,
                              distribution_node) {

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

#' @rdname marginalisers
#' @export
#'
#' @param stepsize the (positive) size of steps in the Newton-Raphson
#'   optimisation algorithm used to find the approximate conditional posterior
#'   mode of the random variable
#'
#' @param stepsize the (positive) numerical convergence tolerance (in units of
#'   the log conditional posterior density) of the Newton-Raphson optimisation
#'   algorithm
#'
#' @param max_iterations the (positive) integer-like maximum number of iterations of
#'   the Newton-Raphson optimisation algorithm
#'
#' @details \code{laplace_approximation} can only be used to marginalise
#'   variables following a multivariate normal distribution. In addition, the
#'   function to be marginalised must \emph{factorise}; ie. it must return a
#'   vector-valued density with as many elements as the vector variable being
#'   marginalised, and each of element of the density must depend only on the
#'   corresponding element of the variable vector. This is the responsibility of
#'   the user, and is not checked.
laplace_approximation <- function(stepsize = 0.05,
                                  tolerance = 1e-6,
                                  max_iterations = 50) {

  # in future:
  #  - enable warm starts for subsequent steps of the outer inference algorithm
  # do linesearch in the Newton-Raphson iterations
  #  - enable a non-factorising version (have the user say whether it is
  # factorising)
  #  - handle an iid normal distribution too.

  # check arguments, Newton-Raphson iterations etc.
  if (!(is.numeric(stepsize) &&
        is.vector(stepsize) &&
        length(stepsize) == 1 &&
        stepsize > 0)) {
    stop ("'stepsize' must be a positive, scalar numeric value")
  }

  if (!(is.numeric(tolerance) &&
        is.vector(tolerance) &&
        length(tolerance) == 1 &&
        tolerance > 0)) {
    stop ("'tolerance' must be a positive, scalar numeric value")
  }

  max_iterations <- as.integer(max_iterations)
  if (!(is.vector(max_iterations) &&
        length(max_iterations) == 1 &&
        max_iterations > 0)) {
    stop ("'max_iterations' must be a positive, scalar integer value")
  }

  # define the marginalisation function
  tf_marginaliser <- function(tf_conditional_density_fun,
                              tf_distribution_log_pdf,
                              other_args,
                              dag,
                              distribution_node) {

    # simpler interface to conditional density. If reduce = TRUE, it does reduce_sum on component densities
    d0 <- function (z, reduce = TRUE) {
      args <- c(list(z), other_args, list(reduce = reduce))
      do.call(tf_conditional_density_fun, args)
    }

    # get the vectors of first and second derivatives of the conditional density function,
    # w.r.t. the variable being marginalised
    derivs <- function (z) {
      y <- d0(z, reduce = FALSE)
      d1 <- tf$gradients(y, z)[[1]]
      d2 <- tf$gradients(d1, z)[[1]]
      list(d1, d2)
    }

    # negative log-posterior for the current value of z under MVN assumption
    psi <- function (a, z, mu) {
      fl(0.5) * tf$matmul(tf_transpose(a), (z - mu)) - d0(z)
    }

    # # compute psi for different values of the stepsize (s)
    # # to be added later, if I can work out how to nest/recode this linesearch optimisation
    # psiline <- function (s, adiff, a, K, mu, d0) {
    #   a <- a + s * as.vector(adiff)
    #   z <- K %*% a + mu
    #   psi(a, z, mu, d0)
    # }

    # tensors for parameters of MVN distribution (make mu column vector now)
    mu_node <- distribution_node$parameters$mean
    mu <- dag$tf_environment[[dag$tf_name(mu_node)]]
    mu <- tf_transpose(mu)
    Sigma_node <- distribution_node$parameters$Sigma
    Sigma <- dag$tf_environment[[dag$tf_name(Sigma_node)]]

    # dimension of the MVN distribution
    n <- dim(mu)[[2]]

    # randomly initialise z, and expand to batch (warm starts will need some TF
    # trickery)
    z_value <- add_first_dim(as_2D_array(rnorm(n)))
    z <- tf$constant(z_value, dtype = tf_float())

    # Newton-Raphson parameters
    tol <- tf$constant(tolerance, tf_float(), shape(1))
    obj_old <- tf$constant(-Inf, tf_float(), shape(1))
    iter <- tf$constant(0L)
    maxiter <- tf$constant(max_iterations)

    # other objects
    a_value <- add_first_dim(as_2D_array(rep(0, n)))
    a <- tf$constant(a_value, dtype = tf_float())
    U_value <- add_first_dim(diag(n))
    U <- tf$constant(U_value, tf_float())
    eye <- tf$constant(add_first_dim(diag(n)),
                       dtype = tf_float())
    stepsize <- fl(stepsize)

    # match batches on everything going into the loop that will have a batch
    # dimension later
    objects <- list(mu, Sigma, z, a, U, obj_old, tol)
    objects <- greta:::match_batches(objects)
    mu <- objects[[1]]
    Sigma <- objects[[2]]
    z <- objects[[3]]
    a <- objects[[4]]
    U <- objects[[5]]
    obj_old <- objects[[6]]
    tol <- objects[[7]]

    obj <- -d0(z)


    # tensorflow while loop to do Newton-Raphson iterations
    body <- function(z, a, U, obj_old, obj, tol, iter, maxiter) {

      obj.old <- obj

      deriv <- derivs(z)
      d1 <- deriv[[1]]
      d2 <- deriv[[2]]

      # curvature of the likelihood
      W <- -d2
      rW <- sqrt(W)

      # decentred values of z
      cf <- z - mu

      # approximate posterior covariance & cholesky factor
      mat1 <- tf$matmul(rW, tf_transpose(rW)) * Sigma + eye
      U <- tf$cholesky(mat1)
      L <- tf_transpose(U)

      # compute Newton-Raphson update direction
      b <- W * cf + d1
      mat2 <- rW * tf$matmul(Sigma, b)
      mat3 <- tf$matrix_triangular_solve(U, mat2)
      adiff <- b - rW * tf$matrix_triangular_solve(L, mat3, lower = FALSE) - a

      # # find the optimal stepsize in that dimension (to be added later)
      # res <- optimise(psiline, c(0, 2), adiff, a, Sigma, mu)
      # stepsize <- res$minimum

      # do the update and compute new z and objective
      a_new <- a + stepsize * adiff
      z_new <- tf$matmul(Sigma, a_new) + mu
      obj <- psi(a_new, z_new, mu)
      obj <- tf$squeeze(obj, 1:2)

      list(z_new, a_new, U, obj_old, obj, tol, iter + 1L, maxiter)

    }

    cond <- function(z, a, U, obj_old, obj, tol, iter, maxiter) {
      none_converged <- tf$reduce_any(tf$greater(obj_old - obj, tol))
      in_time <- tf$less(iter, maxiter)
      in_time & none_converged
    }

    values <- list(z,
                   a,
                   U,
                   obj_old,
                   obj,
                   tol,
                   iter,
                   maxiter)

    # run the Newton-Raphson optimisation to find the posterior mode of z
    out <- tf$while_loop(cond, body, values)

    # get the two components, preventing the gradient from being computed through the optimisation
    a <- tf$stop_gradient(out[[2]])
    U <- tf$stop_gradient(out[[3]])

    # recompute z (gradient of z w.r.t. the free state should include this op,
    # but not optimisation)
    z <- tf$matmul(Sigma, a) + mu

    # the approximate marginal conditional posterior
    lp <- d0(z)
    p1 <- tf$matmul(tf_transpose(a), z - mu) / fl(2)
    p2 <- tf_sum(tf$log(tf$matrix_diag_part(U)))
    nmcp <- tf$squeeze(p1, 1:2) - lp + tf$squeeze(p2, 1)

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
    stop ("this marginalisation method can only be used ",
          "with discrete distributions",
          call. = FALSE)
  }
}

# check that the distribution is multiariate normal
multivariate_normal_check <- function (distrib) {
  if (distrib$distribution_name != "multivariate_normal") {
    stop ("the Laplace approximation can only be used ",
          "with a multivariate normal distribution",
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
