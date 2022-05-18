# nolint start
#' @name optimisers
#'
#' @title optimisation methods
#' @description Functions to set up optimisers (which find parameters that
#'   maximise the joint density of a model) and change their tuning parameters,
#'   for use in [opt()]. For details of the algorithms and how to
#'   tune them, see the
#'   [SciPy
#'    optimiser docs](https://docs.scipy.org/doc/scipy-1.8.0/html-scipyorg/reference/optimize.html#optimization) or the
#'   [TensorFlow
#'    optimiser docs](https://www.tensorflow.org/versions/r1.15/api_docs/python/tf/contrib).
#'
#' @details The optimisers `powell()`, `cg()`, `newton_cg()`,
#'   `l_bfgs_b()`, `tnc()`, `cobyla()`, and `slsqp()` are
#'   deprecated. They will be removed in greta 0.4.0, since they will no longer
#'   be available in TensorFlow 2.0, on which that version of greta will depend.
#'
#' @return an `optimiser` object that can be passed to [opt()].
#'
#' @examples
#' \dontrun{
#' # use optimisation to find the mean and sd of some data
#' x <- rnorm(100, -2, 1.2)
#' mu <- variable()
#' sd <- variable(lower = 0)
#' distribution(x) <- normal(mu, sd)
#' m <- model(mu, sd)
#'
#' # configure optimisers & parameters via 'optimiser' argument to opt
#' opt_res <- opt(m, optimiser = bfgs())
#'
#' # compare results with the analytic solution
#' opt_res$par
#' c(mean(x), sd(x))
#' }
NULL
# nolint end

# deprecate some optimisers
optimiser_deprecation_warning <- function() {
  msg <- cli::format_warning(
    c(
      "This optimiser is deprecated and will be removed in {.pkg greta} 0.4.0.",
      "Please use a different optimiser."
      )
  )
  warning(
    msg,
    call. = FALSE
  )
}

# set up an optimiser object
define_scipy_optimiser <- function(name,
                                   method,
                                   parameters = list(),
                                   other_args = list(uses_callbacks = TRUE)) {
  obj <- list(
    name = name,
    method = method,
    parameters = parameters,
    other_args = other_args,
    class = scipy_optimiser
  )

  class_name <- glue::glue("{name}_optimiser")
  class(obj) <- c(class_name, "optimiser")
  obj
}

define_tf_optimiser <- function(name,
                                method,
                                parameters = list(),
                                other_args = list()) {
  obj <- list(
    name = name,
    method = method,
    parameters = parameters,
    class = tf_optimiser,
    other_args = other_args
  )

  class_name <- glue::glue("{name}_optimiser")
  class(obj) <- c(class_name, "optimiser")
  obj
}

#' @rdname optimisers
#' @export
#'
nelder_mead <- function() {
  define_scipy_optimiser(
    name = "nelder_mead",
    method = "Nelder-Mead"
  )
}

#' @rdname optimisers
#' @export
#'
powell <- function() {
  optimiser_deprecation_warning()

  define_scipy_optimiser(
    name = "powell",
    method = "Powell"
  )
}

#' @rdname optimisers
#' @export
#'
cg <- function() {
  optimiser_deprecation_warning()

  define_scipy_optimiser(
    name = "cg",
    method = "CG"
  )
}

#' @rdname optimisers
#' @export
#'
bfgs <- function() {
  define_scipy_optimiser(
    name = "bfgs",
    method = "BFGS"
  )
}

#' @rdname optimisers
#' @export
#'
newton_cg <- function() {
  optimiser_deprecation_warning()

  define_scipy_optimiser(
    name = "newton_cg",
    method = "Newton-CG"
  )
}

#' @rdname optimisers
#' @export
#'
#' @param maxcor maximum number of 'variable metric corrections' used to define
#'   the approximation to the hessian matrix
#' @param maxls maximum number of line search steps per iteration
#'
l_bfgs_b <- function(maxcor = 10, maxls = 20) {
  optimiser_deprecation_warning()

  define_scipy_optimiser(
    name = "l_bfgs_b",
    method = "L-BFGS-B",
    parameters = list(
      maxcor = as.integer(maxcor),
      maxls = as.integer(maxls)
    )
  )
}

#' @rdname optimisers
#' @export
#'
#' @param max_cg_it maximum number of hessian * vector evaluations per iteration
#' @param stepmx maximum step for the line search
#' @param rescale log10 scaling factor used to trigger rescaling of objective
#'
tnc <- function(max_cg_it = -1, stepmx = 0, rescale = -1) {
  optimiser_deprecation_warning()

  define_scipy_optimiser(
    name = "tnc",
    method = "TNC",
    parameters = list(
      maxCGit = as.integer(max_cg_it),
      stepmx = stepmx,
      rescale = rescale
    )
  )
}

#' @rdname optimisers
#' @export
#'
#' @param rhobeg reasonable initial changes to the variables
#'
#' @details The `cobyla()` does not provide information about the number of
#'   iterations nor convergence, so these elements of the output are set to NA
#'
cobyla <- function(rhobeg = 1) {
  optimiser_deprecation_warning()

  define_scipy_optimiser(
    name = "cobyla",
    method = "COBYLA",
    parameters = list(
      rhobeg = rhobeg
    ),
    other_args = list(uses_callbacks = FALSE)
  )
}

#' @rdname optimisers
#' @export
#'
slsqp <- function() {
  optimiser_deprecation_warning()

  define_scipy_optimiser(
    name = "slsqp",
    method = "SLSQP"
  )
}


#' @rdname optimisers
#' @export
#'
#' @param learning_rate the size of steps (in parameter space) towards the
#'   optimal value. Default value 0.01
#' @param momentum hyperparameter that accelerates gradient descent in the
#'   relevant direction and dampens oscillations. Defaults to 0, which is
#'   vanilla gradient descent.
#' @param nesterov Whether to apply Nesterov momentum. Defaults to FALSE.
#' @param epsilon
gradient_descent <- function(learning_rate = 0.01,
                             momentum = 0,
                             nesterov = FALSE) {
  define_tf_optimiser(
    name = "gradient_descent",
    method = "tf$keras$optimizers$SGD",
    parameters = list(
      learning_rate = learning_rate,
      momentum = momentum,
      nesterov = nesterov
    )
  )
}


#' @rdname optimisers
#' @export
#'
#' @param rho the decay rate
#' @param epsilon a small constant used to condition gradient updates
adadelta <- function(learning_rate = 0.001, rho = 1, epsilon = 1e-08) {
  define_tf_optimiser(
    name = "adadelta",
    method = "tf$keras$optimizers$Adadelta",
    parameters = list(
      learning_rate = learning_rate,
      rho = rho,
      epsilon = epsilon
    )
  )
}

#' @rdname optimisers
#' @export
#'
#' @param initial_accumulator_value initial value of the 'accumulator' used to
#'   tune the algorithm
#'
adagrad <- function(learning_rate = 0.8,
                    initial_accumulator_value = 0.1,
                    epsilon = 1e-08) {
  define_tf_optimiser(
    name = "adagrad",
    method = "tf$keras$optimizers$Adagrad",
    parameters = list(
      learning_rate = learning_rate,
      initial_accumulator_value = initial_accumulator_value,
      epsilon = epsilon
    )
  )
}

# nolint start
#' @rdname optimisers
#' @export
#'
#' @param global_step the current training step number
#' @param initial_gradient_squared_accumulator_value initial value of the
#'   accumulators used to tune the algorithm
#' @param l1_regularization_strength L1 regularisation coefficient (must be 0 or
#'   greater)
#' @param l2_regularization_strength L2 regularisation coefficient (must be 0 or
#'   greater)
#'
adagrad_da <- function(learning_rate = 0.8,
                       global_step = 1L,
                       initial_gradient_squared_accumulator_value = 0.1,
                       l1_regularization_strength = 0,
                       l2_regularization_strength = 0) {
  define_tf_optimiser(
    name = "adagrad_da",
    method = "tf$compat$v1$train$AdagradDAOptimizer",
    parameters = list(
      learning_rate = learning_rate,
      global_step = global_step,
      initial_gradient_squared_accumulator_value =
        initial_gradient_squared_accumulator_value,
      l1_regularization_strength = l1_regularization_strength,
      l2_regularization_strength = l2_regularization_strength
    )
  )
}
# nolint end

#' @rdname optimisers
#' @export
#'
#' @param momentum the momentum of the algorithm
#' @param use_nesterov whether to use Nesterov momentum
#'
momentum <- function(learning_rate = 0.001,
                     momentum = 0.9,
                     use_nesterov = TRUE) {
  define_tf_optimiser(
    name = "momentum",
    method = "tf$compat$v1$train$MomentumOptimizer",
    parameters = list(
      learning_rate = learning_rate,
      momentum = momentum,
      use_nesterov = use_nesterov
    )
  )
}

#' @rdname optimisers
#' @export
#'
#' @param beta1 exponential decay rate for the 1st moment estimates
#' @param beta2 exponential decay rate for the 2nd moment estimates
#'
adam <- function(learning_rate = 0.1,
                 beta1 = 0.9,
                 beta2 = 0.999,
                 epsilon = 1e-08) {
  define_tf_optimiser(
    name = "adam",
    method = "tf$compat$v1$train$AdamOptimizer",
    parameters = list(
      learning_rate = learning_rate,
      beta1 = beta1,
      beta2 = beta2,
      epsilon = epsilon
    )
  )
}

#' @rdname optimisers
#' @export
#'
#' @param learning_rate_power power on the learning rate, must be 0 or less
#'
ftrl <- function(learning_rate = 1,
                 learning_rate_power = -0.5,
                 initial_accumulator_value = 0.1,
                 l1_regularization_strength = 0,
                 l2_regularization_strength = 0) {
  define_tf_optimiser(
    name = "ftrl",
    method = "tf$compat$v1$train$FtrlOptimizer",
    parameters = list(
      learning_rate = learning_rate,
      learning_rate_power = learning_rate_power,
      initial_accumulator_value = initial_accumulator_value,
      l1_regularization_strength = l1_regularization_strength,
      l2_regularization_strength = l2_regularization_strength
    )
  )
}

#' @rdname optimisers
#' @export
#'
proximal_gradient_descent <- function(learning_rate = 0.01,
                                      l1_regularization_strength = 0,
                                      l2_regularization_strength = 0) {
  define_tf_optimiser(
    name = "proximal_gradient_descent",
    method = "tf$compat$v1$train$ProximalGradientDescentOptimizer",
    parameters = list(
      learning_rate = learning_rate,
      l1_regularization_strength = l1_regularization_strength,
      l2_regularization_strength = l2_regularization_strength
    )
  )
}

#' @rdname optimisers
#' @export
#'
proximal_adagrad <- function(learning_rate = 1,
                             initial_accumulator_value = 0.1,
                             l1_regularization_strength = 0,
                             l2_regularization_strength = 0) {
  define_tf_optimiser(
    name = "proximal_adagrad",
    method = "tf$compat$v1$train$ProximalAdagradOptimizer",
    parameters = list(
      learning_rate = learning_rate,
      initial_accumulator_value = initial_accumulator_value,
      l1_regularization_strength = l1_regularization_strength,
      l2_regularization_strength = l2_regularization_strength
    )
  )
}

#' @rdname optimisers
#' @export
#'
#' @param decay discounting factor for the gradient
#'
rms_prop <- function(learning_rate = 0.1,
                     decay = 0.9,
                     momentum = 0,
                     epsilon = 1e-10) {
  define_tf_optimiser(
    name = "rms_prop",
    method = "tf$compat$v1$train$RMSPropOptimizer",
    parameters = list(
      learning_rate = learning_rate,
      decay = decay,
      momentum = momentum,
      epsilon = epsilon
    )
  )
}

#' @noRd
#' @export
print.optimiser <- print.sampler
