# nolint start
#' @name optimisers
#'
#' @title optimisation methods
#' @description Functions to set up optimisers (which find parameters that
#'   maximise the joint density of a model) and change their tuning parameters,
#'   for use in [opt()]. For details of the algorithms and how to
#'   tune them, see the [TensorFlow optimiser docs](https://www.tensorflow.org/api_docs/python/tf/keras/optimizers), or the [Tensorflow Probability optimiser docs](https://www.tensorflow.org/probability/api_docs/python/tfp/optimizer).
#'
#' @details The optimisers `powell()`, `cg()`, `newton_cg()`,
#'   `l_bfgs_b()`, `tnc()`, `cobyla()`, and `slsqp()` are
#'   now defunct. They will error when called in greta 0.5.0. This are removed
#'   because they are no longer available in TensorFlow 2.0. Note that
#'   optimiser `momentum()` has been replaced with `gradient_descent()`
#'
#'
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

# defunct some optimisers
optimiser_defunct_error <- function(optimiser) {
  cli::cli_abort(
    c(
      "The optimiser, {.fun {optimiser}}, is defunct and has been removed \\
      in {.pkg greta} 0.5.0.",
      "Please use a different optimiser.",
      "See {.code ?optimisers} for detail on which optimizers are removed."
      )
  )
}

# deprecate some optimisers
optimiser_deprecation_warning <- function(version = "0.4.0") {
  cli::cli_warn(
    c(
      "This optimiser is deprecated and will be removed in {.pkg greta} \\
      {.val {version}}.",
      "Please use a different optimiser."
    )
  )
}

new_optimiser <- function(name,
                          method,
                          parameters,
                          class,
                          other_args){
  obj <- list(
    name = name,
    method = method,
    parameters = parameters,
    class = class,
    other_args = other_args
  )

  class_name <- glue::glue("{name}_optimiser")
  class(obj) <- c(class_name, "optimiser")
  obj
}

define_tf_optimiser <- function(name,
                                method,
                                parameters = list(),
                                other_args = list()) {
  new_optimiser(
    name = name,
    method = method,
    parameters = parameters,
    class = tf_optimiser,
    other_args = other_args
  )
}

define_tf_compat_optimiser <- function(name,
                                method,
                                parameters = list(),
                                other_args = list()) {
  new_optimiser(
    name = name,
    method = method,
    parameters = parameters,
    class = tf_compat_optimiser,
    other_args = other_args
  )
}

define_tfp_optimiser <- function(name,
                                 method,
                                 parameters = list(),
                                 other_args = list()) {
  new_optimiser(
    name = name,
    method = method,
    parameters = parameters,
    class = tfp_optimiser,
    other_args = other_args
  )
}

#' @rdname optimisers
#'
#' @param objective_function A function that accepts a point as a real Tensor
#'   and returns a Tensor of real dtype containing the value of the function at
#'   that point. The function to be minimized. If `batch_evaluate_objective` is
#'   TRUE, the function may be evaluated on a Tensor of shape `[n+1] + s` where
#'   n is the dimension of the problem and s is the shape of a single point in
#'   the domain (so n is the size of a Tensor representing a single point). In
#'   this case, the expected return value is a Tensor of shape `[n+1]`. Note
#'   that this method does not support univariate functions so the problem
#'   dimension n must be strictly greater than 1.
#' @param initial_vertex  Tensor of real dtype and any shape that can be
#'   consumed by the `objective_function`. A single point in the domain that
#'   will be used to construct an axes aligned initial simplex.
#' @param step_sizes  Tensor of real dtype and shape broadcasting compatible
#'   with `initial_vertex`. Supplies the simplex scale along each axes.
#' @param func_tolerance Single numeric number. The algorithm stops if the
#'    absolute difference between the largest and the smallest function value
#'    on the vertices of the simplex is below this number. Default is 1e-08.
#' @param position_tolerance Single numeric number. The algorithm stops if
#'   the largest absolute difference between the coordinates of the vertices
#'   is below this threshold.
#' @param reflection (optional) Positive Scalar Tensor of same dtype as
#'   `initial_vertex`. This parameter controls the scaling of the reflected
#'   vertex. See, [Press et al(2007)](https://numerical.recipes/book.html)
#'   for details. If not specified, uses the dimension dependent prescription of
#'    Gao and Han (2012) \doi{10.1007/s10589-010-9329-3}
#' @param expansion (optional) Positive Scalar Tensor of same dtype as
#'  `initial_vertex`. Should be greater than 1 and reflection. This parameter
#'  controls the expanded scaling of a reflected vertex.See,
#'   [Press et al(2007)](https://numerical.recipes/book.html) for
#'   details. If not specified, uses the dimension dependent prescription of
#'   Gao and Han (2012) \doi{10.1007/s10589-010-9329-3}
#' @param contraction (optional) Positive scalar Tensor of same dtype as
#'   `initial_vertex`. Must be between 0 and 1. This parameter controls the
#'   contraction of the reflected vertex when the objective function at the
#'   reflected point fails to show sufficient decrease. See,
#'   [Press et al(2007)](https://numerical.recipes/book.html) for
#'   details. If not specified, uses the dimension dependent prescription of
#'   Gao and Han (2012) \doi{10.1007/s10589-010-9329-3}
#' @param shrinkage (Optional) Positive scalar Tensor of same dtype as
#'   `initial_vertex`. Must be between 0 and 1. This parameter is the scale by
#'   which the simplex is shrunk around the best point when the other steps fail
#'   to produce improvements. See,
#'   [Press et al(2007)](https://numerical.recipes/book.html) for
#'   details. If not specified, uses the dimension dependent prescription of
#'   Gao and Han (2012) \doi{10.1007/s10589-010-9329-3}
#'
#' @export
#'
nelder_mead <- function(
    objective_function = NULL,
    initial_vertex = NULL,
    step_sizes = NULL,
    func_tolerance = 1e-08,
    position_tolerance = 1e-08,
    reflection = NULL,
    expansion = NULL,
    contraction = NULL,
    shrinkage = NULL) {

  define_tfp_optimiser(
    name = "nelder_mead",
    method = "tfp$optimizer$nelder_mead_minimize",
    parameters = list(
      objective_function = objective_function,
      initial_simplex = NULL,
      initial_vertex = initial_vertex,
      step_sizes = step_sizes,
      objective_at_initial_simplex = NULL,
      objective_at_initial_vertex = NULL,
      func_tolerance = func_tolerance,
      position_tolerance = position_tolerance,
      parallel_iterations = 1L,
      reflection = reflection,
      expansion = expansion,
      contraction = contraction,
      shrinkage = shrinkage,
      name = NULL
    )
  )
}

#' @rdname optimisers
#'
#' @param value_and_gradients_function A function that accepts a point as a
#'   real Tensor and returns a tuple of Tensors of real dtype containing the
#'   value of the function and its gradient at that point. The function to be
#'   minimized. The input should be of shape `[..., n]`, where n is the size of
#'   the domain of input points, and all others are batching dimensions. The
#'   first component of the return value should be a real Tensor of matching
#'   shape `[...]`. The second component (the gradient) should also be of
#'   shape `[..., n]` like the input value to the function.
#' @param initial_position real Tensor of shape `[..., n]`. The starting point,
#'   or points when using batching dimensions, of the search procedure. At
#'   these points the function value and the gradient norm should be finite.
#' @param tolerance Scalar Tensor of real dtype. Specifies the gradient
#'   tolerance for the procedure. If the supremum norm of the gradient vector
#'   is below this number, the algorithm is stopped. Default is 1e-08.
#' @param x_tolerance Scalar Tensor of real dtype. If the absolute change in
#'   the position between one iteration and the next is smaller than this
#'   number, the algorithm is stopped. Default of 0L.
#' @param f_relative_tolerance Scalar Tensor of real dtype. If the relative
#'   change in the objective value between one iteration and the next is
#'   smaller than this value, the algorithm is stopped.
#' @param initial_inverse_hessian_estimate Optional Tensor of the same dtype
#'   as the components of the output of the value_and_gradients_function. If
#'   specified, the shape should broadcastable to shape `[..., n, n]`; e.g. if a
#'   single `[n, n]` matrix is provided, it will be automatically broadcasted to
#'   all batches. Alternatively, one can also specify a different hessian
#'   estimate for each batch member. For the correctness of the algorithm, it
#'   is required that this parameter be symmetric and positive definite.
#'   Specifies the starting estimate for the inverse of the Hessian at the
#'   initial point. If not specified, the identity matrix is used as the
#'   starting estimate for the inverse Hessian.
#' @param stopping_condition (Optional) A function that takes as input two
#'   Boolean tensors of shape `[...]`, and returns a Boolean scalar tensor. The
#'   input tensors are converged and failed, indicating the current status of
#'   each respective batch member; the return value states whether the
#'   algorithm should stop. The default is `tfp$optimizer.converged_all` which
#'   only stops when all batch members have either converged or failed. An
#'   alternative is `tfp$optimizer.converged_any` which stops as soon as one
#'   batch member has converged, or when all have failed.
#' @param validate_args Logical, default TRUE. When TRUE, optimizer
#'   parameters are checked for validity despite possibly degrading runtime
#'   performance. When FALSE invalid inputs may silently render incorrect outputs.
#' @param max_line_search_iterations Python int. The maximum number of
#'   iterations for the hager_zhang line search algorithm.
#' @param f_absolute_tolerance Scalar Tensor of real dtype. If the absolute
#'   change in the objective value between one iteration and the next is
#'   smaller than this value, the algorithm is stopped.
#'
#' @export
bfgs <- function(value_and_gradients_function = NULL,
                 initial_position = NULL,
                 tolerance = 1e-08,
                 x_tolerance = 0L,
                 f_relative_tolerance = 0L,
                 initial_inverse_hessian_estimate = NULL,
                 stopping_condition = NULL,
                 validate_args = TRUE,
                 max_line_search_iterations = 50L,
                 f_absolute_tolerance = 0L) {
  define_tfp_optimiser(
    name = "bfgs",
    method = "tfp$optimizer$bfgs_minimize",
    parameters = list(
      value_and_gradients_function = value_and_gradients_function,
      initial_position = initial_position,
      tolerance = tolerance,
      x_tolerance = x_tolerance,
      f_relative_tolerance = f_relative_tolerance,
      initial_inverse_hessian_estimate = initial_inverse_hessian_estimate,
      parallel_iterations = 1L,
      stopping_condition = stopping_condition,
      validate_args = validate_args,
      max_line_search_iterations = max_line_search_iterations,
      f_absolute_tolerance = f_absolute_tolerance,
      name = NULL
    )
  )
}

#' @rdname optimisers
#' @export
#'
powell <- function() {
  optimiser_defunct_error("powell")
}

#' @rdname optimisers
#' @export
#'
momentum <- function() {
  optimiser_defunct_error("momentum")
}

#' @rdname optimisers
#' @export
#'
cg <- function() {
  optimiser_defunct_error("cg")
}

#' @rdname optimisers
#' @export
#'
newton_cg <- function() {
  optimiser_defunct_error("newton_cg")
}

#' @rdname optimisers
#' @export
#'
l_bfgs_b <- function() {
  optimiser_defunct_error("l_bfgs_b")
}

#' @rdname optimisers
#' @export
#'
tnc <- function() {
  optimiser_defunct_error("tnc")
}

#' @rdname optimisers
#' @export
#'
cobyla <- function() {
  optimiser_defunct_error("cobyla")
}

#' @rdname optimisers
#' @export
#'
slsqp <- function() {
  optimiser_defunct_error("slsqp")
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
gradient_descent <- function(learning_rate = 0.01,
                             momentum = 0,
                             nesterov = FALSE) {
  define_tf_optimiser(
    name = "gradient_descent",
    method = "tf$keras$optimizers$legacy$SGD",
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
    # method = "tf$keras$optimizers$Adadelta",
    method = "tf$keras$optimizers$legacy$Adadelta",
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
    # method = "tf$keras$optimizers$Adagrad",
    method = "tf$keras$optimizers$legacy$Adagrad",
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
#' @note This optimizer isn't supported in TF2, so proceed with caution. See
#'  the [TF docs on AdagradDAOptimiser](https://www.tensorflow.org/api_docs/python/tf/compat/v1/train/AdagradDAOptimizer) for more detail.
#'
adagrad_da <- function(learning_rate = 0.8,
                       global_step = 1L,
                       initial_gradient_squared_accumulator_value = 0.1,
                       l1_regularization_strength = 0,
                       l2_regularization_strength = 0) {

  optimiser_deprecation_warning(version = "0.6.0")

  define_tf_compat_optimiser(
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
#' @param beta_1 exponential decay rate for the 1st moment estimates
#' @param beta_2 exponential decay rate for the 2nd moment estimates
#' @param amsgrad Boolean. Whether to apply AMSGrad variant of this algorithm
#'   from the paper "On the Convergence of Adam and beyond". Defaults to FALSE.
#'
adam <- function(learning_rate = 0.1,
                 beta_1 = 0.9,
                 beta_2 = 0.999,
                 amsgrad = FALSE,
                 epsilon = 1e-08) {
  define_tf_optimiser(
    name = "adam",
    # method = "tf$keras$optimizers$Adam",
    method = "tf$keras$optimizers$legacy$Adam",
    parameters = list(
      learning_rate = learning_rate,
      beta_1 = beta_1,
      beta_2 = beta_2,
      epsilon = epsilon,
      amsgrad = amsgrad
    )
  )
}

#' @rdname optimisers
#' @export
#'
adamax <- function(learning_rate = 0.001,
                   beta_1 = 0.9,
                   beta_2 = 0.999,
                   epsilon = 1e-07){
  define_tf_optimiser(
    name = "adamax",
    # method = "tf$keras$optimizers$Adamax",
    method = "tf$keras$optimizers$legacy$Adamax",
    parameters = list(
      learning_rate = learning_rate,
      beta_1 = beta_1,
      beta_2 = beta_2,
      epsilon = epsilon
    )
  )
}

#' @rdname optimisers
#' @export
#'
#' @param learning_rate_power power on the learning rate, must be 0 or less
#' @param l2_shrinkage_regularization_strength A float value, must be greater
#'   than or equal to zero. This differs from L2 above in that the L2 above is
#'   a stabilization penalty, whereas this L2 shrinkage is a magnitude penalty.
#'   When input is sparse shrinkage will only happen on the active weights.
#' @param beta A float value, representing the beta value from the paper by
#'   [McMahan et al 2013](https://static.googleusercontent.com/media/research.google.com/en//pubs/archive/41159.pdf). Defaults to 0
#'
ftrl <- function(learning_rate = 1,
                 learning_rate_power = -0.5,
                 initial_accumulator_value = 0.1,
                 l1_regularization_strength = 0,
                 l2_regularization_strength = 0,
                 l2_shrinkage_regularization_strength = 0,
                 beta = 0) {
  define_tf_optimiser(
    name = "ftrl",
    # method = "tf$keras$optimizers$Ftrl",
    method = "tf$keras$optimizers$legacy$Ftrl",
    parameters = list(
      learning_rate = learning_rate,
      learning_rate_power = learning_rate_power,
      initial_accumulator_value = initial_accumulator_value,
      l1_regularization_strength = l1_regularization_strength,
      l2_regularization_strength = l2_regularization_strength,
      l2_shrinkage_regularization_strength = l2_shrinkage_regularization_strength,
      beta = beta
    )
  )
}

#' @rdname optimisers
#' @export
#'
#' @note This optimizer isn't supported in TF2, so proceed with caution. See
#'  the [TF docs on ProximalGradientDescentOptimizer](https://www.tensorflow.org/api_docs/python/tf/compat/v1/train/ProximalGradientDescentOptimizer) for more detail.
#'
proximal_gradient_descent <- function(learning_rate = 0.01,
                                      l1_regularization_strength = 0,
                                      l2_regularization_strength = 0) {

  optimiser_deprecation_warning(version = "0.6.0")

  define_tf_compat_optimiser(
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
#' @note This optimizer isn't supported in TF2, so proceed with caution. See
#'  the [TF docs on ProximalAdagradOptimizer](https://www.tensorflow.org/api_docs/python/tf/compat/v1/train/ProximalAdagradOptimizer) for more detail.

#'
proximal_adagrad <- function(learning_rate = 1,
                             initial_accumulator_value = 0.1,
                             l1_regularization_strength = 0,
                             l2_regularization_strength = 0) {

  optimiser_deprecation_warning(version = "0.6.0")

  define_tf_compat_optimiser(
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
nadam <- function(learning_rate = 0.001,
                  beta_1 = 0.9,
                  beta_2 = 0.999,
                  epsilon = 1e-07){

  define_tf_optimiser(
    name = "nadam",
    # method = "tf$keras$optimizers$Nadam",
    method = "tf$keras$optimizers$legacy$Nadam",
    parameters = list(
      learning_rate = learning_rate,
      beta_1 = beta_1,
      beta_2 = beta_2,
      epsilon = epsilon
    )
  )

}

#' @rdname optimisers
#' @export
#'
#' @param centered Boolean. If TRUE, gradients are normalized by the estimated
#'   variance of the gradient; if FALSE, by the uncentered second moment.
#'   Setting this to TRUE may help with training, but is slightly more
#'   expensive in terms of computation and memory. Defaults to FALSE.
rms_prop <- function(learning_rate = 0.1,
                     rho = 0.9,
                     momentum = 0,
                     epsilon = 1e-10,
                     centered = FALSE) {
  define_tf_optimiser(
    name = "rms_prop",
    # method = "tf$keras$optimizers$RMSprop",
    method = "tf$keras$optimizers$legacy$RMSprop",
    parameters = list(
      learning_rate = learning_rate,
      rho = rho,
      momentum = momentum,
      epsilon = epsilon,
      centered = centered
    )
  )
}

#' @noRd
#' @export
print.optimiser <- print.sampler
