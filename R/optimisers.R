#' @name optimisers
#'
#' @title optimisation methods
#' @description Functions to set up optimisers (which find parameters that
#'   maximise the joint density of a model) and change their tuning parameters,
#'   for use in \code{\link{opt}()}. For details of the algorithms and how to tune them, see the
#'   \href{https://www.tensorflow.org/api_guides/python/train#Optimizers}{TensorFlow
#'   optimiser docs} or the
#'   \href{https://docs.scipy.org/doc/scipy/reference/generated/scipy.optimize.minimize.html}{SciPy
#'   optimiser docs}.
#'
#' @return an \code{optimiser} object that can be passed to \code{\link{opt}}.
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

# set up an optimiser object
optimiser <- function (name,
                       tf_optimiser = NULL,
                       scipy_method = name,
                       parameters = list(),
                       type = c("tensorflow", "scipy")) {

  type <- match.arg(type)

  obj <- list(name = name,
              tf_optimiser = tf_optimiser,
              scipy_method = scipy_method,
              parameters = parameters,
              type = type)

  class_name <- paste0(name, "_optimiser")
  class(obj) <- c(class_name, "optimiser")
  obj

}

#' @rdname optimisers
#' @export
#'
nelder_mead <- function () {
  optimiser("nelder_mead",
            scipy_method = "Nelder-Mead",
            type = "scipy")
}

#' @rdname optimisers
#' @export
#'
powell <- function () {
  optimiser("powell",
            scipy_method = "Powell",
            type = "scipy")
}

#' @rdname optimisers
#' @export
#'
cg <- function () {
  optimiser("cg",
            scipy_method = "CG",
            type = "scipy")
}

#' @rdname optimisers
#' @export
#'
bfgs <- function () {
  optimiser("bfgs",
            scipy_method = "BFGS",
            type = "scipy")
}

#' @rdname optimisers
#' @export
#'
newton_cg <- function () {
  optimiser("newton_cg",
            scipy_method = "Newton-CG",
            type = "scipy")
}

#' @rdname optimisers
#' @export
#'
#' @param maxcor maximum number of 'variable metric corrections' used to define
#'   the approximation to the hessian matrix
#' @param maxls maximum number of line search steps per iteration
#'
l_bfgs_b <- function (maxcor = 10, maxls = 20) {
  optimiser("l_bfgs_b",
            scipy_method = "L-BFGS-B",
            parameters = list(
              maxcor = as.integer(maxcor),
              maxls = as.integer(maxls)
            ),
            type = "scipy")
}

#' @rdname optimisers
#' @export
#'
#' @param max_cg_it maximum number of hessian * vector evaluations per iteration
#' @param stepmx maximum step for the line search
#' @param rescale log10 scaling factor used to trigger rescaling of objective
#'
tnc <- function (max_cg_it = -1, stepmx = 0, rescale = -1) {
  optimiser("tnc",
            scipy_method = "TNC",
            parameters = list(
              maxCGit = as.integer(max_cg_it),
              stepmx = stepmx,
              rescale = rescale
            ),
            type = "scipy")
}

#' @rdname optimisers
#' @export
#'
#' @param rhobeg reasonable initial changes to the variables
#'
cobyla <- function (rhobeg = 1) {
  optimiser("cobyla",
            scipy_method = "COBYLA",
            parameters = list(
              rhobeg = rhobeg
            ),
            type = "scipy")
}

#' @rdname optimisers
#' @export
#'
slsqp <- function () {
  optimiser("slsqp",
            scipy_method = "SLSQP",
            type = "scipy")
}


#' @rdname optimisers
#' @export
#'
#' @param learning_rate the size of steps (in parameter space) towards the
#'   optimal value
gradient_descent <- function (learning_rate = 0.01) {
  optimiser("gradient_descent",
            tf_optimiser = "tf$train$GradientDescentOptimizer",
            parameters = list(
              learning_rate = learning_rate
            ),
            type = "tensorflow")
}


#' @rdname optimisers
#' @export
#'
#' @param rho the decay rate
#' @param epsilon a small constant used to condition gradient updates
adadelta <- function (learning_rate = 0.001, rho = 1, epsilon = 1e-08) {
  optimiser("adadelta",
            tf_optimiser = "tf$train$AdadeltaOptimizer",
            parameters = list(
              learning_rate = learning_rate,
              rho = rho,
              epsilon = epsilon
            ),
            type = "tensorflow")
}

#' @rdname optimisers
#' @export
#'
#' @param initial_accumulator_value initial value of the 'accumulator' used to
#'   tune the algorithm
#'
adagrad <- function (learning_rate = 0.8,
                     initial_accumulator_value = 0.1) {
  optimiser("adagrad",
            tf_optimiser = "tf$train$AdagradOptimizer",
            parameters = list(
              learning_rate = learning_rate,
              initial_accumulator_value = initial_accumulator_value
            ),
            type = "tensorflow")
}

#' @rdname optimisers
#' @export
#'
#' @param global_step the current training step number
#' @param initial_gradient_squared_accumulator_value initial value of the accumulators used to
#'   tune the algorithm
#' @param l1_regularization_strength L1 regularisation coefficient (must be 0 or greater)
#' @param l2_regularization_strength L2 regularisation coefficient (must be 0 or greater)
#'
adagrad_da <- function (learning_rate = 0.8,
                        global_step = 1L,
                        initial_gradient_squared_accumulator_value = 0.1,
                        l1_regularization_strength = 0,
                        l2_regularization_strength = 0) {
  optimiser("adagrad_da",
            tf_optimiser = "tf$train$AdagradDAOptimizer",
            parameters = list(
              learning_rate = learning_rate,
              global_step = global_step,
              initial_gradient_squared_accumulator_value =
                initial_gradient_squared_accumulator_value,
              l1_regularization_strength = l1_regularization_strength,
              l2_regularization_strength = l2_regularization_strength
            ),
            type = "tensorflow")
}

#' @rdname optimisers
#' @export
#'
#' @param momentum the momentum of the algorithm
#' @param use_nesterov whether to use Nesterov momentum
#'
momentum <- function (learning_rate = 0.001,
                      momentum = 0.9,
                      use_nesterov = TRUE) {
  optimiser("momentum",
            tf_optimiser = "tf$train$MomentumOptimizer",
            parameters = list(
              learning_rate = learning_rate,
              momentum = momentum,
              use_nesterov = use_nesterov
            ),
            type = "tensorflow")
}

#' @rdname optimisers
#' @export
#'
#' @param beta1 exponential decay rate for the 1st moment estimates
#' @param beta2 exponential decay rate for the 2nd moment estimates
#'
adam <- function (learning_rate = 0.1,
                  beta1 = 0.9,
                  beta2 = 0.999,
                  epsilon = 1e-08) {
  optimiser("adam",
            tf_optimiser = "tf$train$AdamOptimizer",
            parameters = list(
              learning_rate = learning_rate,
              beta1 = beta1,
              beta2 = beta2,
              epsilon = epsilon
            ),
            type = "tensorflow")
}

#' @rdname optimisers
#' @export
#'
#' @param learning_rate_power power on the learning rate, must be 0 or less
#'
ftrl <- function (learning_rate = 1,
                  learning_rate_power = -0.5,
                  initial_accumulator_value = 0.1,
                  l1_regularization_strength = 0,
                  l2_regularization_strength = 0) {
  optimiser("ftrl",
            tf_optimiser = "tf$train$FtrlOptimizer",
            parameters = list(
              learning_rate = learning_rate,
              learning_rate_power = learning_rate_power,
              initial_accumulator_value = initial_accumulator_value,
              l1_regularization_strength = l1_regularization_strength,
              l2_regularization_strength = l2_regularization_strength
            ),
            type = "tensorflow")
}

#' @rdname optimisers
#' @export
#'
proximal_gradient_descent <- function (learning_rate = 0.01,
                                       l1_regularization_strength = 0,
                                       l2_regularization_strength = 0) {
  optimiser("proximal_gradient_descent",
            tf_optimiser = "tf$train$ProximalGradientDescentOptimizer",
            parameters = list(
              learning_rate = learning_rate,
              l1_regularization_strength = l1_regularization_strength,
              l2_regularization_strength = l2_regularization_strength
            ),
            type = "tensorflow")
}

#' @rdname optimisers
#' @export
#'
proximal_adagrad <- function (learning_rate = 1,
                              initial_accumulator_value = 0.1,
                              l1_regularization_strength = 0,
                              l2_regularization_strength = 0) {
  optimiser("proximal_adagrad",
            tf_optimiser = "tf$train$ProximalAdagradOptimizer",
            parameters = list(
              learning_rate = learning_rate,
              initial_accumulator_value = initial_accumulator_value,
              l1_regularization_strength = l1_regularization_strength,
              l2_regularization_strength = l2_regularization_strength
            ),
            type = "tensorflow")
}

#' @rdname optimisers
#' @export
#'
rms_prop <- function (learning_rate = 0.1,
                      decay = 0.9,
                      momentum = 0,
                      epsilon = 1e-10) {
  optimiser("rms_prop",
            tf_optimiser = "tf$train$RMSPropOptimizer",
            parameters = list(
              learning_rate = learning_rate,
              decay = decay,
              momentum = momentum,
              epsilon = epsilon
            ),
            type = "tensorflow")
}

#' @noRd
#' @export
print.optimiser <- print.sampler
