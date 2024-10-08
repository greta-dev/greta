% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/optimisers.R
\name{optimisers}
\alias{optimisers}
\alias{nelder_mead}
\alias{bfgs}
\alias{powell}
\alias{momentum}
\alias{cg}
\alias{newton_cg}
\alias{l_bfgs_b}
\alias{tnc}
\alias{cobyla}
\alias{slsqp}
\alias{gradient_descent}
\alias{adadelta}
\alias{adagrad}
\alias{adagrad_da}
\alias{adam}
\alias{adamax}
\alias{ftrl}
\alias{proximal_gradient_descent}
\alias{proximal_adagrad}
\alias{nadam}
\alias{rms_prop}
\title{optimisation methods}
\usage{
nelder_mead(
  objective_function = NULL,
  initial_vertex = NULL,
  step_sizes = NULL,
  func_tolerance = 1e-08,
  position_tolerance = 1e-08,
  reflection = NULL,
  expansion = NULL,
  contraction = NULL,
  shrinkage = NULL
)

bfgs(
  value_and_gradients_function = NULL,
  initial_position = NULL,
  tolerance = 1e-08,
  x_tolerance = 0L,
  f_relative_tolerance = 0L,
  initial_inverse_hessian_estimate = NULL,
  stopping_condition = NULL,
  validate_args = TRUE,
  max_line_search_iterations = 50L,
  f_absolute_tolerance = 0L
)

powell()

momentum()

cg()

newton_cg()

l_bfgs_b()

tnc()

cobyla()

slsqp()

gradient_descent(learning_rate = 0.01, momentum = 0, nesterov = FALSE)

adadelta(learning_rate = 0.001, rho = 1, epsilon = 1e-08)

adagrad(learning_rate = 0.8, initial_accumulator_value = 0.1, epsilon = 1e-08)

adagrad_da(
  learning_rate = 0.8,
  global_step = 1L,
  initial_gradient_squared_accumulator_value = 0.1,
  l1_regularization_strength = 0,
  l2_regularization_strength = 0
)

adam(
  learning_rate = 0.1,
  beta_1 = 0.9,
  beta_2 = 0.999,
  amsgrad = FALSE,
  epsilon = 1e-08
)

adamax(learning_rate = 0.001, beta_1 = 0.9, beta_2 = 0.999, epsilon = 1e-07)

ftrl(
  learning_rate = 1,
  learning_rate_power = -0.5,
  initial_accumulator_value = 0.1,
  l1_regularization_strength = 0,
  l2_regularization_strength = 0,
  l2_shrinkage_regularization_strength = 0,
  beta = 0
)

proximal_gradient_descent(
  learning_rate = 0.01,
  l1_regularization_strength = 0,
  l2_regularization_strength = 0
)

proximal_adagrad(
  learning_rate = 1,
  initial_accumulator_value = 0.1,
  l1_regularization_strength = 0,
  l2_regularization_strength = 0
)

nadam(learning_rate = 0.001, beta_1 = 0.9, beta_2 = 0.999, epsilon = 1e-07)

rms_prop(
  learning_rate = 0.1,
  rho = 0.9,
  momentum = 0,
  epsilon = 1e-10,
  centered = FALSE
)
}
\arguments{
\item{objective_function}{A function that accepts a point as a real Tensor
and returns a Tensor of real dtype containing the value of the function at
that point. The function to be minimized. If \code{batch_evaluate_objective} is
TRUE, the function may be evaluated on a Tensor of shape \verb{[n+1] + s} where
n is the dimension of the problem and s is the shape of a single point in
the domain (so n is the size of a Tensor representing a single point). In
this case, the expected return value is a Tensor of shape \verb{[n+1]}. Note
that this method does not support univariate functions so the problem
dimension n must be strictly greater than 1.}

\item{initial_vertex}{Tensor of real dtype and any shape that can be
consumed by the \code{objective_function}. A single point in the domain that
will be used to construct an axes aligned initial simplex.}

\item{step_sizes}{Tensor of real dtype and shape broadcasting compatible
with \code{initial_vertex}. Supplies the simplex scale along each axes.}

\item{func_tolerance}{Single numeric number. The algorithm stops if the
absolute difference between the largest and the smallest function value
on the vertices of the simplex is below this number. Default is 1e-08.}

\item{position_tolerance}{Single numeric number. The algorithm stops if
the largest absolute difference between the coordinates of the vertices
is below this threshold.}

\item{reflection}{(optional) Positive Scalar Tensor of same dtype as
\code{initial_vertex}. This parameter controls the scaling of the reflected
vertex. See, \href{https://numerical.recipes/book.html}{Press et al(2007)}
for details. If not specified, uses the dimension dependent prescription of
Gao and Han (2012) \doi{10.1007/s10589-010-9329-3}}

\item{expansion}{(optional) Positive Scalar Tensor of same dtype as
\code{initial_vertex}. Should be greater than 1 and reflection. This parameter
controls the expanded scaling of a reflected vertex.See,
\href{https://numerical.recipes/book.html}{Press et al(2007)} for
details. If not specified, uses the dimension dependent prescription of
Gao and Han (2012) \doi{10.1007/s10589-010-9329-3}}

\item{contraction}{(optional) Positive scalar Tensor of same dtype as
\code{initial_vertex}. Must be between 0 and 1. This parameter controls the
contraction of the reflected vertex when the objective function at the
reflected point fails to show sufficient decrease. See,
\href{https://numerical.recipes/book.html}{Press et al(2007)} for
details. If not specified, uses the dimension dependent prescription of
Gao and Han (2012) \doi{10.1007/s10589-010-9329-3}}

\item{shrinkage}{(Optional) Positive scalar Tensor of same dtype as
\code{initial_vertex}. Must be between 0 and 1. This parameter is the scale by
which the simplex is shrunk around the best point when the other steps fail
to produce improvements. See,
\href{https://numerical.recipes/book.html}{Press et al(2007)} for
details. If not specified, uses the dimension dependent prescription of
Gao and Han (2012) \doi{10.1007/s10589-010-9329-3}}

\item{value_and_gradients_function}{A function that accepts a point as a
real Tensor and returns a tuple of Tensors of real dtype containing the
value of the function and its gradient at that point. The function to be
minimized. The input should be of shape \verb{[..., n]}, where n is the size of
the domain of input points, and all others are batching dimensions. The
first component of the return value should be a real Tensor of matching
shape \verb{[...]}. The second component (the gradient) should also be of
shape \verb{[..., n]} like the input value to the function.}

\item{initial_position}{real Tensor of shape \verb{[..., n]}. The starting point,
or points when using batching dimensions, of the search procedure. At
these points the function value and the gradient norm should be finite.}

\item{tolerance}{Scalar Tensor of real dtype. Specifies the gradient
tolerance for the procedure. If the supremum norm of the gradient vector
is below this number, the algorithm is stopped. Default is 1e-08.}

\item{x_tolerance}{Scalar Tensor of real dtype. If the absolute change in
the position between one iteration and the next is smaller than this
number, the algorithm is stopped. Default of 0L.}

\item{f_relative_tolerance}{Scalar Tensor of real dtype. If the relative
change in the objective value between one iteration and the next is
smaller than this value, the algorithm is stopped.}

\item{initial_inverse_hessian_estimate}{Optional Tensor of the same dtype
as the components of the output of the value_and_gradients_function. If
specified, the shape should broadcastable to shape \verb{[..., n, n]}; e.g. if a
single \verb{[n, n]} matrix is provided, it will be automatically broadcasted to
all batches. Alternatively, one can also specify a different hessian
estimate for each batch member. For the correctness of the algorithm, it
is required that this parameter be symmetric and positive definite.
Specifies the starting estimate for the inverse of the Hessian at the
initial point. If not specified, the identity matrix is used as the
starting estimate for the inverse Hessian.}

\item{stopping_condition}{(Optional) A function that takes as input two
Boolean tensors of shape \verb{[...]}, and returns a Boolean scalar tensor. The
input tensors are converged and failed, indicating the current status of
each respective batch member; the return value states whether the
algorithm should stop. The default is \code{tfp$optimizer.converged_all} which
only stops when all batch members have either converged or failed. An
alternative is \code{tfp$optimizer.converged_any} which stops as soon as one
batch member has converged, or when all have failed.}

\item{validate_args}{Logical, default TRUE. When TRUE, optimizer
parameters are checked for validity despite possibly degrading runtime
performance. When FALSE invalid inputs may silently render incorrect outputs.}

\item{max_line_search_iterations}{Python int. The maximum number of
iterations for the hager_zhang line search algorithm.}

\item{f_absolute_tolerance}{Scalar Tensor of real dtype. If the absolute
change in the objective value between one iteration and the next is
smaller than this value, the algorithm is stopped.}

\item{learning_rate}{the size of steps (in parameter space) towards the
optimal value. Default value 0.01}

\item{momentum}{hyperparameter that accelerates gradient descent in the
relevant direction and dampens oscillations. Defaults to 0, which is
vanilla gradient descent.}

\item{nesterov}{Whether to apply Nesterov momentum. Defaults to FALSE.}

\item{rho}{the decay rate}

\item{epsilon}{a small constant used to condition gradient updates}

\item{initial_accumulator_value}{initial value of the 'accumulator' used to
tune the algorithm}

\item{global_step}{the current training step number}

\item{initial_gradient_squared_accumulator_value}{initial value of the
accumulators used to tune the algorithm}

\item{l1_regularization_strength}{L1 regularisation coefficient (must be 0 or
greater)}

\item{l2_regularization_strength}{L2 regularisation coefficient (must be 0 or
greater)}

\item{beta_1}{exponential decay rate for the 1st moment estimates}

\item{beta_2}{exponential decay rate for the 2nd moment estimates}

\item{amsgrad}{Boolean. Whether to apply AMSGrad variant of this algorithm
from the paper "On the Convergence of Adam and beyond". Defaults to FALSE.}

\item{learning_rate_power}{power on the learning rate, must be 0 or less}

\item{l2_shrinkage_regularization_strength}{A float value, must be greater
than or equal to zero. This differs from L2 above in that the L2 above is
a stabilization penalty, whereas this L2 shrinkage is a magnitude penalty.
When input is sparse shrinkage will only happen on the active weights.}

\item{beta}{A float value, representing the beta value from the paper by
\href{https://static.googleusercontent.com/media/research.google.com/en//pubs/archive/41159.pdf}{McMahan et al 2013}. Defaults to 0}

\item{centered}{Boolean. If TRUE, gradients are normalized by the estimated
variance of the gradient; if FALSE, by the uncentered second moment.
Setting this to TRUE may help with training, but is slightly more
expensive in terms of computation and memory. Defaults to FALSE.}
}
\value{
an \code{optimiser} object that can be passed to \code{\link[=opt]{opt()}}.
}
\description{
Functions to set up optimisers (which find parameters that
maximise the joint density of a model) and change their tuning parameters,
for use in \code{\link[=opt]{opt()}}. For details of the algorithms and how to
tune them, see the \href{https://www.tensorflow.org/api_docs/python/tf/keras/optimizers}{TensorFlow optimiser docs}, or the \href{https://www.tensorflow.org/probability/api_docs/python/tfp/optimizer}{Tensorflow Probability optimiser docs}.
}
\details{
The optimisers \code{powell()}, \code{cg()}, \code{newton_cg()},
\code{l_bfgs_b()}, \code{tnc()}, \code{cobyla()}, and \code{slsqp()} are
now defunct. They will error when called in greta 0.5.0. This are removed
because they are no longer available in TensorFlow 2.0. Note that
optimiser \code{momentum()} has been replaced with \code{gradient_descent()}
}
\note{
This optimizer isn't supported in TF2, so proceed with caution. See
the \href{https://www.tensorflow.org/api_docs/python/tf/compat/v1/train/AdagradDAOptimizer}{TF docs on AdagradDAOptimiser} for more detail.

This optimizer isn't supported in TF2, so proceed with caution. See
the \href{https://www.tensorflow.org/api_docs/python/tf/compat/v1/train/ProximalGradientDescentOptimizer}{TF docs on ProximalGradientDescentOptimizer} for more detail.

This optimizer isn't supported in TF2, so proceed with caution. See
the \href{https://www.tensorflow.org/api_docs/python/tf/compat/v1/train/ProximalAdagradOptimizer}{TF docs on ProximalAdagradOptimizer} for more detail.
}
\examples{
\dontrun{
# use optimisation to find the mean and sd of some data
x <- rnorm(100, -2, 1.2)
mu <- variable()
sd <- variable(lower = 0)
distribution(x) <- normal(mu, sd)
m <- model(mu, sd)

# configure optimisers & parameters via 'optimiser' argument to opt
opt_res <- opt(m, optimiser = bfgs())

# compare results with the analytic solution
opt_res$par
c(mean(x), sd(x))
}
}
