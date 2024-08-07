% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate.R
\name{calculate}
\alias{calculate}
\title{calculate greta arrays given fixed values}
\usage{
calculate(
  ...,
  values = list(),
  nsim = NULL,
  seed = NULL,
  precision = c("double", "single"),
  trace_batch_size = 100,
  compute_options = cpu_only()
)
}
\arguments{
\item{...}{one or more greta_arrays for which to calculate the value}

\item{values}{a named list giving temporary values of the greta arrays with
which \code{target} is connected, or a \code{greta_mcmc_list} object
returned by \code{\link[=mcmc]{mcmc()}}.}

\item{nsim}{an optional positive integer scalar for the number of responses
to simulate if stochastic greta arrays are present in the model - see
Details.}

\item{seed}{an optional seed to be used in set.seed immediately before the
simulation so as to generate a reproducible sample}

\item{precision}{the floating point precision to use when calculating values.}

\item{trace_batch_size}{the number of posterior samples to process at a time
when \code{target} is a \code{greta_mcmc_list} object; reduce this to
reduce memory demands}

\item{compute_options}{Default is to use CPU only with \code{cpu_only()}. Use
\code{gpu_only()} to use only GPU. In the future we will add more options for
specifying CPU and GPU use.  If setting GPU with \code{gpu_only()} then we
cannot always guarantee that the random number seed will be respected. This
is due to the way tensorflow interfaces with the GPU. If you must have
reproducibility of all simulations we recommend using \code{cpu_only()}, which
is the default. You can turn off the message about setting seed with GPU
usage using \code{options(greta_gpu_message = FALSE)}}
}
\value{
Values of the target greta array(s), given values of the greta arrays
on which they depend (either specified in \code{values} or sampled from
their priors). If \code{values} is a
\code{\link[=mcmc]{greta_mcmc_list()}} and \code{nsim = NULL}, this will
be a \code{greta_mcmc_list} object of posterior samples for the target
greta arrays. Otherwise, the result will be a named list of numeric R
arrays. If \code{nsim = NULL} the dimensions of returned numeric R arrays
will be the same as the corresponding greta arrays, otherwise an additional
dimension with \code{nsim} elements will be prepended, to represent
multiple simulations.
}
\description{
Calculate the values that greta arrays would take, given
temporary, or simulated values for the greta arrays on which they depend.
This can be used to check the behaviour of your model, make predictions to
new data after model fitting, or simulate datasets from either the prior or
posterior of your model.
}
\details{
The greta arrays named in \code{values} need not be variables, they
can also be other operations or even data.

At present, if \code{values} is a named list it must contain values for
\emph{all} of the variable greta arrays with which \code{target} is
connected, even values are given for intermediate operations, or the target
doesn't depend on the variable. That may be relaxed in a future release.

If the model contains stochastic greta arrays; those with a distribution,
calculate can be used to sample from these distributions (and all greta
arrays that depend on them) by setting the \code{nsim} argument to a
positive integer for the required number of samples. If \code{values} is
specified (either as a list of fixed values or as draws), those values will
be used, and remaining variables will be sampled conditional on them.
Observed data with distributions (i.e. response variables defined with
\code{distribution()} can also be sampled, provided they are defined as
greta arrays. This behaviour can be used for a number of tasks, like
simulating datasets for known parameter sets, simulating parameters and
data from a set of priors, or simulating datasets from a model posterior.
See some examples of these below.
}
\examples{
\dontrun{

# define a variable greta array, and another that is calculated from it
# then calculate what value y would take for different values of x
x <- normal(0, 1, dim = 3)
a <- lognormal(0, 1)
y <- sum(x^2) + a
calculate(y, values = list(x = c(0.1, 0.2, 0.3), a = 2))

# by setting nsim, you can also sample values from their priors
calculate(y, nsim = 3)

# you can combine sampling and fixed values
calculate(y, values = list(a = 2), nsim = 3)

# if the greta array only depends on data,
# you can pass an empty list to values (this is the default)
x <- ones(3, 3)
y <- sum(x)
calculate(y)

# define a model
alpha <- normal(0, 1)
beta <- normal(0, 1)
sigma <- lognormal(1, 0.1)
y <- as_data(iris$Petal.Width)
mu <- alpha + iris$Petal.Length * beta
distribution(y) <- normal(mu, sigma)
m <- model(alpha, beta, sigma)

# sample values of the parameters, or different observation data (y), from
# the priors (useful for prior # predictive checking) - see also
# ?simulate.greta_model
calculate(alpha, beta, sigma, nsim = 100)
calculate(y, nsim = 100)

# calculate intermediate greta arrays, given some parameter values (useful
# for debugging models)
calculate(mu[1:5], values = list(alpha = 1, beta = 2, sigma = 0.5))
calculate(mu[1:5], values = list(alpha = -1, beta = 0.2, sigma = 0.5))

# simulate datasets given fixed parameter values
calculate(y, values = list(alpha = -1, beta = 0.2, sigma = 0.5), nsim = 10)

# you can use calculate in conjunction with posterior samples from MCMC, e.g.
# sampling different observation datasets, given a random set of these
# posterior samples - useful for posterior predictive model checks
draws <- mcmc(m, n_samples = 500)
calculate(y, values = draws, nsim = 100)

# you can use calculate on greta arrays created even after the inference on
# the model - e.g. to plot response curves
petal_length_plot <- seq(min(iris$Petal.Length),
  max(iris$Petal.Length),
  length.out = 100
)
mu_plot <- alpha + petal_length_plot * beta
mu_plot_draws <- calculate(mu_plot, values = draws)
mu_est <- colMeans(mu_plot_draws[[1]])
plot(mu_est ~ petal_length_plot,
  type = "n",
  ylim = range(mu_plot_draws[[1]])
)
apply(mu_plot_draws[[1]], 1, lines,
  x = petal_length_plot, col = grey(0.8)
)
lines(mu_est ~ petal_length_plot, lwd = 2)

# trace_batch_size can be changed to trade off speed against memory usage
# when calculating. These all produce the same result, but have increasing
# memory requirements:
mu_plot_draws_1 <- calculate(mu_plot,
  values = draws,
  trace_batch_size = 1
)
mu_plot_draws_10 <- calculate(mu_plot,
  values = draws,
  trace_batch_size = 10
)
mu_plot_draws_inf <- calculate(mu_plot,
  values = draws,
  trace_batch_size = Inf
)
}
}
