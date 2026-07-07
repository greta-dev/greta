# MCMC samplers

Functions to set up MCMC samplers and change the starting values of
their parameters, for use in
[`mcmc()`](https://greta-dev.github.io/greta/reference/inference.md).

## Usage

``` r
hmc(Lmin = 5, Lmax = 10, epsilon = 0.1, diag_sd = 1)

rwmh(proposal = c("normal", "uniform"), epsilon = 0.1, diag_sd = 1)

slice(max_doublings = 5)
```

## Arguments

- Lmin:

  minimum number of leapfrog steps (positive integer, Lmin \> Lmax)

- Lmax:

  maximum number of leapfrog steps (positive integer, Lmax \> Lmin)

- epsilon:

  leapfrog stepsize hyperparameter (positive, will be tuned)

- diag_sd:

  estimate of the posterior marginal standard deviations (positive, will
  be tuned).

- proposal:

  the probability distribution used to generate proposal states

- max_doublings:

  the maximum number of iterations of the 'doubling' algorithm used to
  adapt the size of the slice

## Value

a `sampler` object that can be passed to
[`mcmc()`](https://greta-dev.github.io/greta/reference/inference.md).

## Details

During the warmup iterations of `mcmc`, some of these sampler parameters
will be tuned to improve the efficiency of the sampler, so the values
provided here are used as starting values.

For `hmc()`, the number of leapfrog steps at each iteration is selected
uniformly at random from between `Lmin` and `Lmax`. `diag_sd` is used to
rescale the parameter space to make it more uniform, and make sampling
more efficient.

`rwmh()` creates a random walk Metropolis-Hastings sampler; a a
gradient-free sampling algorithm. The algorithm involves a proposal
generating step `proposal_state = current_state + perturb` by a random
perturbation, followed by Metropolis-Hastings accept/reject step. The
class is implemented for uniform and normal proposals.

`slice()` implements a multivariate slice sampling algorithm. The
parameter `max_doublings` is not tuned during warmup.
