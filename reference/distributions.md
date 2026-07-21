# probability distributions

These functions can be used to define random variables in a greta model.
They return a variable greta array that follows the specified
distribution. This variable greta array can be used to represent a
parameter with prior distribution, combined into a mixture distribution
using
[`mixture()`](https://greta-dev.github.io/greta/reference/mixture.md),
or used with
[`distribution()`](https://greta-dev.github.io/greta/reference/distribution.md)
to define a distribution over a data greta array.

## Usage

``` r
uniform(min, max, dim = NULL)

normal(mean, sd, dim = NULL, truncation = c(-Inf, Inf))

lognormal(meanlog, sdlog, dim = NULL, truncation = c(0, Inf))

bernoulli(prob, dim = NULL)

binomial(size, prob, dim = NULL)

beta_binomial(size, alpha, beta, dim = NULL)

negative_binomial(size, prob, dim = NULL)

hypergeometric(m, n, k, dim = NULL)

poisson(lambda, dim = NULL)

gamma(shape, rate, dim = NULL, truncation = c(0, Inf))

inverse_gamma(alpha, beta, dim = NULL, truncation = c(0, Inf))

weibull(shape, scale, dim = NULL, truncation = c(0, Inf))

exponential(rate, dim = NULL, truncation = c(0, Inf))

pareto(a, b, dim = NULL, truncation = c(0, Inf))

student(df, mu, sigma, dim = NULL, truncation = c(-Inf, Inf))

laplace(mu, sigma, dim = NULL, truncation = c(-Inf, Inf))

beta(shape1, shape2, dim = NULL, truncation = c(0, 1))

cauchy(location, scale, dim = NULL, truncation = c(-Inf, Inf))

chi_squared(df, dim = NULL, truncation = c(0, Inf))

logistic(location, scale, dim = NULL, truncation = c(-Inf, Inf))

f(df1, df2, dim = NULL, truncation = c(0, Inf))

multivariate_normal(mean, Sigma, n_realisations = NULL, dimension = NULL)

wishart(df, Sigma)

lkj_correlation(eta, dimension = 2)

multinomial(size, prob, n_realisations = NULL, dimension = NULL)

categorical(prob, n_realisations = NULL, dimension = NULL)

dirichlet(alpha, n_realisations = NULL, dimension = NULL)

dirichlet_multinomial(size, alpha, n_realisations = NULL, dimension = NULL)
```

## Arguments

- min, max:

  scalar values giving optional limits to `uniform` variables. Like
  `lower` and `upper`, these must be specified as numerics, they cannot
  be greta arrays (though see details for a workaround). Unlike `lower`
  and `upper`, they must be finite. `min` must always be less than
  `max`.

- dim:

  the dimensions of the greta array to be returned, either a scalar or a
  vector of positive integers. See details.

- mean, meanlog, location, mu:

  unconstrained parameters

- sd, sdlog, sigma, lambda, shape, rate, df, scale, shape1, shape2,
  alpha, beta, df1, df2, a, b, eta:

  positive parameters, `alpha` must be a vector for `dirichlet` and
  `dirichlet_multinomial`.

- truncation:

  a length-two vector giving values between which to truncate the
  distribution, similarly to the `lower` and `upper` arguments to
  [`variable()`](https://greta-dev.github.io/greta/reference/variable.md)

- prob:

  probability parameter (`0 < prob < 1`), must be a vector for
  `multinomial` and `categorical`

- size, m, n, k:

  positive integer parameter

- Sigma:

  positive definite variance-covariance matrix parameter

- n_realisations:

  the number of independent realisation of a multivariate distribution

- dimension:

  the dimension of a multivariate distribution

## Value

A variable `greta_array` following the specified probability
distribution.

## Details

The discrete probability distributions (`bernoulli`, `binomial`,
`negative_binomial`, `poisson`, `multinomial`, `categorical`,
`dirichlet_multinomial`) can be used when they have fixed values (e.g.
defined as a likelihood using
[`distribution()`](https://greta-dev.github.io/greta/reference/distribution.md),
but not as unknown variables.

For univariate distributions `dim` gives the dimensions of the greta
array to create. Each element of the greta array will be (independently)
distributed according to the distribution. `dim` can also be left at its
default of `NULL`, in which case the dimension will be detected from the
dimensions of the parameters (provided they are compatible with one
another).

For multivariate distributions (`multivariate_normal()`,
`multinomial()`, `categorical()`, `dirichlet()`, and
`dirichlet_multinomial()`) each row of the output and parameters
corresponds to an independent realisation. If a single realisation or
parameter value is specified, it must therefore be a row vector (see
example). `n_realisations` gives the number of rows/realisations, and
`dimension` gives the dimension of the distribution. I.e. a bivariate
normal distribution would be produced with
`multivariate_normal(..., dimension = 2)`. The dimension can usually be
detected from the parameters.

`multinomial()` does not check that observed values sum to `size`, and
`categorical()` does not check that only one of the observed entries
is 1. It's the user's responsibility to check their data matches the
distribution!

The parameters of `uniform` must be fixed, not greta arrays. This
ensures these values can always be transformed to a continuous scale to
run the samplers efficiently. However, a hierarchical `uniform`
parameter can always be created by defining a `uniform` variable
constrained between 0 and 1, and then transforming it to the required
scale. See below for an example.

Wherever possible, the parameterisations and argument names of greta
distributions match commonly used R functions for distributions, such as
those in the `stats` or `extraDistr` packages. The following table
states the distribution function to which greta's implementation
corresponds:

|  |  |
|----|----|
| greta | reference |
| `uniform` | [stats::dunif](https://rdrr.io/r/stats/Uniform.html) |
| `normal` | [stats::dnorm](https://rdrr.io/r/stats/Normal.html) |
| `lognormal` | [stats::dlnorm](https://rdrr.io/r/stats/Lognormal.html) |
| `bernoulli` | [extraDistr::dbern](https://rdrr.io/pkg/extraDistr/man/Bernoulli.html) |
| `binomial` | [stats::dbinom](https://rdrr.io/r/stats/Binomial.html) |
| `beta_binomial` | [extraDistr::dbbinom](https://rdrr.io/pkg/extraDistr/man/BetaBinom.html) |
| `negative_binomial` | [stats::dnbinom](https://rdrr.io/r/stats/NegBinomial.html) |
| `hypergeometric` | [stats::dhyper](https://rdrr.io/r/stats/Hypergeometric.html) |
| `poisson` | [stats::dpois](https://rdrr.io/r/stats/Poisson.html) |
| `gamma` | [stats::dgamma](https://rdrr.io/r/stats/GammaDist.html) |
| `inverse_gamma` | [extraDistr::dinvgamma](https://rdrr.io/pkg/extraDistr/man/InvGamma.html) |
| `weibull` | [stats::dweibull](https://rdrr.io/r/stats/Weibull.html) |
| `exponential` | [stats::dexp](https://rdrr.io/r/stats/Exponential.html) |
| `pareto` | [extraDistr::dpareto](https://rdrr.io/pkg/extraDistr/man/Pareto.html) |
| `student` | [extraDistr::dlst](https://rdrr.io/pkg/extraDistr/man/LocationScaleT.html) |
| `laplace` | [extraDistr::dlaplace](https://rdrr.io/pkg/extraDistr/man/Laplace.html) |
| `beta` | [stats::dbeta](https://rdrr.io/r/stats/Beta.html) |
| `cauchy` | [stats::dcauchy](https://rdrr.io/r/stats/Cauchy.html) |
| `chi_squared` | [stats::dchisq](https://rdrr.io/r/stats/Chisquare.html) |
| `logistic` | [stats::dlogis](https://rdrr.io/r/stats/Logistic.html) |
| `f` | [stats::df](https://rdrr.io/r/stats/Fdist.html) |
| `multivariate_normal` | [mvtnorm::dmvnorm](https://rdrr.io/pkg/mvtnorm/man/Mvnorm.html) |
| `multinomial` | [stats::dmultinom](https://rdrr.io/r/stats/Multinom.html) |
| `categorical` | [stats::dmultinom](https://rdrr.io/r/stats/Multinom.html) (size = 1) |
| `dirichlet` | [extraDistr::ddirichlet](https://rdrr.io/pkg/extraDistr/man/Dirichlet.html) |
| `dirichlet_multinomial` | [extraDistr::ddirmnom](https://rdrr.io/pkg/extraDistr/man/DirMnom.html) |
| `wishart` | [stats::rWishart](https://rdrr.io/r/stats/rWishart.html) |
| `lkj_correlation` | [rethinking::dlkjcorr](https://rdrr.io/github/rmcelreath/rethinking/man/dlkjcorr.html) |

## Examples

``` r
if (FALSE) { # \dontrun{

# a uniform parameter constrained to be between 0 and 1
phi <- uniform(min = 0, max = 1)

# a length-three variable, with each element following a standard normal
# distribution
alpha <- normal(0, 1, dim = 3)

# a length-three variable of lognormals
sigma <- lognormal(0, 3, dim = 3)

# a hierarchical uniform, constrained between alpha and alpha + sigma,
eta <- alpha + uniform(0, 1, dim = 3) * sigma

# a hierarchical distribution
mu <- normal(0, 1)
sigma <- lognormal(0, 1)
theta <- normal(mu, sigma)

# a vector of 3 variables drawn from the same hierarchical distribution
thetas <- normal(mu, sigma, dim = 3)

# a matrix of 12 variables drawn from the same hierarchical distribution
thetas <- normal(mu, sigma, dim = c(3, 4))

# a multivariate normal variable, with correlation between two elements
# note that the parameter must be a row vector
Sig <- diag(4)
Sig[3, 4] <- Sig[4, 3] <- 0.6
theta <- multivariate_normal(t(rep(mu, 4)), Sig)

# 10 independent replicates of that
theta <- multivariate_normal(t(rep(mu, 4)), Sig, n_realisations = 10)

# 10 multivariate normal replicates, each with a different mean vector,
# but the same covariance matrix
means <- matrix(rnorm(40), 10, 4)
theta <- multivariate_normal(means, Sig, n_realisations = 10)
dim(theta)

# a Wishart variable with the same covariance parameter
theta <- wishart(df = 5, Sigma = Sig)
} # }
```
