# define joint distributions

`joint` combines univariate probability distributions together into a
multivariate (and *a priori* independent between dimensions) joint
distribution, either over a variable, or for fixed data.

## Usage

``` r
joint(..., dim = NULL)
```

## Arguments

- ...:

  scalar variable greta arrays following probability distributions (see
  [`distributions()`](https://greta-dev.github.io/greta/dev/reference/distributions.md));
  the components of the joint distribution.

- dim:

  the dimensions of the greta array to be returned, either a scalar or a
  vector of positive integers. The final dimension of the greta array
  returned will be determined by the number of component distributions

## Value

A variable `greta_array` following the joint distribution.

## Details

The component probability distributions must all be either continuous or
discrete, and must have the same dimensions.

This functionality is unlikely to be useful in most models, since the
same result can usually be achieved by combining variables with separate
distributions. It is included for situations where it is more convenient
to consider these as a single distribution, e.g. for use with
`distribution` or `mixture`.

## Examples

``` r
if (FALSE) { # \dontrun{
# an uncorrelated bivariate normal
x <- joint(normal(-3, 0.5), normal(3, 0.5))
m <- model(x)
plot(mcmc(m, n_samples = 500))

# joint distributions can be used to define densities over data
x <- cbind(rnorm(10, 2, 0.5), rbeta(10, 3, 3))
mu <- normal(0, 10)
sd <- normal(0, 3, truncation = c(0, Inf))
a <- normal(0, 3, truncation = c(0, Inf))
b <- normal(0, 3, truncation = c(0, Inf))
distribution(x) <- joint(normal(mu, sd), beta(a, b),
  dim = 10
)
m <- model(mu, sd, a, b)
plot(mcmc(m))
} # }
```
