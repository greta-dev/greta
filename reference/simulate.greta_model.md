# Simulate Responses From `greta_model` Object

Simulate values of all named greta arrays associated with a greta model
from the model priors, including the response variable.

## Usage

``` r
# S3 method for class 'greta_model'
simulate(object, nsim = 1, seed = NULL, precision = c("double", "single"), ...)
```

## Arguments

- object:

  a
  [`greta_model()`](https://greta-dev.github.io/greta/reference/model.md)
  object

- nsim:

  positive integer scalar - the number of responses to simulate

- seed:

  an optional seed to be used in set.seed immediately before the
  simulation so as to generate a reproducible sample

- precision:

  the floating point precision to use when calculating values.

- ...:

  optional additional arguments, none are used at present

## Value

A named list of vectors, matrices or arrays containing independent
samples of the greta arrays associated with the model. The number of
samples will be prepended as the first dimension of the greta array, so
that a vector of samples is returned for each scalar greta array, and a
matrix is returned for each vector greta array, etc.

## Details

This is essentially a wrapper around
[`calculate()`](https://greta-dev.github.io/greta/reference/calculate.md)
that finds all relevant greta arrays. See that function for more
functionality, including simulation conditional on fixed values or
posterior samples.

To simulate values of the response variable, it must be both a named
object (in the calling environment) and be a greta array. If you don't
see it showing up in the output, you may need to use `as_data` to
convert it to a greta array before defining the model.

## Examples

``` r
if (FALSE) { # \dontrun{
# build a greta model
n <- 10
y <- rnorm(n)
y <- as_data(y)

library(greta)
sd <- lognormal(1, 2)
mu <- normal(0, 1, dim = n)
distribution(y) <- normal(mu, sd)
m <- model(mu, sd)

# simulate one random draw of y, mu and sd from the model prior:
sims <- simulate(m)

# 100 simulations of y, mu and sd
sims <- simulate(m, nsim = 100)
} # }
# nolint start
```
