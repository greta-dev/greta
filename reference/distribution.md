# define a distribution over data

`distribution` defines probability distributions over observed data,
e.g. to set a model likelihood.

## Usage

``` r
distribution(greta_array) <- value

distribution(greta_array)
```

## Arguments

- greta_array:

  a data greta array. For the assignment method it must not already have
  a probability distribution assigned

- value:

  a greta array with a distribution (see
  [`distributions()`](https://greta-dev.github.io/greta/reference/distributions.md))

## Details

The extract method returns the greta array if it has a distribution, or
`NULL` if it doesn't. It has no real use-case, but is included for
completeness

## Examples

``` r
if (FALSE) { # \dontrun{

# define a model likelihood

# observed data and mean parameter to be estimated
# (explicitly coerce data to a greta array so we can refer to it later)
y <- as_data(rnorm(5, 0, 3))

mu <- uniform(-3, 3)

# define the distribution over y (the model likelihood)
distribution(y) <- normal(mu, 1)

# get the distribution over y
distribution(y)
} # }
```
