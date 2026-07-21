# transformation functions for greta arrays

transformations for greta arrays, which may also be used as inverse link
functions. Also see
[operators](https://greta-dev.github.io/greta/dev/reference/operators.md)
and
[functions](https://greta-dev.github.io/greta/dev/reference/functions.md).

## Usage

``` r
iprobit(x)

ilogit(x)

icloglog(x)

icauchit(x)

log1pe(x)

imultilogit(x)
```

## Arguments

- x:

  a real-valued (i.e. values ranging from -Inf to Inf) greta array to
  transform to a constrained value

## Value

A `greta_array` with the transformation applied.

## Details

greta does not allow you to state the transformation/link on the left
hand side of an assignment, as is common in the BUGS and STAN modelling
languages. That's because the same syntax has a very different meaning
in R, and can only be applied to objects that are already in existence.
The inverse forms of the common link functions (prefixed with an 'i')
can be used instead.

The `log1pe` inverse link function is equivalent to `log(1 + exp(x))`,
yielding a positive transformed parameter. Unlike the log
transformation, this transformation is approximately linear for x \> 1.
i.e. when \\x \> 1\\, \\y\\ is approximately \\x\\

`imultilogit` expects an n-by-m greta array, and returns an n-by-(m+1)
greta array of positive reals whose rows sum to one. This is equivalent
adding a final column of 0s and then running the softmax function widely
used in machine learning.

## Examples

``` r
if (FALSE) { # \dontrun{

x1 <- normal(1, 3, dim = 10)

# transformation to the unit interval
p1 <- iprobit(x1)
p2 <- ilogit(x1)
p3 <- icloglog(x1)
p4 <- icauchit(x1)

# and to positive reals
y <- log1pe(x1)

# transform from 10x3 to 10x4, where rows are a complete set of
# probabilities
x2 <- normal(1, 3, dim = c(10, 3))
z <- imultilogit(x2)
} # }
```
