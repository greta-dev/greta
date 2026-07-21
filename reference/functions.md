# functions for greta arrays

This is a list of functions (mostly from base R) that are currently
implemented to transform greta arrays. Also see
[operators](https://greta-dev.github.io/greta/reference/operators.md)
and
[transforms](https://greta-dev.github.io/greta/reference/transforms.md).

## Value

A `greta_array`, with the function applied elementwise or as appropriate
for the function called.

## Details

TensorFlow only enables rounding to integers, so
[`round()`](https://rdrr.io/r/base/Round.html) will error if `digits` is
set to anything other than `0`.

Any additional arguments to
[`chol()`](https://rdrr.io/r/base/chol.html), `chol2inv`,
[`solve()`](https://rdrr.io/r/base/solve.html), and
[`log()`](https://rdrr.io/r/base/Log.html) will be ignored, see the
TensorFlow documentation for details of these routines.

[`sweep()`](https://greta-dev.github.io/greta/reference/overloaded.md)
only works on two-dimensional greta arrays (so `MARGIN` can only be
either 1 or 2), and only for subtraction, addition, division and
multiplication.

[`tapply()`](https://greta-dev.github.io/greta/reference/overloaded.md)
works on column vectors (2D greta arrays with one column), and `INDEX`
cannot be a greta array. Currently five functions are available, and
arguments passed to ... are ignored.

[`cospi()`](https://rdrr.io/r/base/Trig.html),
[`sinpi()`](https://rdrr.io/r/base/Trig.html), and
[`tanpi()`](https://rdrr.io/r/base/Trig.html) do not use the
computationally more stable routines to compute `cos(x * pi)` etc. that
are available in R under some operating systems. Similarly
[`trigamma()`](https://rdrr.io/r/base/Special.html) uses TensorFlow's
polygamma function, resulting in lower precision than R's equivalent.

## Usage



     # logarithms and exponentials
     log(x)
     exp(x)
     log1p(x)
     expm1(x)

     # miscellaneous mathematics
     abs(x)
     mean(x)
     sqrt(x)
     sign(x)

     # rounding of numbers
     ceiling(x)
     floor(x)
     round(x, digits = 0)

     # trigonometry
     cos(x)
     sin(x)
     tan(x)
     acos(x)
     asin(x)
     atan(x)
     cosh(x)
     sinh(x)
     tanh(x)
     acosh(x)
     asinh(x)
     atanh(x)
     cospi(x)
     sinpi(x)
     tanpi(x)

     # special mathematical functions
     lgamma(x)
     digamma(x)
     trigamma(x)
     choose(n, k)
     lchoose(n, k)

     # matrix operations
     t(x)
     chol(x, ...)
     chol2inv(x, ...)
     cov2cor(V)
     solve(a, b, ...)
     kronecker(X, Y, FUN = c('*', '/', '+', '-'))

     # reducing operations
     sum(..., na.rm = TRUE)
     prod(..., na.rm = TRUE)
     min(..., na.rm = TRUE)
     max(..., na.rm = TRUE)

     # cumulative operations
     cumsum(x)
     cumprod(x)
     cummax(x)
     cummin(x)

     # solve an upper or lower triangular system
     backsolve(r, x, k = ncol(r), upper.tri = TRUE,
               transpose = FALSE)
     forwardsolve(l, x, k = ncol(l), upper.tri = FALSE,
                  transpose = FALSE)

     # miscellaneous operations
     aperm(x, perm)
     apply(x, MARGIN, FUN = c("sum", "max", "mean", "min",
                              "prod", "cumsum", "cumprod"))
     sweep(x, MARGIN, STATS, FUN = c('-', '+', '/', '*'))
     tapply(X, INDEX, FUN = c("sum", "max", "mean", "min", "prod"), ...)

## Examples

``` r
if (FALSE) { # \dontrun{

x <- as_data(matrix(1:9, nrow = 3, ncol = 3))
a <- log(exp(x))
b <- log1p(expm1(x))
c <- sign(x - 5)
d <- abs(x - 5)

z <- t(a)

y <- sweep(x, 1, e, "-")
} # }
```
