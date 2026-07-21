# arithmetic, logical and relational operators for greta arrays

This is a list of currently implemented arithmetic, logical and
relational operators to combine greta arrays into probabilistic models.
Also see
[functions](https://greta-dev.github.io/greta/dev/reference/functions.md)
and
[transforms](https://greta-dev.github.io/greta/dev/reference/transforms.md).

## Value

A `greta_array`, the result of applying the operator.

## Details

greta's operators are used just like R's the standard arithmetic,
logical and relational operators, but they return other greta arrays.
Since the operations are only carried during sampling, the greta array
objects have unknown values.

## Usage


     # arithmetic operators
     -x
     x + y
     x - y
     x * y
     x / y
     x ^ y
     x %% y
     x %/% y
     x %*% y

     # logical operators
     !x
     x & y
     x | y

     # relational operators
     x < y
     x > y
     x <= y
     x >= y
     x == y
     x != y
     

## Examples

``` r
if (FALSE) { # \dontrun{

x <- as_data(-1:12)

# arithmetic
a <- x + 1
b <- 2 * x + 3
c <- x %% 2
d <- x %/% 5

# logical
e <- (x > 1) | (x < 1)
f <- e & (x < 2)
g <- !f

# relational
h <- x < 1
i <- (-x) >= x
j <- h == x
} # }
```
