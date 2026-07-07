# extract, replace and combine greta arrays

Generic methods to extract and replace elements of greta arrays, or to
combine greta arrays.

## Arguments

- x:

  a greta array

- i, j:

  indices specifying elements to extract or replace

- n:

  a single integer, as in
  [`utils::head()`](https://rdrr.io/r/utils/head.html) and
  [`utils::tail()`](https://rdrr.io/r/utils/head.html)

- nrow, ncol:

  optional dimensions for the resulting greta array when x is not a
  matrix.

- value:

  for `[<-` a greta array to replace elements, for `dim<-` either NULL
  or a numeric vector of dimensions

- ...:

  either further indices specifying elements to extract or replace
  (`[`), or multiple greta arrays to combine (`cbind()`, `rbind()` &
  `c()`), or additional arguments (`rep()`,
  [`head()`](https://rdrr.io/r/utils/head.html),
  [`tail()`](https://rdrr.io/r/utils/head.html))

- drop, recursive:

  generic arguments that are ignored for greta arrays

## Details

[`diag()`](https://greta-dev.github.io/greta/reference/overloaded.md)
can be used to extract or replace the diagonal part of a square and
two-dimensional greta array, but it cannot be used to create a
matrix-like greta array from a scalar or vector-like greta array. A
static diagonal matrix can always be created with e.g. `diag(3)`, and
then converted into a greta array.

Also note that since R 4.0.0, `head` and `tail` methods for arrays
changed to print a vector rather than maintain the array structure. The
`greta` package supports both methods, and will do so based on which
version of R you are using.

## Usage


    # extract
    x[i]
    x[i, j, ..., drop = FALSE]
    head(x, n = 6L, ...)
    tail(x, n = 6L, ...)
    diag(x, nrow, ncol)

    # replace
    x[i] <- value
    x[i, j, ...] <- value
    diag(x) <- value

    # combine
    cbind(...)
    rbind(...)
    abind(...)
    c(..., recursive = FALSE)
    rep(x, times, ..., recursive = FALSE)

    # get and set dimensions
    length(x)
    dim(x)
    dim(x) <- value

## Examples

``` r
if (FALSE) { # \dontrun{

x <- as_data(matrix(1:12, 3, 4))

# extract and replace
x[1:3, ]
x[, 2:4] <- 1:9
e <- diag(x)
diag(x) <- e + 1

# combine
cbind(x[, 2], x[, 1])
rbind(x[1, ], x[3, ])
abind(x[1, ], x[3, ], along = 1)
c(x[, 1], x)
rep(x[, 2], times = 3)
} # }
```
