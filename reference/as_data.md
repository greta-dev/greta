# convert other objects to greta arrays

define an object in an R session as a data greta array for use as data
in a greta model.

## Usage

``` r
as_data(x)
```

## Arguments

- x:

  an R object that can be coerced to a greta_array (see details).

## Details

`as_data()` can currently convert R objects to greta_arrays if they are
numeric or logical vectors, matrices or arrays; or if they are
dataframes with only numeric (including integer) or logical elements.
Logical elements are always converted to numerics. R objects cannot be
converted if they contain missing (`NA`) or infinite (`-Inf` or `Inf`)
values.

## Examples

``` r
if (FALSE) { # \dontrun{

# numeric/integer/logical vectors, matrices and arrays can all be coerced to
# data greta arrays

vec <- rnorm(10)
mat <- matrix(seq_len(3 * 4), nrow = 3)
arr <- array(sample(c(TRUE, FALSE), 2 * 2 * 2, replace = TRUE),
  dim = c(2, 2, 2)
)
(a <- as_data(vec))
(b <- as_data(mat))
(c <- as_data(arr))

# dataframes can also be coerced, provided all the columns are numeric,
# integer or logical
df <- data.frame(
  x1 = rnorm(10),
  x2 = sample(1L:10L),
  x3 = sample(c(TRUE, FALSE), 10, replace = TRUE)
)
(d <- as_data(df))
} # }
```
