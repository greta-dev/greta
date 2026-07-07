# create data greta arrays

These structures can be used to set up more complex models. For example,
scalar parameters can be embedded in a greta array by first creating a
greta array with `zeros()` or `ones()`, and then embedding the parameter
value using greta's replacement syntax.

## Usage

``` r
zeros(...)

ones(...)

greta_array(data = 0, dim = length(data))
```

## Arguments

- ...:

  dimensions of the greta arrays to create

- data:

  a vector giving data to fill the greta array. Other object types are
  coerced by [`as.vector()`](https://rdrr.io/r/base/vector.html).

- dim:

  an integer vector giving the dimensions for the greta array to be
  created.

## Value

a greta array object

## Details

`greta_array` is a convenience function to create an R array with
[`array()`](https://rdrr.io/r/base/array.html) and then coerce it to a
greta array. I.e. when passed something that can be coerced to a numeric
array, it is equivalent to `as_data(array(data, dim))`.

If `data` is a greta array and dim is different than `dim(data)`, a
reshaped greta array is returned. This is equivalent to:
`dim(data) <- dim`.

## Examples

``` r
if (FALSE) { # \dontrun{

# a 3 row, 4 column greta array of 0s
z <- zeros(3, 4)

# a 3x3x3 greta array of 1s
z <- ones(3, 3, 3)

# a 2x4 greta array filled with pi
z <- greta_array(pi, dim = c(2, 4))

# a 3x3x3 greta array filled with 1, 2, and 3
z <- greta_array(1:3, dim = c(3, 3, 3))
} # }
```
