# Compute the Cholesky Factor of a Matrix

Compute the Cholesky Factor of a Matrix

## Usage

``` r
# S3 method for class 'greta_array'
chol(x, ..., force_cholesky = FALSE)
```

## Arguments

- x:

  an object for which a method exists. The default method applies to
  numeric (or logical) symmetric, positive-definite matrices.

- ...:

  further arguments pass to or from methods.

- force_cholesky:

  Whether to force cholesky computation. Currently used as a workaround
  to ensure cholesky is calculated properly, and may result in code that
  uses [`chol()`](https://rdrr.io/r/base/chol.html) to be slow. Default
  is TRUE. Can change to FALSE, but may encounter issues in
  <https://github.com/greta-dev/greta/issues/585>.

## Value

An upper-triangular `greta_array` representing the Cholesky factor of
`x`.
