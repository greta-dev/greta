# Cholesky Factor to Symmetric Matrix

Evaluate `t(x) \%*\% x` efficiently, where `x` is the (upper-triangular)
Cholesky factor of a symmetric, positive definite square matrix. I.e. it
is the inverse of `chol`.

## Usage

``` r
chol2symm(x)
```

## Arguments

- x:

  a square, upper triangular matrix representing the Cholesky factor of
  a symmetric, positive definite square matrix

## Value

A symmetric, positive-definite matrix or `greta_array`.

## Examples

``` r
# a symmetric, positive definite square matrix
y <- rWishart(1, 4, diag(3))[, , 1]
y
#>              [,1]         [,2]       [,3]
#> [1,]  0.550668691 -0.004134288  0.8522024
#> [2,] -0.004134288  0.494051727 -1.2868919
#> [3,]  0.852202424 -1.286891918  6.7100410
u <- chol(y)
u
#>           [,1]         [,2]      [,3]
#> [1,] 0.7420705 -0.005571287  1.148412
#> [2,] 0.0000000  0.702866052 -1.821818
#> [3,] 0.0000000  0.000000000  1.439504
chol2symm(u)
#>              [,1]         [,2]       [,3]
#> [1,]  0.550668691 -0.004134288  0.8522024
#> [2,] -0.004134288  0.494051727 -1.2868919
#> [3,]  0.852202424 -1.286891918  6.7100410
identical(y, chol2symm(u))
#> [1] TRUE
identical(chol2symm(u), t(u) %*% u)
#> [1] TRUE
if (FALSE) { # \dontrun{
u_greta <- cholesky_variable(3)
y_greta <- chol2symm(u)
} # }
```
