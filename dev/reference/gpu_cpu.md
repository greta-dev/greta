# Set GPU or CPU usage

These functions set the use of CPU or GPU inside of greta. They simply
return either "GPU" or "CPU", but in the future may handle more
complexity. These functions are passed to `compute_options` inside of a
few functions:
[`mcmc()`](https://greta-dev.github.io/greta/dev/reference/inference.md),
[`opt()`](https://greta-dev.github.io/greta/dev/reference/inference.md),
and
[`calculate()`](https://greta-dev.github.io/greta/dev/reference/calculate.md).

## Usage

``` r
gpu_only()

cpu_only()
```

## Value

A single character string, `"GPU"` or `"CPU"`.

## Examples

``` r
gpu_only()
#> [1] "GPU"
cpu_only()
#> [1] "CPU"
```
