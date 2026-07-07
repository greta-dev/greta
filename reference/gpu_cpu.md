# Set GPU or CPU usage

These functions set the use of CPU or GPU inside of greta. They simply
return either "GPU" or "CPU", but in the future may handle more
complexity. These functions are passed to `compute_options` inside of a
few functions:
[`mcmc()`](https://greta-dev.github.io/greta/reference/inference.md),
[`opt()`](https://greta-dev.github.io/greta/reference/inference.md), and
[`calculate()`](https://greta-dev.github.io/greta/reference/calculate.md).

## Usage

``` r
gpu_only()

cpu_only()
```
