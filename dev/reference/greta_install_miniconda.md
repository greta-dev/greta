# Installs miniconda

This installs miniconda using
[`reticulate::install_miniconda()`](https://rstudio.github.io/reticulate/reference/install_miniconda.html)
inside
[`callr::r_process_options()`](https://callr.r-lib.org/reference/r_process_options.html).
Used internally by
[`install_greta_deps()`](https://greta-dev.github.io/greta/dev/reference/install_greta_deps.md).
We mostly recommend users use
[`install_greta_deps()`](https://greta-dev.github.io/greta/dev/reference/install_greta_deps.md)
to manage their python dependency installation.

## Usage

``` r
greta_install_miniconda(timeout = 5)
```

## Arguments

- timeout:

  time (minutes) until installation stops. Default is 5 minutes.

## Value

nothing - installs miniconda.
