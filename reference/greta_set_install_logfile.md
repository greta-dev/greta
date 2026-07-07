# Set logfile path when installing greta

To help debug greta installation, you can save all installation output
to a single logfile.

## Usage

``` r
greta_set_install_logfile(path)
```

## Arguments

- path:

  valid path to logfile - should end with `.html` so you can benefit
  from the html rendering.

## Value

nothing - sets an environment variable for use with
[`install_greta_deps()`](https://greta-dev.github.io/greta/reference/install_greta_deps.md).
