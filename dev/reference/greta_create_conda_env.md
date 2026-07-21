# Create conda environment for greta

This function runs
[`reticulate::conda_create()`](https://rstudio.github.io/reticulate/reference/conda-tools.html)
inside
[`callr::r_process_options()`](https://callr.r-lib.org/reference/r_process_options.html),
to create the conda environment, "greta-env-tf2". This is used within
[`install_greta_deps()`](https://greta-dev.github.io/greta/dev/reference/install_greta_deps.md)
as part of setting up python dependencies. It uses a version of python
that is compatible with the versions of tensorflow and
tensorflow-probability, which is established with
[`greta_deps_spec()`](https://greta-dev.github.io/greta/dev/reference/greta_deps_spec.md).
We mostly recommend users use
[`install_greta_deps()`](https://greta-dev.github.io/greta/dev/reference/install_greta_deps.md)
to manage their python dependency installation.

## Usage

``` r
greta_create_conda_env(timeout = 5, deps = greta_deps_spec())
```

## Arguments

- timeout:

  time (minutes) until installation stops. Default is 5 minutes.

- deps:

  dependency specification, see
  [`greta_deps_spec()`](https://greta-dev.github.io/greta/dev/reference/greta_deps_spec.md)
  for more details.

## Value

nothing - creates a conda environment for a specific python version
