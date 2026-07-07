# Helpers to remove, and reinstall python environments and miniconda

This can be useful when debugging greta installation to get to "clean
slate". There are five functions:

## Usage

``` r
remove_greta_env(ask = interactive())

reinstall_greta_env(timeout = 5, ask = interactive())

remove_miniconda(ask = interactive())

reinstall_miniconda(timeout = 5, ask = interactive())

remove_reticulate_uv_cache(ask = interactive())
```

## Arguments

- ask:

  Ask for confirmation? Default is
  [`interactive()`](https://rdrr.io/r/base/interactive.html).

- timeout:

  time in minutes to wait until timeout (default is 5 minutes)

## Value

Invisibly, TRUE if anything was removed, otherwise FALSE.

## Details

- `remove_greta_env()` removes the 'greta-env-tf2' conda environment

- `remove_miniconda()` removes miniconda installation

- `remove_reticulate_uv_cache()` removes reticulate's managed uv cache.
  Note this cache is shared by all R packages that use reticulate's uv
  (it is not greta-specific), and a system-wide uv cache is left
  untouched.

- `reinstall_greta_env()` remove 'greta-env-tf2' and reinstall it using
  [`greta_create_conda_env()`](https://greta-dev.github.io/greta/reference/greta_create_conda_env.md)
  (which is used internally).

- `reinstall_miniconda()` removes miniconda and reinstalls it using
  [`greta_install_miniconda()`](https://greta-dev.github.io/greta/reference/greta_install_miniconda.md)
  (which is used internally)

To remove everything at once, see
[`greta_remove_all_deps()`](https://greta-dev.github.io/greta/reference/greta_remove_all_deps.md).

## See also

[`destroy_greta_deps()`](https://greta-dev.github.io/greta/reference/destroy_greta_deps.md),
[`greta_remove_all_deps()`](https://greta-dev.github.io/greta/reference/greta_remove_all_deps.md)

## Examples

``` r
if (FALSE) { # \dontrun{
remove_greta_env()
remove_miniconda()
reinstall_greta_env()
reinstall_miniconda()
} # }
```
