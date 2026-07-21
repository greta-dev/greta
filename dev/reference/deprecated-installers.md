# Deprecated removal and reinstallation helpers

**\[deprecated\]**

These functions have been consolidated into
[`greta_remove()`](https://greta-dev.github.io/greta/dev/reference/greta_remove.md)
(for removal) and
[`reinstall_greta_deps()`](https://greta-dev.github.io/greta/dev/reference/install_greta_deps.md)
(for reinstallation). They remain exported and functional but will be
removed in a future release.

## Usage

``` r
remove_greta_env(ask = interactive())

remove_miniconda(ask = interactive())

remove_reticulate_uv_cache(ask = interactive())

greta_remove_all_deps(ask = interactive())

destroy_greta_deps(ask = interactive())

reinstall_greta_env(timeout = 5, ask = interactive())

reinstall_miniconda(timeout = 5, ask = interactive())
```

## Arguments

- ask:

  Ask for confirmation? Default is
  [`interactive()`](https://rdrr.io/r/base/interactive.html).

- timeout:

  time in minutes to wait until timeout (default is 5 minutes).

## Value

Invisibly, `TRUE` if anything was removed, otherwise `FALSE`.

## See also

[`greta_remove()`](https://greta-dev.github.io/greta/dev/reference/greta_remove.md),
[`reinstall_greta_deps()`](https://greta-dev.github.io/greta/dev/reference/install_greta_deps.md)
