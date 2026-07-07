# Remove all greta Python dependencies (nuclear reset)

A "nuclear" reset that returns greta to a blank slate. Asks once for
confirmation, it removes:

1.  the 'greta-env-tf2' conda environment, with
    [`remove_greta_env()`](https://greta-dev.github.io/greta/reference/reinstallers.md)

2.  the miniconda installation, with
    [`remove_miniconda()`](https://greta-dev.github.io/greta/reference/reinstallers.md)

3.  reticulate's managed uv cache (if any), with
    [`remove_reticulate_uv_cache()`](https://greta-dev.github.io/greta/reference/reinstallers.md)

4.  greta's stored Python preference (set via
    [`greta_set_python_uv()`](https://greta-dev.github.io/greta/reference/greta_set_python.md)
    and friends)

## Usage

``` r
greta_remove_all_deps(ask = interactive())
```

## Arguments

- ask:

  Ask for confirmation? Default is
  [`interactive()`](https://rdrr.io/r/base/interactive.html).

## Value

Invisibly, TRUE if anything was removed, otherwise FALSE.

## Details

This is broader than
[`destroy_greta_deps()`](https://greta-dev.github.io/greta/reference/destroy_greta_deps.md),
which only removes the conda environment and miniconda. Note that a
system-wide uv cache is *not* removed (it is managed by uv itself, and
shared beyond reticulate). After running this, restart R; greta
reinstalls what it needs on next use.

## See also

[reinstallers](https://greta-dev.github.io/greta/reference/reinstallers.md),
[`destroy_greta_deps()`](https://greta-dev.github.io/greta/reference/destroy_greta_deps.md)

## Examples

``` r
if (FALSE) { # \dontrun{
greta_remove_all_deps()
} # }
```
