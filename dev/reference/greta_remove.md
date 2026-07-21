# Remove greta's Python dependencies

A single entry point for removing greta's Python bits, which is useful
when debugging a greta installation and you want to get back to a "clean
slate". Use the `what` argument to choose how much to remove.

## Usage

``` r
greta_remove(
  what = c("all", "env", "miniconda", "uv_cache", "preference", "deps"),
  ask = interactive()
)
```

## Arguments

- what:

  What to remove. One of:

  - `"all"` (default): the `"greta-env-tf2"` conda environment,
    miniconda, reticulate's uv cache, and greta's stored preferences
    (the Python backend set via
    [`greta_set_python()`](https://greta-dev.github.io/greta/dev/reference/greta_set_python.md),
    and the dependency versions set via
    [`greta_set_deps()`](https://greta-dev.github.io/greta/dev/reference/greta_set_deps.md)).
    This is a "nuclear" reset that asks once, then removes everything it
    finds.

  - `"env"`: the `"greta-env-tf2"` conda environment.

  - `"miniconda"`: the miniconda installation.

  - `"uv_cache"`: reticulate's uv cache. Note this cache is shared by
    all R packages that use reticulate's uv (it is not greta-specific);
    a system-wide uv cache is left untouched.

  - `"preference"`: greta's stored Python backend preference (set via
    [`greta_set_python()`](https://greta-dev.github.io/greta/dev/reference/greta_set_python.md)).

  - `"deps"`: greta's stored dependency versions (set via
    [`greta_set_deps()`](https://greta-dev.github.io/greta/dev/reference/greta_set_deps.md)).

- ask:

  Ask for confirmation? Default is
  [`interactive()`](https://rdrr.io/r/base/interactive.html).

## Value

Invisibly, `TRUE` if anything was removed, otherwise `FALSE`.

## See also

[`reinstall_greta_deps()`](https://greta-dev.github.io/greta/dev/reference/install_greta_deps.md),
[`greta_set_python()`](https://greta-dev.github.io/greta/dev/reference/greta_set_python.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# remove everything (nuclear reset)
greta_remove()

# remove only the conda environment
greta_remove("env")

# clear the stored Python preference
greta_remove("preference")

# clear the stored dependency versions
greta_remove("deps")
} # }
```
