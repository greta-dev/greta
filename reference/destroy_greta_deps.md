# Remove greta dependencies and remove miniconda

Sometimes when installing greta you might encounter an error and the
best thing to do is start from a clean slate. This function does two
things:

1.  Removes the "greta-env-tf2" with
    [`remove_greta_env()`](https://greta-dev.github.io/greta/reference/reinstallers.md)

2.  Removes the miniconda installation with
    [`remove_miniconda()`](https://greta-dev.github.io/greta/reference/reinstallers.md)

## Usage

``` r
destroy_greta_deps(ask = interactive())
```

## Arguments

- ask:

  Ask for confirmation? Default is
  [`interactive()`](https://rdrr.io/r/base/interactive.html).

## Value

Invisibly, TRUE if anything was removed, otherwise FALSE.
