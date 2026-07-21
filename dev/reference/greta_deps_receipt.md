# Capture greta python dependencies.

To assist with capturing and sharing python dependencies, we provide a
way to capture the dependencies currently used. Unlike
[`greta_deps_spec()`](https://greta-dev.github.io/greta/dev/reference/greta_deps_spec.md),
the receipt records the versions actually installed and is **not**
validated against the versions greta supports - so it will faithfully
report, for example, a TensorFlow version newer than greta's supported
range.

## Usage

``` r
greta_deps_receipt()
```

## Value

[`greta_deps_spec()`](https://greta-dev.github.io/greta/dev/reference/greta_deps_spec.md)
object

## Examples

``` r
if (FALSE) { # \dontrun{
my_deps <- greta_deps_receipt()
} # }
```
