# Specify python dependencies for greta

A helper function for specifying versions of Tensorflow (TF), Tensorflow
Probability (TFP), and Python. Defaulting to 2.21.0, 0.25.0, and 3.12,
respectively.

## Usage

``` r
greta_deps_spec(
  tf_version = "2.21.0",
  tfp_version = "0.25.0",
  python_version = "3.12"
)
```

## Arguments

- tf_version:

  character. TensorFlow version, in the format major.minor.patch.
  Default is 2.21.0.

- tfp_version:

  Character. Tensorflow probability (TFP) version major.minor.patch.
  Default is 0.25.0.

- python_version:

  Character. Python version in format major.minor.patch. Default is
  3.12.

## Value

data frame of valid dependencies

## Supported versions

TensorFlow 2.18.0 to 2.21.0, with TensorFlow Probability fixed at
0.25.0. Anything else is rejected, for either of them.

Python 3.9 to 3.12 is accepted, which is what some supported TensorFlow
can use. Which of them work with the TensorFlow you picked is narrower,
and left to the resolver: TensorFlow 2.21.0 is not built for Python 3.9,
so a combination greta accepts here can still fail at install time.

For why the range is this narrow, and why TensorFlow Probability is not
a choice, see the "I need specific dependency versions" section of
[`vignette("installation", package = "greta")`](https://greta-dev.github.io/greta/dev/articles/installation.md).

Calling `greta_deps_spec()` with no arguments returns greta's current
default (recommended) versions, and is the supported way to query them -
for example `greta_deps_spec()$tf_version` for the default TensorFlow
version.

## Examples

``` r
greta_deps_spec()
#>   tf_version tfp_version python_version
#> 1     2.21.0      0.25.0           3.12
# every combination below is one the weekly install check passes on
greta_deps_spec(tf_version = "2.18.0")
#>   tf_version tfp_version python_version
#> 1     2.18.0      0.25.0           3.12
greta_deps_spec(tf_version = "2.19.0")
#>   tf_version tfp_version python_version
#> 1     2.19.0      0.25.0           3.12
greta_deps_spec(tf_version = "2.20.0")
#>   tf_version tfp_version python_version
#> 1     2.20.0      0.25.0           3.12
greta_deps_spec(
  tf_version = "2.19.0",
  tfp_version = "0.25.0",
  python_version = "3.12"
)
#>   tf_version tfp_version python_version
#> 1     2.19.0      0.25.0           3.12
# these fail: greta supports TensorFlow 2.18.0 up to the version it pins
if (FALSE) { # \dontrun{
greta_deps_spec(tf_version = "2.17.0")
greta_deps_spec(tf_version = "2.99.0")
} # }
```
