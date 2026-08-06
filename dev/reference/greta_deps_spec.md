# Specify python dependencies for greta

A helper function for specifying versions of Tensorflow (TF), Tensorflow
Probability (TFP), and Python. Defaulting to 2.21.0, 0.25.0, and 3.12,
respectively. greta checks it supports the TF version (versions newer
than greta has been tested against are rejected); compatible TFP and
Python versions are resolved at install time by uv (or, for a conda
environment, by conda/pip). greta checks a range of TensorFlow versions
weekly; open the most recent run from
<https://github.com/greta-dev/greta/actions/workflows/install-check.yaml>
to see which combinations were tried and what each resolved to.

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

## Details

Calling `greta_deps_spec()` with no arguments returns greta's current
default (recommended) versions, and is the supported way to query them -
for example `greta_deps_spec()$tf_version` for the default TensorFlow
version.

## Examples

``` r
greta_deps_spec()
#>   tf_version tfp_version python_version
#> 1     2.21.0      0.25.0           3.12
greta_deps_spec(tf_version = "2.15.1")
#>   tf_version tfp_version python_version
#> 1     2.15.1      0.25.0           3.12
greta_deps_spec(tf_version = "2.15.0")
#>   tf_version tfp_version python_version
#> 1     2.15.0      0.25.0           3.12
greta_deps_spec(tf_version = "2.15.1", tfp_version = "0.23.0")
#>   tf_version tfp_version python_version
#> 1     2.15.1      0.23.0           3.12
greta_deps_spec(tf_version = "2.15.0", tfp_version = "0.23.0")
#>   tf_version tfp_version python_version
#> 1     2.15.0      0.23.0           3.12
greta_deps_spec(tf_version = "2.15.1", python_version = "3.10")
#>   tf_version tfp_version python_version
#> 1     2.15.1      0.25.0           3.10
greta_deps_spec(tf_version = "2.15.0", python_version = "3.10")
#>   tf_version tfp_version python_version
#> 1     2.15.0      0.25.0           3.10
greta_deps_spec(
  tf_version = "2.14.0",
  tfp_version = "0.22.1",
  python_version = "3.10"
  )
#>   tf_version tfp_version python_version
#> 1     2.14.0      0.22.1           3.10
# this will fail: greta has not been tested above the version it pins
if (FALSE) { # \dontrun{
greta_deps_spec(tf_version = "2.99.0")
  } # }
```
