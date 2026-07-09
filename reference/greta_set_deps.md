# Choose the dependency versions greta installs

Persistently choose which versions of TensorFlow, TensorFlow
Probability, and Python greta uses, independently of *where* they are
installed (see
[`greta_set_python()`](https://greta-dev.github.io/greta/reference/greta_set_python.md)
for that). The stored versions are used by:

- the managed (uv) environment, which installs them automatically on
  first use after a restart, and

- [`install_greta_deps()`](https://greta-dev.github.io/greta/reference/install_greta_deps.md),
  as its default `deps` argument when building a conda environment.

Most users never need this: greta's defaults (TensorFlow 2.15.1,
TensorFlow Probability 0.23.0, Python 3.11) are the newest versions
greta supports.

To clear the stored versions and return to the defaults, use
[`greta_remove()`](https://greta-dev.github.io/greta/reference/greta_remove.md)`("deps")`.

## Usage

``` r
greta_set_deps(deps = greta_deps_spec())
```

## Arguments

- deps:

  object created with
  [`greta_deps_spec()`](https://greta-dev.github.io/greta/reference/greta_deps_spec.md),
  which checks that the TensorFlow version is one greta supports.

## Value

Invisibly, the stored
[`greta_deps_spec()`](https://greta-dev.github.io/greta/reference/greta_deps_spec.md).

## Details

Your choice is stored under `tools::R_user_dir("greta", "config")` and
applied the next time greta is loaded, so you will need to **restart R**
for it to take effect. Changing versions on the managed (uv) backend may
require internet access on the next load, to download the newly
requested versions.

## See also

[`greta_set_python()`](https://greta-dev.github.io/greta/reference/greta_set_python.md),
[`greta_deps_spec()`](https://greta-dev.github.io/greta/reference/greta_deps_spec.md),
[`install_greta_deps()`](https://greta-dev.github.io/greta/reference/install_greta_deps.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# pin an older TensorFlow for the managed (uv) environment
greta_set_deps(greta_deps_spec(
  tf_version = "2.14.0",
  tfp_version = "0.22.1",
  python_version = "3.10"
))

# clear the stored versions and return to greta's defaults
greta_remove("deps")
} # }
```
