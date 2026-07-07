# Choose the Python environment greta uses

greta runs on Python (via TensorFlow and TensorFlow Probability). By
default it uses [`uv`](https://docs.astral.sh/uv/) (via the reticulate R
package) to install a compatible Python, TensorFlow, and TensorFlow
Probability automatically on first use. These helper functions let you
persistently switch greta to a different Python environment - for
example a conda environment created by
[`install_greta_deps()`](https://greta-dev.github.io/greta/reference/install_greta_deps.md),
or your own Python. `greta_reset_python()` clears the stored choice,
returning to greta's automatic resolution.

## Usage

``` r
greta_set_python_uv()

greta_set_python_conda_env(name = "greta-env-tf2")

greta_set_python_path(path)

greta_reset_python()
```

## Arguments

- name:

  Name of the conda environment to use. Defaults to `"greta-env-tf2"`,
  the environment created by
  [`install_greta_deps()`](https://greta-dev.github.io/greta/reference/install_greta_deps.md).

- path:

  Path to a Python executable.

## Value

Invisibly, the stored preference (`NULL` for `greta_reset_python()`).

## Details

greta resolves which Python to use, in this order:

1.  The `RETICULATE_PYTHON` environment variable, if set (usually in
    `~/.Renviron`, your `.Rprofile`, or your shell environment). This
    always wins: it takes precedence over any stored preference.

2.  Your stored preference, set with `greta_set_python_uv()`,
    `greta_set_python_conda_env()`, or `greta_set_python_path()`.

3.  An auto-detected `"greta-env-tf2"` conda environment (created by
    [`install_greta_deps()`](https://greta-dev.github.io/greta/reference/install_greta_deps.md)) -
    kept so setups from older greta versions keep working after
    upgrading.

4.  Otherwise, the uv-managed environment (the default as of greta
    0.6.0): reticulate installs a compatible Python, TensorFlow, and
    TensorFlow Probability automatically on first use. No setup is
    needed - this happens "automagically".

To check which Python greta is currently using, and which it will use
after a restart, call
[`greta_sitrep()`](https://greta-dev.github.io/greta/reference/greta_sitrep.md).

If a stored preference appears to be ignored, `RETICULATE_PYTHON` is
usually why: remove it from wherever it is set (for example
`~/.Renviron`), then restart R. Note that
[`Sys.unsetenv()`](https://rdrr.io/r/base/Sys.setenv.html) within a
session is not enough, as the choice is applied when greta loads.

Your choice is stored under `tools::R_user_dir("greta", "config")` and
applied the next time greta is loaded, so you will need to **restart R**
for it to take effect.

## Examples

``` r
if (FALSE) { # \dontrun{
# use the uv-managed environment (the default)
greta_set_python_uv()

# use the conda environment from install_greta_deps()
greta_set_python_conda_env()

# use a specific Python
greta_set_python_path("/path/to/python")

# clear the stored choice and return to automatic resolution
greta_reset_python()
} # }
```
