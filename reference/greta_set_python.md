# Choose the Python environment greta uses

greta runs on Python (via TensorFlow and TensorFlow Probability). By
default it uses [`uv`](https://docs.astral.sh/uv/) (via the reticulate R
package) to install a compatible Python, TensorFlow, and TensorFlow
Probability automatically on first use. `greta_set_python()`
persistently selects which Python environment greta uses: the managed
(uv) environment, a conda environment (for example one created by
[`install_greta_deps()`](https://greta-dev.github.io/greta/reference/install_greta_deps.md)),
or your own Python. `greta_reset_python()` clears the stored choice,
returning to greta's automatic resolution.

To choose which *versions* of TensorFlow and TensorFlow Probability the
managed (uv) environment installs, see
[`greta_set_deps()`](https://greta-dev.github.io/greta/reference/greta_set_deps.md) -
dependency versions are separate from the choice of Python environment.

## Usage

``` r
greta_set_python(backend = c("uv", "conda", "path"), path = NULL, name = NULL)

greta_reset_python()
```

## Arguments

- backend:

  Which Python environment to use. One of:

  - `"uv"` (default): the managed (uv) environment. reticulate installs
    a compatible Python, TensorFlow, and TensorFlow Probability
    automatically on first use.

  - `"conda"`: a conda environment, named by `name`.

  - `"path"`: a specific Python, given by `path`.

- path:

  Only for `backend = "path"`. Path to a Python executable, or to an
  environment directory (a virtualenv or conda prefix) containing one.
  When given a directory, greta looks for `bin/python` (Unix) or
  `Scripts/python.exe` (Windows) inside it. Pointing at an
  already-installed environment on disk never downloads anything, which
  makes it useful for offline or restricted-network setups.

- name:

  Only for `backend = "conda"`. Name of the conda environment to use.
  Defaults to `"greta-env-tf2"`, the environment created by
  [`install_greta_deps()`](https://greta-dev.github.io/greta/reference/install_greta_deps.md).

## Value

Invisibly, the stored preference (`NULL` for `greta_reset_python()`).

## Details

greta resolves which Python to use, in this order:

1.  The `RETICULATE_PYTHON` environment variable, if set (usually in
    `~/.Renviron`, your `.Rprofile`, or your shell environment). This
    always wins: it takes precedence over any stored preference.

2.  Your stored preference, set with `greta_set_python()`.

3.  An auto-detected `"greta-env-tf2"` conda environment (created by
    [`install_greta_deps()`](https://greta-dev.github.io/greta/reference/install_greta_deps.md)) -
    kept so setups from older greta versions keep working after
    upgrading.

4.  Otherwise, the managed (uv) environment (the default as of greta
    0.6.0): reticulate installs a compatible Python, TensorFlow, and
    TensorFlow Probability automatically on first use. No setup is
    needed - this happens "automagically".

For the managed (uv) environment, greta automatically enables uv's
offline mode once the environment is installed, so it no longer reaches
out to PyPI. Set `UV_OFFLINE=0` yourself to force online resolution (for
example, to refresh the environment), or `UV_OFFLINE=1` to force offline
mode - greta never overrides a value you have already set.

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

## See also

[`greta_set_deps()`](https://greta-dev.github.io/greta/reference/greta_set_deps.md),
[`greta_sitrep()`](https://greta-dev.github.io/greta/reference/greta_sitrep.md),
[`install_greta_deps()`](https://greta-dev.github.io/greta/reference/install_greta_deps.md),
[`greta_remove()`](https://greta-dev.github.io/greta/reference/greta_remove.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# use the managed (uv) environment (the default)
greta_set_python()

# use the conda environment from install_greta_deps()
greta_set_python("conda")

# use a differently-named conda environment
greta_set_python("conda", name = "my-tf-env")

# use a specific Python binary, or an environment directory
greta_set_python("path", path = "/path/to/python")
greta_set_python("path", path = "/opt/python-envs/greta")

# clear the stored choice and return to automatic resolution
greta_reset_python()
} # }
```
