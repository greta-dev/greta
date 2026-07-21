# Install Python dependencies for greta

This is a helper function to install specified versions of Python
dependencies needed for greta. By default, greta version \>= 0.6.0 now
uses reticulate's managed (uv) Python environment to automatically
identify dependencies. You can change over to this new approach with
[`greta_set_python()`](https://greta-dev.github.io/greta/dev/reference/greta_set_python.md),
which is now what we recommend. This has changed from where we would
previously use `install_greta_deps()`.

## Usage

``` r
install_greta_deps(
  deps = greta_deps_spec(),
  timeout = 5,
  restart = c("ask", "force", "no"),
  ...
)

reinstall_greta_deps(
  deps = greta_deps_spec(),
  timeout = 5,
  restart = c("ask", "force", "no"),
  ask = interactive()
)
```

## Arguments

- deps:

  object created with
  [`greta_deps_spec()`](https://greta-dev.github.io/greta/dev/reference/greta_deps_spec.md)
  where you specify python, TensorFlow (TF), and TensorFlow Probability
  (TFP) versions. By default these are TF 2.15.1, TFP 0.23.0, and Python
  3.11.
  [`greta_deps_spec()`](https://greta-dev.github.io/greta/dev/reference/greta_deps_spec.md)
  checks that the TF version is one greta supports; compatible TFP and
  Python versions are resolved at install time. See
  ?[`greta_deps_spec()`](https://greta-dev.github.io/greta/dev/reference/greta_deps_spec.md)
  for more information, and the data object `greta_deps_tf_tfp` for
  known-good combinations. If you have stored a preference with
  [`greta_set_deps()`](https://greta-dev.github.io/greta/dev/reference/greta_set_deps.md),
  it is used when `deps` is not supplied.

- timeout:

  maximum time in minutes until the installation for each installation
  component times out and exits. Default is 5 minutes per installation
  component.

- restart:

  character. Restart R after installation? Default is "ask". Other
  options are, "force", and "no". Using "force" will will force a
  restart after installation. Using "no" will not restart. Note that
  this only restarts R during interactive sessions, and only in RStudio.

- ...:

  Optional arguments, reserved for future expansion.

- ask:

  Logical; for `reinstall_greta_deps()`, whether to ask for confirmation
  before removing the existing greta conda environment. Defaults to
  [`interactive()`](https://rdrr.io/r/base/interactive.html).

## Value

Invisibly returns `NULL`; called for its side effect of installing
greta's Python dependencies into a conda environment.

## Details

This function, `install_greta_deps()`, is an alternative installation
workflow. The default versions of the python modules are: TensorFlow
2.15.1, TensorFlow Probability 0.23.0, and Python 3.11. These Python
modules will be installed into a conda environment named
"greta-env-tf2".

It can be useful to identify installation notes, warnings, or errors
that arise during install. You can do this by accessing the logfile with
[`open_greta_install_log()`](https://greta-dev.github.io/greta/dev/reference/open_greta_install_log.md),
which opens your logfile in your default web browser. The logfile of the
installation process is written to a user directory, by default to
`tools::R_user_dir("greta")`, and is named:
"greta-installation-logfile.html".

You can set the logfile location with
[`greta_set_install_logfile()`](https://greta-dev.github.io/greta/dev/reference/greta_set_install_logfile.md).
E.g., `greta_set_install_logfile('path/to/logfile.html')`. You can also
specify this with an environment variable, `GRETA_INSTALLATION_LOG`,
e.g., `Sys.setenv('GRETA_INSTALLATION_LOG'='path/to/logfile.html')`.

By default, if using RStudio, it will ask you if you want to restart the
R session. If the session is not interactive, or is not in RStudio, it
will not restart. You can also override this with `restart = TRUE`.

## Note

This will automatically install Miniconda (a minimal version of the
Anaconda scientific software management system), create a 'conda'
environment for greta named 'greta-env-tf2' with required python and
python package versions, and forcibly switch over to using that conda
environment.

We now recommend using the new default method for installation, which
uses [uv](https://docs.astral.sh/uv/) (via the reticulate package) to
install TensorFlow and TensorFlow Probability on first use. To make
greta use the "greta-env-tf2" conda environment created here instead,
use `greta_set_python("conda")` (or set the `RETICULATE_PYTHON`
environment variable to its Python before loading greta). See the
"Installing Dependencies" vignette and
[`greta_set_python()`](https://greta-dev.github.io/greta/dev/reference/greta_set_python.md).

If you don't want to use conda or the "greta-env-tf2" conda environment,
you can install versions that you like, e.g., using
[`reticulate::py_install()`](https://rstudio.github.io/reticulate/reference/py_install.html).
If you want to see which versions of TF, TFP, and Python work with each
other (at least according to information from tensorflows website), see
the data `greta_deps_tf_tfp`, which is provided with greta. Managing
your own installation is not always straightforward, so proceed with
caution.

## Examples

``` r
if (FALSE) { # \dontrun{
install_greta_deps()
} # }
if (FALSE) { # \dontrun{
# to help troubleshoot your greta installation, this can help resolve some
# issues with installing greta dependencies
reinstall_greta_deps()
} # }
```
