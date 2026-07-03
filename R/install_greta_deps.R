#' Install Python dependencies for greta
#'
#' This is a helper function to install specified versions of Python
#' dependencies needed for greta. By default, greta version >= 0.6.0 now uses
#' reticulate's uv-managed Python to automatically identify dependencies. You
#' can change over to this new approach with [greta_set_python_uv()], which is
#' now what we recommend. This has changed from where we would previously use
#' `install_greta_deps()`.
#'
#'   This function, `install_greta_deps()`, is an alternative installation
#'   workflow. The default versions of the python modules are: TensorFlow
#'   `r greta_deps_default$tf`, TensorFlow Probability
#'   `r greta_deps_default$tfp`, and Python `r greta_deps_default$python`.
#'   These Python modules will be installed into a conda environment named
#'   "greta-env-tf2".
#'
#'   It can be useful to identify installation notes, warnings, or errors that
#'   arise during install. You can do this by accessing the logfile with
#'   [open_greta_install_log()], which opens your logfile in your default web
#'   browser. The logfile of the installation process is written to a user
#'   directory, by default to `tools::R_user_dir("greta")`, and is named:
#'   "greta-installation-logfile.html".
#'
#'   You can set the logfile location with [greta_set_install_logfile()]. E.g.,
#'   `greta_set_install_logfile('path/to/logfile.html')`. You can also specify
#'   this with an environment variable, `GRETA_INSTALLATION_LOG`, e.g.,
#'   `Sys.setenv('GRETA_INSTALLATION_LOG'='path/to/logfile.html')`.
#'
#' @param deps object created with [greta_deps_spec()] where you
#'   specify python, TensorFlow (TF), and TensorFlow Probability (TFP) versions.
#'   By default these are TF `r greta_deps_default$tf`, TFP
#'   `r greta_deps_default$tfp`, and Python `r greta_deps_default$python`.
#'   [greta_deps_spec()] checks that the TF version is one greta supports;
#'   compatible TFP and Python versions are resolved at install time. See
#'   ?[greta_deps_spec()] for more information, and the data object
#'   `greta_deps_tf_tfp` for known-good combinations.
#'
#' @param timeout maximum time in minutes until the installation for each
#'    installation component times out and exits. Default is 5 minutes per
#'    installation component.
#' @param restart character. Restart R after installation? Default is "ask".
#'  Other options are, "force", and "no". Using "force" will will force a
#'  restart after installation. Using  "no" will not restart. Note that this
#'  only restarts R during interactive sessions, and only in RStudio.
#'
#' @param ... Optional arguments, reserved for future expansion.
#'
#' @details
#'  By default, if using RStudio, it will ask you if you want to restart the R
#'  session. If the session is not interactive, or is not in RStudio, it will
#'  not restart. You can also override this with `restart = TRUE`.
#'
#' @note This will automatically install Miniconda (a minimal version of the
#'  Anaconda scientific software management system), create a 'conda'
#'  environment for greta named 'greta-env-tf2' with required python and python
#'  package versions, and forcibly switch over to using that conda environment.
#'
#'  We now recommend using the new default method for installation, which uses
#'  [uv](https://docs.astral.sh/uv/) (via the reticulate package) to install
#'  TensorFlow and TensorFlow Probability on first use. To make greta use the
#'  "greta-env-tf2" conda environment created here instead, use
#'  [greta_set_python_conda_env()] (or set the `RETICULATE_PYTHON` environment
#'  variable to its Python before loading greta). See the "Installing
#'  Dependencies" vignette and [greta_set_python_path()].
#'
#'  If you don't want to use conda or the "greta-env-tf2" conda environment, you
#'  can install versions that you like, e.g., using [reticulate::py_install()].
#'  If you want to see which versions of TF, TFP, and Python work with each
#'  other (at least according to information from tensorflows website), see the
#'  data `greta_deps_tf_tfp`, which is provided with greta. Managing your own
#'  installation is not always straightforward, so proceed with caution.
#'
#'
#' @name install_greta_deps
#'
#' @export
#'
#' @examples
#' \dontrun{
#' install_greta_deps()
#' }
#' @importFrom reticulate py_available
#' @importFrom tensorflow install_tensorflow
#' @importFrom reticulate conda_create
#' @importFrom reticulate conda_install
#' @importFrom cli cli_alert_info
#' @importFrom cli cli_process_start
#' @importFrom cli cli_process_done
#' @importFrom cli cli_ul
#' @importFrom callr r_process_options
#' @importFrom callr r_process
#' @importFrom cli cli_alert_success
#' @importFrom cli cli_ul
install_greta_deps <- function(
  deps = greta_deps_spec(),
  timeout = 5,
  restart = c("ask", "force", "no"),
  ...
) {
  check_greta_deps_spec(deps)

  restart <- rlang::arg_match(
    arg = restart,
    values = c("ask", "force", "no")
  )

  cli::cli_inform(c(
    "i" = "Most users do not need {.fun install_greta_deps}: greta installs \\
          TensorFlow and TensorFlow Probability automatically (via {.pkg uv}) \\
          on first use.",
    "i" = "You can set this with {.fun greta_set_python_uv}",
    "i" = "Use {.fun install_greta_deps} to install a conda environment \\
          (e.g. offline, or to pin \\ versions), then select it with \\
          {.fun greta_set_python_conda_env}.",
    "i" = "See the installation vignette: {.vignette greta::installation}."
  ))

  # set warning message length
  options(warning.length = 2000)

  # install miniconda if needed
  if (!have_conda()) {
    greta_install_miniconda(timeout)
  }

  if (!have_greta_conda_env()) {
    greta_create_conda_env(
      timeout = timeout,
      deps = deps
    )
  }

  # TODO
  # Issue warning if you already have conda env +/ miniconda
  # suggest using `reinstall_greta_deps()`
  greta_install_python_deps(
    timeout = timeout,
    deps = deps
  )

  # record the conda env python so load-time detection finds it in any
  # conda root
  tryCatch(
    record_greta_conda_python(),
    error = function(e) {
      cli::cli_warn(
        "Could not record the conda environment location; greta may not \\
        auto-detect it. Select it explicitly with \\
        {.fun greta_set_python_conda_env}."
      )
    }
  )

  # TODO
  # Detect if you have tried to install greta multiple times in the same
  # session, and suggest that perhaps they want to use
  # `reinstall_greta_deps()`
  # perhaps even stopping the session with a "yesno"

  greta_logfile <- sys_get_env("GRETA_INSTALLATION_LOG")

  greta_logfile <- greta_logfile %||% greta_default_logfile()

  write_greta_install_log(path = greta_logfile)

  cli::cli_alert_success(
    "Installation of {.pkg greta} dependencies \\
                         is complete!",
    wrap = TRUE
  )

  restart_or_not(restart)
}

get_pkg_user_dir <- function() {
  pkg_user_dir <- tools::R_user_dir("greta")
  if (!dir.exists(pkg_user_dir)) {
    dir.create(pkg_user_dir, recursive = TRUE)
  }
  pkg_user_dir
}

greta_default_logfile <- function() {
  greta_user_dir <- get_pkg_user_dir()
  file.path(greta_user_dir, "greta-installation-logfile.html")
}


restart_or_not <- function(restart) {
  # Managing how to restart R
  # requires RStudio and also an interactive session
  has_rstudioapi_pkg <- requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::hasFun("restartSession")

  # Default (if using rstudio) - we ask the user if they want to restart?
  ask_restart <- interactive() && has_rstudioapi_pkg && (restart == "ask")

  # where the user has specified a restart
  user_force_restart <- (restart == "force") &&
    interactive() &&
    has_rstudioapi_pkg

  # Where there is no rstudio/not interactive, suggest restarting.
  suggest_restart <- (restart == "force" | restart == "no") &&
    (!interactive() | !has_rstudioapi_pkg)

  if (suggest_restart) {
    cli::cli_inform(
      "Restart R, then load {.pkg greta} with: {.code library(greta)}"
    )
    return(invisible())
  }

  if (ask_restart) {
    if (yesno::yesno("Restart R and load greta?")) {
      rstudioapi::restartSession(
        command = "library(greta)",
        clean = TRUE
      )
    }
  }

  if (user_force_restart) {
    cli::cli_inform("Restarting R, then loading {.pkg greta}")
    rstudioapi::restartSession(
      command = "library(greta)",
      clean = TRUE
    )
  }
}

# To make it easier to maintain the canonical Python deps that greta supports:
# this is the single source of truth for the uv py_require() pins
# (greta_py_require_args()), the TF support ceiling
# (check_greta_tf_supported()), and the roxygen for greta_deps_spec().
# The greta_deps_spec() formals repeat these literal version numbers, so users
# see actual values, not `greta_deps_default$tf` etc.
# the consistency test in test_greta_deps_spec.R keeps them in agreement.
# Pins (tf, tfp, python) are what greta installs and defaults to; floors
# (*_min) are the oldest versions greta_sitrep() accepts.
# In the future, if we update greta default versions, we can just do that in
# two places - here, and in `greta_deps_spec()`.
greta_deps_default <- list(
  tf = "2.15.1",
  tfp = "0.23.0",
  python = "3.11",
  tf_min = "2.15.0",
  tfp_min = "0.23.0",
  python_min = "3.9",
  python_range = ">=3.9,<=3.11"
)

#' Specify python dependencies for greta
#'
#' A helper function for specifying versions of Tensorflow (TF), Tensorflow
#' Probability (TFP), and Python. Defaulting to `r greta_deps_default$tf`,
#' `r greta_deps_default$tfp`, and `r greta_deps_default$python`, respectively.
#' greta checks it supports the TF version (greta does not support TF 2.16 or
#' later, which ship Keras 3); compatible TFP and Python versions are resolved
#' at install time by uv (or, for a conda environment, by conda/pip). The
#' `greta_deps_tf_tfp` dataset lists known-good combinations of TF, TFP, and
#' Python; inspect it with `View(greta_deps_tf_tfp)`.
#'
#' @param tf_version character. TensorFlow version, in the format
#'   major.minor.patch. Default is `r greta_deps_default$tf`.
#' @param tfp_version Character. Tensorflow probability (TFP) version
#'   major.minor.patch. Default is `r greta_deps_default$tfp`.
#' @param python_version Character. Python version in format major.minor.patch.
#'   Default is `r greta_deps_default$python`.
#'
#' @return data frame of valid dependencies
#' @export
#'
#' @examples
#' greta_deps_spec()
#' greta_deps_spec(tf_version = "2.15.1")
#' greta_deps_spec(tf_version = "2.15.0")
#' greta_deps_spec(tf_version = "2.15.1", tfp_version = "0.23.0")
#' greta_deps_spec(tf_version = "2.15.0", tfp_version = "0.23.0")
#' greta_deps_spec(tf_version = "2.15.1", python_version = "3.10")
#' greta_deps_spec(tf_version = "2.15.0", python_version = "3.10")
#' greta_deps_spec(
#'   tf_version = "2.14.0",
#'   tfp_version = "0.22.1",
#'   python_version = "3.10"
#'   )
#' # this will fail: greta does not support TF 2.16+ (Keras 3)
#' \dontrun{
#' greta_deps_spec(tf_version = "2.16.0")
#'   }
greta_deps_spec <- function(
  tf_version = "2.15.1",
  tfp_version = "0.23.0",
  python_version = "3.11"
) {
  deps_obj <- new_greta_deps_spec(
    tf_version = tf_version,
    tfp_version = tfp_version,
    python_version = python_version
  )

  # greta only constrains the TensorFlow version (see check_greta_tf_supported);
  # compatible TFP and Python versions are left to uv (or conda) to resolve
  check_greta_tf_supported(deps_obj)

  deps_obj
}

new_greta_deps_spec <- function(tf_version, tfp_version, python_version) {
  deps_list <- data.frame(
    tf_version = tf_version,
    tfp_version = tfp_version,
    python_version = python_version
  )

  structure(
    deps_list,
    class = c("greta_deps_spec", "data.frame")
  )
}

# Translate the canonical (or a requested) TF/TFP version into reticulate
# py_require() arguments for the uv environment (see apply_greta_python_plan()).
# Defaults derive from greta_deps_default; greta_deps_spec()'s matching literal
# defaults are enforced by the consistency test in test_greta_deps_spec.R. greta
# does not support TF 2.16+, which ships Keras 3 (#675).

greta_py_require_args <- function(
  tf_version = greta_deps_default$tf,
  tfp_version = greta_deps_default$tfp
) {
  tf_minor <- sub("\\.[^.]*$", "", tf_version)
  tfp_minor <- sub("\\.[^.]*$", "", tfp_version)
  list(
    packages = c(
      paste0("tensorflow==", tf_minor, ".*"),
      paste0("tensorflow_probability==", tfp_minor, ".*")
    ),
    python_version = greta_deps_default$python_range
  )
}

check_greta_deps_spec <- function(deps, call = rlang::caller_env()) {
  if (!inherits(deps, "greta_deps_spec")) {
    cli::cli_abort(
      message = "{.arg deps} must be created by {.fun greta_deps_spec}.",
      call = call
    )
  }
}

#' Print method for greta python deps
#'
#' @param x greta python deps
#' @param ... extra args, not used
#' @export
print.greta_deps_spec <- function(x, ...) {
  print.data.frame(x)
}

#' Capture greta python dependencies.
#'
#' To assist with capturing and sharing python dependencies, we provide a way
#'   to capture the dependencies currently used. Unlike [greta_deps_spec()],
#'   the receipt records the versions actually installed and is **not**
#'   validated against the versions greta supports - so it will faithfully
#'   report, for example, a TensorFlow version newer than greta's supported
#'   range.
#'
#' @return `greta_deps_spec()` object
#' @export
#'
#' @examples
#' \dontrun{
#' my_deps <- greta_deps_receipt()
#' }
greta_deps_receipt <- function() {
  tf_version <- version_tf()
  tfp_version <- version_tfp()

  if (is.null(tf_version) || is.null(tfp_version)) {
    cli::cli_abort(
      c(
        "Cannot capture a dependency receipt as TensorFlow and TensorFlow \\
        Probability are not both available.",
        "i" = "greta installs these automatically the first time it is used. \\
        Run {.run greta::greta_sitrep()} or fit a model to trigger setup, \\
        then try again.",
        "i" = "For help, including offline or conda installs, see the \\
        installation vignette ({.vignette greta::installation}) or \\
        {.fun install_greta_deps}."
      )
    )
  }

  new_greta_deps_spec(
    tf_version = tf_version,
    tfp_version = tfp_version,
    python_version = as.character(py_version())
  )
}

# greta supports TensorFlow only up to the version in greta_deps_default$tf (the
# newest version greta is known to work with). Later versions are not supported
# -- in particular TF 2.16 ships Keras 3, which breaks greta (#675). This is the
# only version constraint greta enforces itself; compatible TensorFlow
# Probability and Python versions are resolved by uv (or, for a conda
# environment, by conda/pip).

check_greta_tf_supported <- function(deps, call = rlang::caller_env()) {
  greta_tf_version_max <- greta_deps_default$tf
  too_new <- numeric_version(deps$tf_version) >
    numeric_version(greta_tf_version_max)
  if (too_new) {
    gh_issue <- "https://github.com/greta-dev/greta/issues/675"
    cli::cli_abort(
      message = c(
        "{.pkg greta} supports TensorFlow up to version \\
        {.val {greta_tf_version_max}}.",
        "x" = "The provided version was {.val {deps$tf_version}}.",
        "i" = "Later versions are not yet supported (TensorFlow 2.16 ships \\
        Keras 3, which breaks greta); see {.url {gh_issue}}.",
        "i" = "greta resolves compatible TensorFlow Probability and Python \\
        versions automatically."
      ),
      call = call
    )
  }
}
