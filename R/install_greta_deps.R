#' Install Python dependencies for greta
#'
#' This is a helper function to install specified versions of Python
#' dependencies needed for greta. By default, greta version >= 0.6.0 now uses
#' reticulate's managed (uv) Python environment to automatically identify
#' dependencies. You can change over to this new approach with
#' [greta_set_python()], which is
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
#'   ?[greta_deps_spec()] for more information. If you have stored a
#'   preference with [greta_set_deps()], it is used when `deps` is not
#'   supplied.
#'
#' @param timeout maximum time in minutes until the installation for each
#'    installation component times out and exits. Default is 5 minutes per
#'    installation component.
#'
#' @param ask Logical; for [reinstall_greta_deps()], whether to ask for
#'   confirmation before removing the existing greta conda environment.
#'   Defaults to `interactive()`.
#'
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
#'  `greta_set_python("conda")` (or set the `RETICULATE_PYTHON` environment
#'  variable to its Python before loading greta). See the "Installing
#'  Dependencies" vignette and [greta_set_python()].
#'
#'  If you don't want to use conda or the "greta-env-tf2" conda environment, you
#'  can install versions that you like, e.g., using [reticulate::py_install()].
#'  greta installs and runs against a range of TensorFlow versions weekly, on
#'  Linux, macOS and Windows. Each run lists the combinations it tried and the
#'  versions each resolved to; open the most recent one from
#'  <https://github.com/greta-dev/greta/actions/workflows/install-check.yaml>.
#'  Managing your own installation is not always straightforward, so proceed
#'  with caution.
#'
#'
#' @name install_greta_deps
#'
#' @return Invisibly returns `NULL`; called for its side effect of
#'   installing greta's Python dependencies into a conda environment.
#' @export
#'
#' @examples
#' \dontrun{
#' install_greta_deps()
#' }
#' @importFrom reticulate py_available
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
  if (missing(deps)) {
    stored_deps <- get_greta_stored_deps()
    if (!is.null(stored_deps)) {
      cli::cli_inform(c(
        "i" = "Using dependency versions stored with {.fun greta_set_deps}."
      ))
      deps <- stored_deps
    }
  }
  check_greta_deps_spec(deps)

  restart <- rlang::arg_match(
    arg = restart,
    values = c("ask", "force", "no")
  )

  cli::cli_inform(c(
    "i" = "Most users do not need {.fun install_greta_deps}: greta installs \\
          TensorFlow and TensorFlow Probability automatically (via {.pkg uv}) \\
          on first use.",
    "i" = "Use {.fun install_greta_deps} to install a conda environment \\
          (e.g. offline, or to pin versions), then select it with \\
          {.code greta_set_python(\"conda\")}.",
    "i" = "See the installation vignette: {.vignette greta::installation}."
  ))

  # set warning message length, restoring the previous value on exit
  old_options <- options(warning.length = 2000)
  on.exit(options(old_options), add = TRUE)

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
        {.code greta_set_python(\"conda\")}."
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
  tf = "2.21.0",
  tfp = "0.25.0",
  python = "3.12",
  # The floors are what the weekly install check actually passes on, not the
  # oldest version that ever worked. TensorFlow 2.16 and 2.17 install but
  # cannot load tensorflow_probability, because greta pins TFP to the 0.25
  # series whatever the TensorFlow version; so the floor is a consequence of
  # that pinning, and may drop again if greta stops pinning TFP (#638).
  tf_min = "2.18.0",
  tfp_min = "0.25.0",
  python_min = "3.9",
  # python_min and python_range disagree on purpose, because they answer
  # different questions. python_min is the oldest Python greta_sitrep() will
  # accept in an environment that already exists: someone running Python 3.9
  # with TensorFlow 2.15 has a working setup, and warning them about it would be
  # wrong. python_range is what greta asks uv to install now, so it has to be a
  # Python the *pinned* TensorFlow publishes wheels for -- TensorFlow dropped
  # cp39 at 2.21, and leaving the floor at 3.9 let uv resolve a Python that no
  # TensorFlow would install against, which broke installation on Windows.
  python_range = ">=3.10,<=3.12"
)

#' Specify python dependencies for greta
#'
#' A helper function for specifying versions of Tensorflow (TF), Tensorflow
#' Probability (TFP), and Python. Defaulting to `r greta_deps_default$tf`,
#' `r greta_deps_default$tfp`, and `r greta_deps_default$python`, respectively.
#'
#' @section What you can actually choose:
#'
#' **TensorFlow, between `r greta_deps_default$tf_min` and
#' `r greta_deps_default$tf`.** Versions outside that range are rejected, and
#' the range is narrower than it looks for two reasons that compound:
#'
#' - greta's optimisers use the Keras 3 API, which ships with TensorFlow 2.16.
#'   Below that, `adam()` and the rest cannot be constructed at all.
#' - TensorFlow Probability has not had a release since 0.25.0 in November
#'   2024, and 0.25.0 is tested against TensorFlow 2.18. It installs against
#'   2.16 and 2.17 -- its metadata asks only for `tensorflow>=2.16` -- and then
#'   fails to import.
#'
#' **Python.** Passed through to the resolver rather than checked by greta.
#'
#' **TensorFlow Probability, in principle.** In practice there is one usable
#' version: 0.25.0 is the newest release and the only one that pairs with the
#' supported TensorFlow range. Older TFP releases pair with TensorFlow versions
#' greta no longer supports (0.24.0 with 2.16, 0.23.0 with 2.15). So changing
#' `tfp_version` is unlikely to give you a working environment, and greta does
#' not check it -- an incompatible choice surfaces as a resolver or import
#' error at install time.
#'
#' greta installs and runs against this range weekly, on Linux, macOS and
#' Windows. Each run publishes a table of every combination tried, what each
#' resolved to, and which did not work; open the most recent from
#' <https://github.com/greta-dev/greta/actions/workflows/install-check.yaml>.
#'
#' Calling `greta_deps_spec()` with no arguments returns greta's current
#' default (recommended) versions, and is the supported way to query them -
#' for example `greta_deps_spec()$tf_version` for the default TensorFlow
#' version.
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
#' # every combination below is one the weekly install check passes on
#' greta_deps_spec(tf_version = "2.18.0")
#' greta_deps_spec(tf_version = "2.19.0")
#' greta_deps_spec(tf_version = "2.20.0")
#' greta_deps_spec(
#'   tf_version = "2.19.0",
#'   tfp_version = "0.25.0",
#'   python_version = "3.12"
#' )
#' # these fail: greta supports TensorFlow 2.18.0 up to the version it pins
#' \dontrun{
#' greta_deps_spec(tf_version = "2.17.0")
#' greta_deps_spec(tf_version = "2.99.0")
#' }
greta_deps_spec <- function(
  tf_version = "2.21.0",
  tfp_version = "0.25.0",
  python_version = "3.12"
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
# defaults are enforced by the consistency test in test_greta_deps_spec.R.
#
# TensorFlow Probability needs tf-keras, which it imports unconditionally. Its
# `tf` extra declares that (`tf-keras>=2.16`), so asking for the extra lets the
# resolver pick a version rather than greta guessing one -- greta previously
# pinned tf-keras to TensorFlow's minor series, which has no release for TF 2.17
# or 2.18 and so could not be installed at all. greta does not set
# TF_USE_LEGACY_KERAS, so `tf$keras` remains Keras 3.

greta_py_require_args <- function(
  tf_version = greta_deps_default$tf,
  tfp_version = greta_deps_default$tfp,
  python_version = NULL
) {
  tf_minor <- sub("\\.[^.]*$", "", tf_version)
  tfp_minor <- sub("\\.[^.]*$", "", tfp_version)
  list(
    packages = c(
      paste0("tensorflow==", tf_minor, ".*"),
      paste0("tensorflow_probability[tf]==", tfp_minor, ".*")
    ),
    python_version = python_version %||% greta_deps_default$python_range
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
#' @return Invisibly returns `x`; called for its side effect of printing
#'   `x` as a data frame.
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

# greta supports TensorFlow between greta_deps_default$tf_min and
# greta_deps_default$tf, and the window is narrow because two constraints
# compound:
#
#   * greta's optimisers use the Keras 3 API (tf$keras$optimizers$*), which
#     ships with TensorFlow 2.16. Below that they cannot be constructed.
#   * TensorFlow Probability has had no release since 0.25.0 (November 2024),
#     and 0.25.0 is tested against TensorFlow 2.18.
#
# So the usable band is TF 2.18 upwards, with the one TFP release that pairs
# with it. This is worth stating rather than implying: greta's version
# arguments look like a free choice, and for TFP they are not.
#
# Rejecting a too-old version here rather than letting it install matters, and
# TFP's own metadata is why. Its `tf` extra asks only for `tensorflow>=2.16`,
# with no upper bound, so a resolver will happily put TFP 0.25 next to
# TensorFlow 2.16 -- it installs, downloads 43 packages, and only then fails to
# import. That also rules out the obvious fix of leaving TFP unpinned: the
# metadata carries too little information to resolve on.

check_greta_tf_supported <- function(deps, call = rlang::caller_env()) {
  greta_tf_version_max <- greta_deps_default$tf
  greta_tf_version_min <- greta_deps_default$tf_min
  install_check <- paste0(
    "https://github.com/greta-dev/greta/actions/workflows/install-check.yaml"
  )

  too_old <- numeric_version(deps$tf_version) <
    numeric_version(greta_tf_version_min)
  if (too_old) {
    cli::cli_abort(
      message = c(
        "{.pkg greta} supports TensorFlow {.val {greta_tf_version_min}} to \\
        {.val {greta_tf_version_max}}, with TensorFlow Probability \\
        {.val {greta_deps_default$tfp}}.",
        "x" = "The provided version was {.val {deps$tf_version}}, which is \\
        too old.",
        "i" = "{.pkg greta}'s optimisers use the Keras 3 API, which needs \\
        TensorFlow 2.16 or later.",
        "i" = "TensorFlow Probability has had no release since \\
        {.val {greta_deps_default$tfp}} (November 2024), and that release is \\
        tested against TensorFlow {.val {greta_tf_version_min}}. Older \\
        TensorFlow installs, then fails to import it.",
        "i" = "Every combination tried, and what each resolved to: \\
        {.url {install_check}}"
      ),
      call = call
    )
  }

  too_new <- numeric_version(deps$tf_version) >
    numeric_version(greta_tf_version_max)
  if (too_new) {
    gh_issue <- "https://github.com/greta-dev/greta/issues/675"
    cli::cli_abort(
      message = c(
        "{.pkg greta} supports TensorFlow {.val {greta_tf_version_min}} to \\
        {.val {greta_tf_version_max}}, with TensorFlow Probability \\
        {.val {greta_deps_default$tfp}}.",
        "x" = "The provided version was {.val {deps$tf_version}}, which is \\
        newer than {.pkg greta} has been tested against; see \\
        {.url {gh_issue}}.",
        "i" = "Every combination tried, and what each resolved to: \\
        {.url {install_check}}"
      ),
      call = call
    )
  }
}
