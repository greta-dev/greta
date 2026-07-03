# Python backend resolution for greta.
#
# greta can use one of several Python "backends":
#   * "managed"  - reticulate's managed (uv) environment, which auto-installs
#                  a compatible Python + TensorFlow + TensorFlow Probability.
#   * a path     - a specific Python (e.g. a conda env or virtualenv) that the
#                  user wants greta to use.
#
# The backend is resolved cheaply at load time (no conda subprocess) by
# `greta_python_plan()`, and can be set persistently by the `greta_set_python_*()`
# helpers, which write a small backend file under `R_user_dir("greta")`.

# Location of python backends

greta_python_backend_file <- function() {
  file.path(tools::R_user_dir("greta", "config"), "python-backend")
}

# sibling of greta_python_backend_file(); records the conda env Python found
# at install time, so detect_greta_conda_python() works for any conda root
greta_conda_record_file <- function() {
  file.path(tools::R_user_dir("greta", "config"), "conda-env-record")
}

get_greta_python_backend <- function() {
  read_config_line(greta_python_backend_file())
}

# create the greta config dir if it doesn't already exist
ensure_greta_config_dir <- function() {
  config_dir <- tools::R_user_dir("greta", "config")
  if (!dir.exists(config_dir)) {
    dir.create(config_dir, recursive = TRUE)
  }
  invisible(config_dir)
}

set_greta_python_backend <- function(value) {
  ensure_greta_config_dir()
  writeLines(value, greta_python_backend_file())
  invisible(value)
}

clear_greta_python_backend <- function() {
  backend_file <- greta_python_backend_file()
  if (file.exists(backend_file)) {
    unlink(backend_file)
  }
  invisible(NULL)
}

# read one line from a config file; NULL on missing/empty/unreadable. Shared
# by get_greta_python_backend() and detect_greta_conda_python() so neither has
# to handle a 0-byte or unreadable file itself.
read_config_line <- function(path) {
  if (!file.exists(path)) {
    return(NULL)
  }
  value <- tryCatch(
    readLines(path, n = 1, warn = FALSE),
    error = function(e) NULL
  )
  # length() == 0 covers BOTH NULL (read failed) and character(0) (empty file)
  if (length(value) == 0) {
    return(NULL)
  }
  value <- trimws(value)
  if (!nzchar(value)) {
    return(NULL)
  }
  value
}

# --- cheap detection of an existing greta conda env ---------------------------

# Returns the path to the Python in the greta conda env if it exists, else
# NULL. Checks the install-time record first (exact, works for any conda
# root), then falls back to file.exists() on the default miniconda path.
# Deliberately avoids reticulate::conda_list() so that nothing is shelled out
# at load time.
detect_greta_conda_python <- function(name = "greta-env-tf2") {
  # 1. install-time record: exact, works for any conda root
  recorded <- read_config_line(greta_conda_record_file())
  if (!is.null(recorded)) {
    if (file.exists(recorded)) {
      return(recorded)
    }
    # stale record (env deleted outside greta): drop it so we stop re-checking
    unlink(greta_conda_record_file())
  }
  # 2. fall back to the default miniconda path
  env_dir <- file.path(reticulate::miniconda_path(), "envs", name)
  python_path <- if (is_windows()) {
    file.path(env_dir, "python.exe")
  } else {
    file.path(env_dir, "bin", "python")
  }
  if (file.exists(python_path)) {
    return(python_path)
  } else {
    return(NULL)
  }
}

# called by install_greta_deps() after deps install succeeds
record_greta_conda_python <- function(name = "greta-env-tf2") {
  python <- reticulate::conda_python(name)
  ensure_greta_config_dir()
  writeLines(python, greta_conda_record_file())
  invisible(python)
}

# --- resolution ---------------------------------------------------------------

# A backend plan, as returned by greta_python_plan():
#   backend: "managed" | "user" | "conda"
#   source:  where the decision came from (for messaging / sitrep)
#   python:  the Python path (for "user"/"conda"; NULL for "managed")
new_python_plan <- function(backend, source, python = NULL) {
  list(backend = backend, source = source, python = python)
}

# RETICULATE_PYTHON and the stored preference share a grammar: the sentinel
# "managed", a "conda:<path>" tag (only ever written by the stored preference,
# via greta_set_python_conda_env()), or a plain path to a Python. Interpret any
# of these into a plan.
plan_from_value <- function(value, source) {
  if (identical(value, "managed")) {
    new_python_plan("managed", source)
  } else if (startsWith(value, "conda:")) {
    new_python_plan("conda", source, python = sub("^conda:", "", value))
  } else {
    new_python_plan("user", source, python = value)
  }
}

# Decide which Python backend greta should use, in priority order:
#   1. an explicit RETICULATE_PYTHON (a path, or the sentinel "managed")
#   2. a stored greta backend (set via the greta_set_python_*() helpers)
#   3. an auto-detected greta-env-tf2 conda env (keeps upgraders working)
#   4. the managed (uv) environment, as the default
# Pure given its arguments, so it can be tested without side effects.
greta_python_plan <- function(
  reticulate_python = Sys.getenv("RETICULATE_PYTHON"),
  stored_backend = get_greta_python_backend(),
  conda_python = detect_greta_conda_python()
) {
  if (nzchar(reticulate_python)) {
    return(plan_from_value(reticulate_python, source = "RETICULATE_PYTHON"))
  }
  if (!is.null(stored_backend)) {
    return(plan_from_value(stored_backend, source = "preference"))
  }
  if (!is.null(conda_python)) {
    return(new_python_plan("conda", "auto_detect", python = conda_python))
  }
  new_python_plan("managed", "default")
}

# Execute a backend plan: point RETICULATE_PYTHON at the chosen Python, and for
# the managed backend declare greta's requirements so uv can install them.
apply_greta_python_plan <- function(plan) {
  switch(
    plan$backend,
    managed = {
      Sys.setenv("RETICULATE_PYTHON" = "managed")
      # pins derive from greta_deps_default via greta_py_require_args();
      # agreement with greta_deps_spec()'s defaults is test-enforced
      py_req <- greta_py_require_args()
      reticulate::py_require(
        packages = py_req$packages,
        python_version = py_req$python_version
      )
    },
    # "user" and "conda" both point at a specific Python
    Sys.setenv("RETICULATE_PYTHON" = plan$python)
  )
  invisible(plan)
}

# --- one-time hints -----------------------------------------------------------

# A small registry of opt-in hints that have already been shown, so nudges
# (e.g. "you could switch to the managed environment") appear once rather than
# on every attach. Reusable for future opt-in suggestions.
greta_hints_file <- function() {
  file.path(tools::R_user_dir("greta", "config"), "shown-hints")
}

greta_hint_shown <- function(hint) {
  hints_file <- greta_hints_file()
  if (!file.exists(hints_file)) {
    return(FALSE)
  }
  hint %in% readLines(hints_file, warn = FALSE)
}

mark_greta_hint_shown <- function(hint) {
  config_dir <- tools::R_user_dir("greta", "config")
  if (!dir.exists(config_dir)) {
    dir.create(config_dir, recursive = TRUE)
  }
  hints_file <- greta_hints_file()
  if (file.exists(hints_file)) {
    shown <- readLines(hints_file, warn = FALSE)
  } else {
    shown <- character()
  }
  writeLines(unique(c(shown, hint)), hints_file)
  invisible(hint)
}

# Should we nudge the user (once, interactively) that they can switch from an
# auto-detected conda env to the managed environment? (#801)
should_nudge_to_managed <- function(
  plan = greta_stash$python_backend,
  is_interactive = interactive()
) {
  !is.null(plan) &&
    identical(plan$source, "auto_detect") &&
    is_interactive &&
    !greta_hint_shown("conda_to_managed")
}

# --- reporting ----------------------------------------------------------------

# Describe the resolved Python backend, for greta_sitrep().
report_python_backend <- function(
  plan = greta_stash$python_backend %||% greta_python_plan()
) {
  backend_desc <- switch(
    plan$backend,
    managed = "managed (uv) environment",
    conda = "conda environment",
    user = "user-specified Python"
  )
  source_desc <- switch(
    plan$source,
    RETICULATE_PYTHON = "RETICULATE_PYTHON environment variable",
    preference = "greta preference (see ?greta_set_python)",
    auto_detect = "auto-detected greta-env-tf2 conda environment",
    default = "default"
  )

  cli::cli_ul("backend: {.val {backend_desc}}")
  if (!is.null(plan$python)) {
    cli::cli_ul("python: {.path {plan$python}}")
  }
  cli::cli_ul("selected via: {source_desc}")
  # only nudge when greta auto-detected the conda env; if the user chose it
  # deliberately, don't second-guess them
  if (identical(plan$source, "auto_detect")) {
    cli::cli_ul(
      "To use the {.pkg uv} environment instead, run \\
      {.code greta_set_python_uv()}."
    )
  }

  invisible(plan)
}

# The plan greta would resolve on a fresh restart. Identical to a load-time
# resolution except it reads the just-written preference; it uses the
# RETICULATE_PYTHON captured at load, since apply_greta_python_plan() overwrote
# the live one this session.
pending_python_plan <- function() {
  greta_python_plan(
    reticulate_python = greta_stash$reticulate_python_at_load %||% ""
  )
}

report_pending_python_backend <- function() {
  cli::cli_inform(c("i" = "After you restart R, greta will use:"))
  report_python_backend(plan = pending_python_plan())
}

warn_if_reticulate_python_overrides <- function() {
  rp <- greta_stash$reticulate_python_at_load %||% ""
  # "managed" is greta's own sentinel, not a user override
  override_active <- nzchar(rp) && !identical(rp, "managed")
  if (override_active) {
    cli::cli_warn(c(
      "!" = "{.envvar RETICULATE_PYTHON} is set to {.path {rp}} and takes
         precedence over the stored preference.",
      "i" = "greta resolves Python in this order:",
      " " = "1. {.envvar RETICULATE_PYTHON} - usually set in {.file ~/.Renviron}
         or your shell environment",
      " " = "2. Stored preference - set with {.fun greta_set_python_uv},
         {.fun greta_set_python_conda_env}, or {.fun greta_set_python_path}",
      " " = "3. Auto-detected {.val greta-env-tf2} conda environment - created
         by {.fun install_greta_deps}",
      " " = "4. The managed uv environment - the default, no setup needed",
      "i" = "To use your stored preference, remove {.envvar RETICULATE_PYTHON}
         from {.file ~/.Renviron} (or wherever it is set), then restart R."
    ))
  }
}

# the shared tail replacing the four copy-pasted endings
finish_python_backend_change <- function(stored_msg, value) {
  cli::cli_inform(c("v" = stored_msg), .envir = parent.frame())
  warn_if_reticulate_python_overrides()
  report_pending_python_backend()
  invisible(value)
}

# --- user-facing helpers ------------------------------------------------------

#' Choose the Python environment greta uses
#'
#' @description
#' greta runs on Python (via TensorFlow and TensorFlow Probability). By default
#' it uses [`uv`](https://docs.astral.sh/uv/) (via the reticulate R package) to
#' install a compatible Python, TensorFlow, and TensorFlow Probability
#' automatically on first use. These helper functions let you persistently
#' switch greta to a different Python environment - for example a conda
#' environment created by [install_greta_deps()], or your own Python.
#' [greta_reset_python()] clears the stored choice, returning to greta's
#' automatic resolution.
#'
#' @param name Name of the conda environment to use. Defaults to
#'   `"greta-env-tf2"`, the environment created by [install_greta_deps()].
#' @param path Path to a Python executable.
#'
#' @return Invisibly, the stored preference (`NULL` for `greta_reset_python()`).
#'
#' @details
#' greta resolves which Python to use, in this order:
#'
#' 1. The `RETICULATE_PYTHON` environment variable, if set (usually in
#'    `~/.Renviron`, your `.Rprofile`, or your shell environment). This
#'    always wins: it takes precedence over any stored preference.
#' 2. Your stored preference, set with `greta_set_python_uv()`,
#'    `greta_set_python_conda_env()`, or `greta_set_python_path()`.
#' 3. An auto-detected `"greta-env-tf2"` conda environment (created by
#'    [install_greta_deps()]) - kept so setups from older greta versions keep
#'    working after upgrading.
#' 4. Otherwise, the uv-managed environment (the default as of greta 0.6.0):
#'    reticulate installs a compatible Python, TensorFlow, and TensorFlow
#'    Probability automatically on first use. No setup is needed - this happens
#'    "automagically".
#'
#' To check which Python greta is currently using, and which it will use
#' after a restart, call [greta_sitrep()].
#'
#' If a stored preference appears to be ignored, `RETICULATE_PYTHON` is
#' usually why: remove it from wherever it is set (for example
#' `~/.Renviron`), then restart R. Note that `Sys.unsetenv()` within a
#' session is not enough, as the choice is applied when greta loads.
#'
#' Your choice is stored under `tools::R_user_dir("greta", "config")` and
#' applied the next time greta is loaded, so you will need to **restart R**
#' for it to take effect.
#'
#' @rdname greta_set_python
#' @aliases greta_set_python
#' @export
#' @examples
#' \dontrun{
#' # use the uv-managed environment (the default)
#' greta_set_python_uv()
#'
#' # use the conda environment from install_greta_deps()
#' greta_set_python_conda_env()
#'
#' # use a specific Python
#' greta_set_python_path("/path/to/python")
#'
#' # clear the stored choice and return to automatic resolution
#' greta_reset_python()
#' }
greta_set_python_uv <- function() {
  set_greta_python_backend("managed")
  finish_python_backend_change(
    stored_msg = "Stored preference: the uv-managed Python environment.",
    value = "managed"
  )
}

#' @rdname greta_set_python
#' @export
greta_set_python_conda_env <- function(name = "greta-env-tf2") {
  python <- reticulate::conda_python(name)
  # tag the preference as conda so the resolver/report label it as a conda
  # environment rather than a generic user-specified Python
  set_greta_python_backend(paste0("conda:", python))
  finish_python_backend_change(
    stored_msg = "Stored preference: the conda environment {.val {name}}.",
    value = python
  )
}

#' @rdname greta_set_python
#' @export
greta_set_python_path <- function(path) {
  if (!file.exists(path)) {
    cli::cli_abort(
      "No Python executable found at {.path {path}}."
    )
  }
  set_greta_python_backend(path)
  finish_python_backend_change(
    stored_msg = "Stored preference: Python at {.path {path}}.",
    value = path
  )
}

#' @rdname greta_set_python
#' @export
greta_reset_python <- function() {
  clear_greta_python_backend()
  finish_python_backend_change(
    stored_msg = "Cleared stored preference; greta resolves automatically.",
    value = NULL
  )
}
