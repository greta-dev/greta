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

get_greta_python_backend <- function() {
  backend_file <- greta_python_backend_file()
  if (!file.exists(backend_file)) {
    return(NULL)
  }
  value <- tryCatch(
    trimws(readLines(backend_file, n = 1, warn = FALSE)),
    error = function(e) NULL
  )
  value_exists <- !is.null(value) && nzchar(value)
  if (!value_exists) {
    return(NULL)
  }
  value
}

set_greta_python_backend <- function(value) {
  config_dir <- tools::R_user_dir("greta", "config")
  if (!dir.exists(config_dir)) {
    dir.create(config_dir, recursive = TRUE)
  }
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

# --- cheap detection of an existing greta conda env ---------------------------

# Returns the path to the Python in the greta conda env if it exists, else NULL.
# Deliberately uses file.exists() on the expected path rather than
# reticulate::conda_list() so that nothing is shelled out at load time.
detect_greta_conda_python <- function(name = "greta-env-tf2") {
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
      # pins derived from greta's canonical default versions (see
      # greta_py_require_args()) so they cannot drift from greta_deps_spec()
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
  report_python_backend(pending_python_plan())
}

# --- user-facing helpers ------------------------------------------------------

#' Choose the Python environment greta uses
#'
#' @description
#' greta runs on Python (via TensorFlow and TensorFlow Probability). By default
#' it uses [`uv`](https://docs.astral.sh/uv/) (via the reticulate R package) to
#' install a compatible Python, TensorFlow, and TensorFlow Probability
#' automatically on first use. These helper functions let you persistently switch greta to a different
#' Python environment - for example a conda environment created by
#' [install_greta_deps()], or your own Python. [greta_reset_python()] clears the
#' stored choice, returning to greta's automatic resolution.
#'
#' Each helper reports what greta will resolve to the next time it loads. The
#' choice is stored under [tools::R_user_dir()] and applied the next time greta
#' is loaded, so you will need to **restart R** for it to take effect.
#'
#' @param name Name of the conda environment to use. Defaults to
#'   `"greta-env-tf2"`, the environment created by [install_greta_deps()].
#' @param path Path to a Python executable.
#'
#' @return Invisibly, the stored preference (`NULL` for `greta_reset_python()`).
#'
#' @details
#' These set a greta preference; setting the `RETICULATE_PYTHON` environment
#' variable directly (e.g. in your `.Rprofile`) takes precedence over the stored
#' preference.
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
  cli::cli_inform(c(
    "v" = "greta will use the {.pkg uv}-installed Python environment."
  ))
  report_pending_python_backend()
  invisible("managed")
}

#' @rdname greta_set_python
#' @export
greta_set_python_conda_env <- function(name = "greta-env-tf2") {
  python <- reticulate::conda_python(name)
  # tag the preference as conda so the resolver/report label it as a conda
  # environment rather than a generic user-specified Python
  set_greta_python_backend(paste0("conda:", python))
  cli::cli_inform(c(
    "v" = "greta will use the conda environment {.val {name}}."
  ))
  report_pending_python_backend()
  invisible(python)
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
  cli::cli_inform(c(
    "v" = "greta will use Python at {.path {path}}."
  ))
  report_pending_python_backend()
  invisible(path)
}

#' @rdname greta_set_python
#' @export
greta_reset_python <- function() {
  clear_greta_python_backend()
  cli::cli_inform(c(
    "v" = "Cleared your stored Python choice; greta will resolve automatically."
  ))
  report_pending_python_backend()
  invisible(NULL)
}
