# Python backend resolution for greta.
#
# greta can use one of several Python "backends":
#   * "managed"  - reticulate's managed (uv) environment, which auto-installs
#                  a compatible Python + TensorFlow + TensorFlow Probability.
#   * a path     - a specific Python (e.g. a conda env or virtualenv) that the
#                  user wants greta to use.
#
# The backend is resolved cheaply at load time (no conda subprocess) by
# `greta_python_plan()`, and can be set persistently by `greta_set_python()`,
# which writes a small backend file under `R_user_dir("greta")`.

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
# via greta_set_python("conda")), or a plain path to a Python. Interpret any
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
#   2. a stored greta backend (set via greta_set_python())
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
      # pins derive from greta_deps_default via greta_py_require_args(),
      # unless the user stored a preference with greta_set_deps();
      # agreement with greta_deps_spec()'s defaults is test-enforced
      stored_deps <- get_greta_stored_deps()
      py_req <- if (is.null(stored_deps)) {
        greta_py_require_args()
      } else {
        greta_py_require_args(
          tf_version = stored_deps$tf_version,
          tfp_version = stored_deps$tfp_version,
          python_version = stored_deps$python_version
        )
      }
      reticulate::py_require(
        packages = py_req$packages,
        python_version = py_req$python_version
      )
      # the frozen-pins argument for offline mode (#814) only holds for the
      # default pins; user-chosen versions may need one online resolve
      if (is.null(stored_deps)) {
        maybe_enable_uv_offline()
      }
    },
    # "user" and "conda" both point at a specific Python
    Sys.setenv("RETICULATE_PYTHON" = plan$python)
  )
  invisible(plan)
}

# --- uv cache detection ---------------------------------------------------------
#
# reticulate only bootstraps its own uv (with the cache redirected to
# reticulate_cache_dir("uv", ...)) when no usable uv is already installed;
# with uv on the PATH (or at ~/.local/bin/uv), reticulate uses that system uv
# and the environment cache lives in uv's own cache directory instead
# (UV_CACHE_DIR, or the platform default reported by `uv cache dir`). Offline
# detection must look wherever reticulate's chosen uv actually caches, or the
# offline start (#814) never engages for system-uv users.

reticulate_uv_cache_dir <- function() {
  file.path(tools::R_user_dir("reticulate", "cache"), "uv")
}

# The uv binary reticulate would use when that is NOT reticulate's own
# bootstrapped uv; NULL means reticulate would use (or install) its managed
# uv, whose cache location greta then knows without asking. Mirrors the
# search order in reticulate:::uv_binary(): the RETICULATE_UV environment
# variable, the reticulate.uv_binary option (both may hold the "managed"
# sentinel), uv on the PATH, then ~/.local/bin/uv. Never runs uv, so a stale
# binary reticulate would reject is still reported; the empirical populated
# check below keeps that safe.
detect_system_uv <- function() {
  usable <- function(uv) {
    length(uv) == 1 && !is.na(uv) && nzchar(uv) && file.exists(uv)
  }
  env_uv <- Sys.getenv("RETICULATE_UV", unset = NA)
  if (!is.na(env_uv)) {
    if (identical(env_uv, "managed")) {
      return(NULL)
    }
    return(if (usable(env_uv)) env_uv else NULL)
  }
  opt_uv <- getOption("reticulate.uv_binary")
  if (!is.null(opt_uv)) {
    if (identical(opt_uv, "managed")) {
      return(NULL)
    }
    return(if (usable(opt_uv)) opt_uv else NULL)
  }
  path_uv <- unname(Sys.which("uv"))
  if (usable(path_uv)) {
    return(path_uv)
  }
  local_uv <- path.expand("~/.local/bin/uv")
  if (usable(local_uv)) {
    return(local_uv)
  }
  NULL
}

# Ask a system uv where its cache lives (`uv cache dir`), falling back to
# uv's platform-default locations when uv cannot be asked. NULL when the
# cache location cannot be determined.
system_uv_cache_dir <- function(uv) {
  out <- tryCatch(
    suppressWarnings(
      system2(uv, c("cache", "dir"), stdout = TRUE, stderr = FALSE, timeout = 2)
    ),
    error = function(e) NULL
  )
  ok <- !is.null(out) && is.null(attr(out, "status")) && length(out) >= 1
  if (ok) {
    # uv may colourise its output; strip any ANSI escapes
    cache_dir <- trimws(gsub("\u001b\\[[0-9;]*m", "", out[[1]]))
    if (nzchar(cache_dir)) {
      return(cache_dir)
    }
  }
  default_uv_cache_dir()
}

# uv's platform-default cache locations: $XDG_CACHE_HOME/uv, ~/.cache/uv (uv
# prefers an existing ~/.cache/uv even on macOS), ~/Library/Caches/uv on
# macOS, and %LOCALAPPDATA%/uv/cache on Windows. The first that exists, else
# NULL.
default_uv_cache_dir <- function() {
  xdg <- Sys.getenv("XDG_CACHE_HOME", unset = "")
  local_app_data <- Sys.getenv("LOCALAPPDATA", unset = "")
  candidates <- c(
    if (nzchar(xdg)) file.path(xdg, "uv"),
    path.expand("~/.cache/uv"),
    if (is_mac()) path.expand("~/Library/Caches/uv"),
    if (is_windows() && nzchar(local_app_data)) {
      file.path(local_app_data, "uv", "cache")
    }
  )
  for (candidate in candidates) {
    if (dir.exists(candidate)) {
      return(candidate)
    }
  }
  NULL
}

# Is the uv cache that reticulate's chosen uv would use already populated?
# Shared by maybe_enable_uv_offline() and report_offline_readiness().
# Conservative on both layouts: an undetermined or empty cache reports
# populated = FALSE (never falsely offline-ready).
#   * managed uv: reticulate pins UV_CACHE_DIR and UV_PYTHON_INSTALL_DIR
#     under reticulate_cache_dir("uv"), so both subdirectories must exist.
#   * system uv: UV_CACHE_DIR wins if set, else uv's own cache dir; populated
#     means it holds real content (archive/wheels/environments buckets - a
#     fresh cache dir may contain only CACHEDIR.TAG). The interpreter can
#     live inside the archive bucket, so no separate python check is needed.
greta_uv_cache_status <- function(
  system_uv = detect_system_uv(),
  reticulate_uv_cache = reticulate_uv_cache_dir(),
  uv_cache_dir_env = Sys.getenv("UV_CACHE_DIR", unset = "")
) {
  if (is.null(system_uv)) {
    populated <- dir.exists(file.path(reticulate_uv_cache, "python")) &&
      dir.exists(file.path(reticulate_uv_cache, "cache"))
    return(list(kind = "managed", populated = populated))
  }
  cache_dir <- if (nzchar(uv_cache_dir_env)) {
    uv_cache_dir_env
  } else {
    system_uv_cache_dir(system_uv)
  }
  if (is.null(cache_dir) || !dir.exists(cache_dir)) {
    return(list(kind = "system", populated = FALSE))
  }
  entries <- list.files(cache_dir)
  populated <- any(grepl("^(archive|wheels|environments)-", entries))
  list(kind = "system", populated = populated)
}

# For the managed backend, auto-enable uv's offline mode when the uv cache is
# already populated (#814). greta pins *frozen* ranges for the managed
# backend (TensorFlow 2.15.*, TensorFlow Probability 0.23.*); TF 2.16+ ships
# Keras 3, which greta does not support, so no newer match will ever appear.
# That makes a cache-only resolve safe: uv never needs to reach PyPI once the
# environment is installed, so greta can start on an offline / air-gapped
# machine.
maybe_enable_uv_offline <- function(cache_status = greta_uv_cache_status()) {
  # respect the user: if UV_OFFLINE is already set to anything, leave it alone
  existing <- Sys.getenv("UV_OFFLINE", unset = NA)
  if (!is.na(existing)) {
    return(invisible(FALSE))
  }
  if (isTRUE(cache_status$populated)) {
    Sys.setenv(UV_OFFLINE = "1")
    return(invisible(TRUE))
  }
  invisible(FALSE)
}

# --- session invalidation on removal -------------------------------------------
#
# greta_remove() can delete the active Python environment mid-session, while
# greta_stash$python_backend (frozen at load by .onLoad()) and
# RETICULATE_PYTHON still point at it. Without invalidating this session
# state, check_tf_version() and greta_sitrep() would keep confidently
# reporting an environment that no longer exists, until the user restarts R.

# record that a removal has invalidated the active session's Python setup, so
# check_tf_version() and .onAttach() can nudge the user to restart R
flag_greta_deps_removed <- function() {
  greta_stash$deps_removed_this_session <- TRUE
  invisible(TRUE)
}

# drop the frozen backend plan, and unset RETICULATE_PYTHON only when greta
# (not the user) owns it: a "" or "managed" value captured at load means
# greta set it itself, while any other value is a real user override that
# must be left untouched
invalidate_greta_python_session <- function() {
  greta_stash$python_backend <- NULL
  reticulate_python_at_load <- greta_stash$reticulate_python_at_load %||% ""
  greta_owns_reticulate_python <- reticulate_python_at_load %in%
    c("", "managed")
  if (greta_owns_reticulate_python) {
    Sys.unsetenv("RETICULATE_PYTHON")
  }
  invisible(NULL)
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
# auto-detected conda env to the managed environment? (#801) Also requires the
# conda env to still exist on disk, so greta never nudges about (or claims to
# use) an environment that greta_remove() has since deleted.
should_nudge_to_managed <- function(
  plan = greta_stash$python_backend,
  is_interactive = interactive()
) {
  !is.null(plan) &&
    identical(plan$source, "auto_detect") &&
    !is.null(plan$python) &&
    file.exists(plan$python) &&
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
      {.code greta_set_python()}."
    )
  }

  invisible(plan)
}

# Report whether greta can start without internet access, for greta_sitrep().
# Only the managed (uv) backend ever downloads anything: the user/conda/path
# backends point at an environment already on disk. For the managed backend,
# readiness depends on UV_OFFLINE and on whether the uv cache is populated
# (the same greta_uv_cache_status() check as maybe_enable_uv_offline()).
report_offline_readiness <- function(
  plan = greta_stash$python_backend %||% greta_python_plan(),
  cache_status = if (identical(plan$backend, "managed")) {
    greta_uv_cache_status()
  },
  uv_offline = Sys.getenv("UV_OFFLINE", unset = "")
) {
  if (!identical(plan$backend, "managed")) {
    python_on_disk <- !is.null(plan$python) && file.exists(plan$python)
    if (python_on_disk) {
      cli::cli_alert_success(
        "offline-ready: this environment is already on disk; greta never \\
        downloads into it",
        wrap = TRUE
      )
    } else {
      cli::cli_alert_danger(
        "the selected Python environment no longer exists on disk (was it \\
        removed?); restart R to re-resolve it",
        wrap = TRUE
      )
      cli::cli_inform(c(
        "i" = "See the installation vignette: {.vignette greta::installation}."
      ))
    }
    return(invisible(plan))
  }
  cache_populated <- isTRUE(cache_status$populated)
  if (identical(uv_offline, "1") && cache_populated) {
    cli::cli_alert_success(
      "offline-ready: {.envvar UV_OFFLINE}=1 is set and the uv cache is \\
      populated",
      wrap = TRUE
    )
  } else if (identical(uv_offline, "1")) {
    cli::cli_alert_danger(
      "{.envvar UV_OFFLINE}=1 is set but the uv cache is not yet populated, \\
      so the next start may fail to resolve dependencies",
      wrap = TRUE
    )
    cli::cli_inform(c(
      "i" = "See the installation vignette: {.vignette greta::installation}."
    ))
  } else if (identical(uv_offline, "0")) {
    cli::cli_alert_danger(
      "will need internet on next start: {.envvar UV_OFFLINE}=0 forces \\
      online resolution",
      wrap = TRUE
    )
    cli::cli_inform(c(
      "i" = "See the installation vignette: {.vignette greta::installation}."
    ))
  } else if (cache_populated) {
    cli::cli_alert_success(
      "offline-ready: uv cache present, offline mode will engage",
      wrap = TRUE
    )
  } else {
    cli::cli_alert_danger(
      "will need internet on next start: uv cache not yet populated",
      wrap = TRUE
    )
    cli::cli_inform(c(
      "i" = "See the installation vignette: {.vignette greta::installation}."
    ))
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
      " " = "2. Stored preference - set with {.fun greta_set_python}",
      " " = "3. Auto-detected {.val greta-env-tf2} conda environment - created
         by {.fun install_greta_deps}",
      " " = "4. The managed (uv) environment - the default, no setup needed",
      "i" = "To use your stored preference, remove {.envvar RETICULATE_PYTHON}
         from {.file ~/.Renviron} (or wherever it is set), then restart R.",
      "i" = "See the installation vignette: {.vignette greta::installation}."
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

# --- internal setter impls ----------------------------------------------------
#
# The setting logic lives in these non-exported helpers so that
# greta_set_python() can dispatch to them per backend without the validation
# logic and the persistence logic tangling. Each persists a value in the
# grammar interpreted by plan_from_value().

set_python_uv_impl <- function() {
  set_greta_python_backend("managed")
  finish_python_backend_change(
    stored_msg = "Stored preference: the managed (uv) Python environment.",
    value = "managed"
  )
}

set_python_conda_impl <- function(name = "greta-env-tf2") {
  python <- reticulate::conda_python(name)
  # tag the preference as conda so the resolver/report label it as a conda
  # environment rather than a generic user-specified Python
  set_greta_python_backend(paste0("conda:", python))
  finish_python_backend_change(
    stored_msg = "Stored preference: the conda environment {.val {name}}.",
    value = python
  )
}

set_python_path_impl <- function(path) {
  python <- resolve_python_path(path)
  set_greta_python_backend(python)
  finish_python_backend_change(
    stored_msg = "Stored preference: Python at {.path {python}}.",
    value = python
  )
}

# validate the backend/path/name combination for greta_set_python():
# `path` is required iff backend = "path"; `name` is allowed only for
# backend = "conda"
check_greta_set_python_args <- function(
  backend,
  path,
  name,
  call = rlang::caller_env()
) {
  if (!identical(backend, "path") && !is.null(path)) {
    cli::cli_abort(
      message = c(
        "{.arg path} can only be used with {.code backend = \"path\"}.",
        "i" = "Did you mean {.code greta_set_python(\"path\", path = ...)}?"
      ),
      call = call
    )
  }
  if (!identical(backend, "conda") && !is.null(name)) {
    cli::cli_abort(
      message = c(
        "{.arg name} can only be used with {.code backend = \"conda\"}.",
        "i" = "Did you mean {.code greta_set_python(\"conda\", name = ...)}?"
      ),
      call = call
    )
  }
  if (identical(backend, "path") && is.null(path)) {
    cli::cli_abort(
      message = c(
        "{.arg path} must be supplied when {.code backend = \"path\"}.",
        "i" = "Pass a Python binary or an environment directory, e.g. \\
        {.code greta_set_python(\"path\", path = \"/opt/envs/greta\")}.",
        "i" = "See the installation vignette: {.vignette greta::installation}."
      ),
      call = call
    )
  }
}

# --- user-facing helpers ------------------------------------------------------

#' Choose the Python environment greta uses
#'
#' @description
#' greta runs on Python (via TensorFlow and TensorFlow Probability). By default
#' it uses [`uv`](https://docs.astral.sh/uv/) (via the reticulate R package) to
#' install a compatible Python, TensorFlow, and TensorFlow Probability
#' automatically on first use. `greta_set_python()` persistently selects which
#' Python environment greta uses: the managed (uv) environment, a conda
#' environment (for example one created by [install_greta_deps()]), or your
#' own Python. [greta_reset_python()] clears the stored choice, returning to
#' greta's automatic resolution.
#'
#' To choose which *versions* of TensorFlow and TensorFlow Probability the
#' managed (uv) environment installs, see [greta_set_deps()] - dependency
#' versions are separate from the choice of Python environment.
#'
#' @param backend Which Python environment to use. One of:
#'   - `"uv"` (default): the managed (uv) environment. reticulate installs a
#'     compatible Python, TensorFlow, and TensorFlow Probability automatically
#'     on first use.
#'   - `"conda"`: a conda environment, named by `name`.
#'   - `"path"`: a specific Python, given by `path`.
#' @param path Only for `backend = "path"`. Path to a Python executable, or to
#'   an environment directory (a virtualenv or conda prefix) containing one.
#'   When given a directory, greta looks for `bin/python` (Unix) or
#'   `Scripts/python.exe` (Windows) inside it. Pointing at an
#'   already-installed environment on disk never downloads anything, which
#'   makes it useful for offline or restricted-network setups.
#' @param name Only for `backend = "conda"`. Name of the conda environment to
#'   use. Defaults to `"greta-env-tf2"`, the environment created by
#'   [install_greta_deps()].
#'
#' @return Invisibly, the stored preference (`NULL` for `greta_reset_python()`).
#'
#' @details
#' greta resolves which Python to use, in this order:
#'
#' 1. The `RETICULATE_PYTHON` environment variable, if set (usually in
#'    `~/.Renviron`, your `.Rprofile`, or your shell environment). This
#'    always wins: it takes precedence over any stored preference.
#' 2. Your stored preference, set with `greta_set_python()`.
#' 3. An auto-detected `"greta-env-tf2"` conda environment (created by
#'    [install_greta_deps()]) - kept so setups from older greta versions keep
#'    working after upgrading.
#' 4. Otherwise, the managed (uv) environment (the default as of greta 0.6.0):
#'    reticulate installs a compatible Python, TensorFlow, and TensorFlow
#'    Probability automatically on first use. No setup is needed - this happens
#'    "automagically".
#'
#' For the managed (uv) environment, greta automatically enables uv's offline
#' mode once the environment is installed, so it no longer reaches out to
#' PyPI. Set `UV_OFFLINE=0` yourself to force online resolution (for example,
#' to refresh the environment), or `UV_OFFLINE=1` to force offline mode -
#' greta never overrides a value you have already set.
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
#' @seealso [greta_set_deps()], [greta_sitrep()], [install_greta_deps()],
#'   [greta_remove()]
#' @rdname greta_set_python
#' @export
#' @examples
#' \dontrun{
#' # use the managed (uv) environment (the default)
#' greta_set_python()
#'
#' # use the conda environment from install_greta_deps()
#' greta_set_python("conda")
#'
#' # use a differently-named conda environment
#' greta_set_python("conda", name = "my-tf-env")
#'
#' # use a specific Python binary, or an environment directory
#' greta_set_python("path", path = "/path/to/python")
#' greta_set_python("path", path = "/opt/python-envs/greta")
#'
#' # clear the stored choice and return to automatic resolution
#' greta_reset_python()
#' }
greta_set_python <- function(
  backend = c("uv", "conda", "path"),
  path = NULL,
  name = NULL
) {
  backend_choices <- c("uv", "conda", "path")
  looks_like_path <- rlang::is_string(backend) &&
    !backend %in% backend_choices &&
    (grepl("[/\\\\]", backend) || file.exists(backend))
  if (looks_like_path) {
    cli::cli_abort(c(
      "{.arg backend} must be one of {.val uv}, {.val conda}, or {.val path}.",
      "i" = "To use the Python at {.path {backend}}, run \\
      {.code greta_set_python(\"path\", path = \"{backend}\")}."
    ))
  }
  backend <- rlang::arg_match(backend, values = backend_choices)
  check_greta_set_python_args(backend = backend, path = path, name = name)
  switch(
    backend,
    uv = set_python_uv_impl(),
    conda = set_python_conda_impl(name = name %||% "greta-env-tf2"),
    path = set_python_path_impl(path = path)
  )
}

# Resolve a user-supplied path to a Python executable. `path` may be the Python
# binary itself, or an environment directory (a virtualenv or conda prefix)
# containing one; in the latter case we look for the platform's usual location.
# This never runs Python or reaches the network, so it stays fully offline.
resolve_python_path <- function(path) {
  # an existing file (not a directory) is taken to be the Python binary itself
  if (file.exists(path) && !dir.exists(path)) {
    return(path)
  }
  candidate <- if (is_windows()) {
    file.path(path, "Scripts", "python.exe")
  } else {
    file.path(path, "bin", "python")
  }
  if (file.exists(candidate)) {
    return(candidate)
  }
  cli::cli_abort(c(
    "No Python executable found for {.path {path}}.",
    "i" = "Pass a path to a Python binary, or to an environment directory \\
      containing {.path bin/python} (or {.path Scripts/python.exe} on Windows).",
    "i" = "See the installation vignette: {.vignette greta::installation}."
  ))
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

# --- stored deps preference ---------------------------------------------------
#
# Sibling of greta_python_backend_file(): records the user's preferred
# TensorFlow / TensorFlow Probability / Python versions, as set by
# greta_set_deps(). Read (a) at load time by apply_greta_python_plan() to feed
# py_require() for the managed backend, and (b) by install_greta_deps() as its
# default deps. Validation is delegated to greta_deps_spec(), both when
# writing and when reading back, so a stored file that no longer passes
# greta's checks is treated as absent.

greta_deps_file <- function() {
  file.path(tools::R_user_dir("greta", "config"), "python-deps")
}

get_greta_stored_deps <- function() {
  path <- greta_deps_file()
  if (!file.exists(path)) {
    return(NULL)
  }
  lines <- tryCatch(
    readLines(path, warn = FALSE),
    error = function(e) NULL
  )
  fields <- c("tf_version", "tfp_version", "python_version")
  pattern <- paste0("^(", paste(fields, collapse = "|"), ")=")
  keyed <- grepl(pattern, lines %||% character())
  values <- sub(pattern, "", lines[keyed])
  names(values) <- sub("=.*$", "", lines[keyed])
  if (!all(fields %in% names(values))) {
    return(NULL)
  }
  # re-validate on read: a stale file (e.g. written before a support-ceiling
  # change) must not crash load; treat it as absent instead
  tryCatch(
    greta_deps_spec(
      tf_version = values[["tf_version"]],
      tfp_version = values[["tfp_version"]],
      python_version = values[["python_version"]]
    ),
    error = function(e) NULL
  )
}

clear_greta_stored_deps <- function() {
  deps_file <- greta_deps_file()
  if (file.exists(deps_file)) {
    unlink(deps_file)
  }
  invisible(NULL)
}

#' Choose the dependency versions greta installs
#'
#' @description
#' Persistently choose which versions of TensorFlow, TensorFlow Probability,
#' and Python greta uses, independently of *where* they are installed (see
#' [greta_set_python()] for that). The stored versions are used by:
#'
#' - the managed (uv) environment, which installs them automatically on first
#'   use after a restart, and
#' - [install_greta_deps()], as its default `deps` argument when building a
#'   conda environment.
#'
#' Most users never need this: greta's defaults (TensorFlow
#' `r greta_deps_default$tf`, TensorFlow Probability
#' `r greta_deps_default$tfp`, Python `r greta_deps_default$python`) are the
#' newest versions greta supports.
#'
#' To clear the stored versions and return to the defaults, use
#' [greta_remove()]`("deps")`.
#'
#' @param deps object created with [greta_deps_spec()], which checks that the
#'   TensorFlow version is one greta supports.
#'
#' @return Invisibly, the stored [greta_deps_spec()].
#'
#' @details
#' Your choice is stored under `tools::R_user_dir("greta", "config")` and
#' applied the next time greta is loaded, so you will need to **restart R**
#' for it to take effect. Changing versions on the managed (uv) backend may
#' require internet access on the next load, to download the newly requested
#' versions.
#'
#' @seealso [greta_set_python()], [greta_deps_spec()], [install_greta_deps()]
#' @export
#' @examples
#' \dontrun{
#' # pin an older TensorFlow for the managed (uv) environment
#' greta_set_deps(greta_deps_spec(
#'   tf_version = "2.14.0",
#'   tfp_version = "0.22.1",
#'   python_version = "3.10"
#' ))
#'
#' # clear the stored versions and return to greta's defaults
#' greta_remove("deps")
#' }
greta_set_deps <- function(deps = greta_deps_spec()) {
  if (is.null(deps)) {
    cli::cli_abort(c(
      "{.arg deps} must be a {.fun greta_deps_spec} object.",
      "i" = "To clear stored dependency versions, use \\
      {.code greta_remove(\"deps\")}."
    ))
  }
  check_greta_deps_spec(deps)
  ensure_greta_config_dir()
  writeLines(
    c(
      paste0("tf_version=", deps$tf_version),
      paste0("tfp_version=", deps$tfp_version),
      paste0("python_version=", deps$python_version)
    ),
    greta_deps_file()
  )
  cli::cli_inform(c(
    "v" = "Stored dependency versions: TensorFlow {.val {deps$tf_version}}, \\
    TensorFlow Probability {.val {deps$tfp_version}}, Python \\
    {.val {deps$python_version}}.",
    "i" = "Restart R for this to take effect."
  ))
  invisible(deps)
}
