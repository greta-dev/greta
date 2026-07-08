# --- internal removal helpers -------------------------------------------------
#
# The actual removal logic lives in these non-exported helpers so that
# greta_remove() and the deprecated shims (remove_greta_env() and friends) can
# share it without duplicating any behaviour.

# remove the 'greta-env-tf2' conda environment (and its stale install record)
remove_greta_env_impl <- function(ask = interactive()) {
  if (!have_greta_conda_env()) {
    cli::cli_alert_info(
      "No 'greta-env-tf2' conda environment found.",
      wrap = TRUE
    )
    return(invisible(FALSE))
  }
  confirmed <- user_agrees(
    ask = ask,
    question = paste0(
      "Are you sure you want to remove the 'greta-env-tf2' conda environment?"
    )
  )
  if (confirmed) {
    cli::cli_alert_info(
      "removing 'greta-env-tf2' conda environment",
      wrap = TRUE
    )
    reticulate::conda_remove(
      envname = "greta-env-tf2"
    )
    # the install-time location record is now stale; remove it too
    unlink(greta_conda_record_file())
    cli::cli_alert_success("greta-env-tf2 environment removed!", wrap = TRUE)
    return(invisible(TRUE))
  }
  return(invisible(FALSE))
}

# delete the miniconda installation
remove_miniconda_impl <- function(ask = interactive()) {
  path_to_miniconda <- reticulate::miniconda_path()
  if (!file.exists(path_to_miniconda)) {
    cli::cli_alert_info(
      "No miniconda files found at {path_to_miniconda}",
      wrap = TRUE
    )
    return(invisible(FALSE))
  }
  confirmed <- user_agrees(
    ask = ask,
    question = paste0(
      "Are you sure you want to delete miniconda from ",
      path_to_miniconda,
      "?"
    )
  )
  if (confirmed) {
    cli::cli_alert_info("removing 'miniconda' installation", wrap = TRUE)
    unlink(path_to_miniconda, recursive = TRUE)
    cli::cli_alert_success("'miniconda' successfully removed!", wrap = TRUE)
    return(invisible(TRUE))
  }
  return(invisible(FALSE))
}

# remove reticulate's uv cache (shared by all reticulate packages)
remove_reticulate_uv_cache_impl <- function(ask = interactive()) {
  # only ever touch reticulate's *managed* uv cache; a system-wide uv cache is
  # uv's own to manage (e.g. `uv cache clean`) and is shared beyond reticulate
  uv_cache <- file.path(tools::R_user_dir("reticulate", "cache"), "uv")
  if (!dir.exists(uv_cache)) {
    cli::cli_inform(c(
      "i" = "No reticulate-managed {.pkg uv} cache found at \\
      {.path {uv_cache}}.",
      "i" = "If reticulate is using a system {.pkg uv}, its cache is managed \\
      by {.pkg uv} itself (e.g. {.code uv cache clean}); greta does not remove \\
      it."
    ))
    return(invisible(FALSE))
  }

  confirmed <- user_agrees(
    ask = ask,
    question = paste0(
      "Remove reticulate's uv cache at ",
      uv_cache,
      "? This is shared by other R packages that use reticulate's uv."
    )
  )
  if (confirmed) {
    cli::cli_alert_info("removing reticulate's uv cache", wrap = TRUE)
    unlink(uv_cache, recursive = TRUE, force = TRUE)
    cli::cli_alert_success("reticulate uv cache removed!", wrap = TRUE)
    return(invisible(TRUE))
  }
  invisible(FALSE)
}

# clear greta's stored Python backend preference and report what happened
remove_greta_preference_impl <- function() {
  had_preference <- !is.null(get_greta_python_backend())
  clear_greta_python_backend()
  if (had_preference) {
    cli::cli_inform(c(
      "v" = "Cleared the stored greta Python preference."
    ))
  } else {
    cli::cli_inform(c(
      "i" = "No stored greta Python preference to clear."
    ))
  }
  invisible(had_preference)
}

# the "nuclear" reset: remove the conda env, miniconda, reticulate's uv cache,
# and the stored preferences (Python environment and dependency versions).
# Asks once up front, then removes without prompting.
remove_greta_all <- function(ask = interactive()) {
  if (
    !user_agrees(
      ask = ask,
      question = paste0(
        "Remove ALL of greta's Python dependencies (conda env, miniconda, ",
        "reticulate's uv cache, stored preferences)?"
      )
    )
  ) {
    return(invisible(FALSE))
  }
  env_removed <- remove_greta_env_impl(ask = FALSE)
  mc_removed <- remove_miniconda_impl(ask = FALSE)
  cache_removed <- remove_reticulate_uv_cache_impl(ask = FALSE)
  clear_greta_python_backend()
  clear_greta_stored_deps()
  removed <- c(
    if (env_removed) "the 'greta-env-tf2' conda environment",
    if (mc_removed) "miniconda",
    if (cache_removed) "reticulate's uv cache"
  )
  # report honestly: with ask = FALSE, FALSE means "nothing was there"
  anything_removed <- length(removed) > 0
  if (anything_removed) {
    cli::cli_inform(c(
      "v" = "Successfully removed {removed}.",
      "v" = "Cleared any stored greta preferences.",
      "i" = "Restart R; greta will reinstall what it needs on next use.",
      "i" = "See the installation vignette: {.vignette greta::installation}."
    ))
  } else {
    cli::cli_inform(c(
      "i" = "Nothing to remove.",
      "v" = "Cleared any stored greta preferences."
    ))
  }
  invisible(anything_removed)
}

# --- user-facing removal API --------------------------------------------------

#' Remove greta's Python dependencies
#'
#' A single entry point for removing greta's Python bits, which is useful when
#' debugging a greta installation and you want to get back to a "clean slate".
#' Use the `what` argument to choose how much to remove.
#'
#' @param what What to remove. One of:
#'   - `"all"` (default): the `"greta-env-tf2"` conda environment, miniconda,
#'     reticulate's uv cache, and greta's stored preferences (the Python
#'     backend set via [greta_set_python()], and the dependency versions set
#'     via [greta_set_deps()]). This is a "nuclear" reset that asks once, then
#'     removes everything it finds.
#'   - `"env"`: the `"greta-env-tf2"` conda environment.
#'   - `"miniconda"`: the miniconda installation.
#'   - `"uv_cache"`: reticulate's uv cache. Note this cache is shared by
#'     all R packages that use reticulate's uv (it is not greta-specific); a
#'     system-wide uv cache is left untouched.
#'   - `"preference"`: greta's stored Python backend preference (set via
#'     [greta_set_python()]).
#' @param ask Ask for confirmation? Default is `interactive()`.
#'
#' @return Invisibly, `TRUE` if anything was removed, otherwise `FALSE`.
#' @seealso [reinstall_greta_deps()], [greta_set_python()]
#' @export
#' @examples
#' \dontrun{
#' # remove everything (nuclear reset)
#' greta_remove()
#'
#' # remove only the conda environment
#' greta_remove("env")
#'
#' # clear the stored Python preference
#' greta_remove("preference")
#' }
greta_remove <- function(
  what = c("all", "env", "miniconda", "uv_cache", "preference"),
  ask = interactive()
) {
  what <- match.arg(what)
  switch(
    what,
    all = remove_greta_all(ask = ask),
    env = remove_greta_env_impl(ask = ask),
    miniconda = remove_miniconda_impl(ask = ask),
    uv_cache = remove_reticulate_uv_cache_impl(ask = ask),
    preference = remove_greta_preference_impl()
  )
}

#' @rdname install_greta_deps
#' @export
#' @examples
#' \dontrun{
#' # to help troubleshoot your greta installation, this can help resolve some
#' # issues with installing greta dependencies
#' reinstall_greta_deps()
#' }
reinstall_greta_deps <- function(
  deps = greta_deps_spec(),
  timeout = 5,
  restart = c("ask", "force", "no"),
  ask = interactive()
) {
  if (
    !user_agrees(
      ask = ask,
      question = "Remove 'greta-env-tf2' and miniconda, then reinstall?"
    )
  ) {
    return(invisible(FALSE))
  }
  remove_greta_env_impl(ask = FALSE)
  remove_miniconda_impl(ask = FALSE)
  install_greta_deps(
    deps = deps,
    timeout = timeout,
    restart = restart
  )
}

# --- deprecated shims ---------------------------------------------------------
#
# These remain exported and fully functional, delegating to the internal
# helpers so their behaviour is unchanged. They simply warn and point at the
# consolidated greta_remove() / reinstall_greta_deps() API.

#' Deprecated removal and reinstallation helpers
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' These functions have been consolidated into [greta_remove()] (for removal)
#' and [reinstall_greta_deps()] (for reinstallation). They remain exported and
#' functional but will be removed in a future release.
#'
#' @param ask Ask for confirmation? Default is `interactive()`.
#' @param timeout time in minutes to wait until timeout (default is 5 minutes).
#'
#' @return Invisibly, `TRUE` if anything was removed, otherwise `FALSE`.
#' @seealso [greta_remove()], [reinstall_greta_deps()]
#' @keywords internal
#' @name deprecated-installers
NULL

#' @rdname deprecated-installers
#' @export
remove_greta_env <- function(ask = interactive()) {
  lifecycle::deprecate_warn(
    when = "0.6.0",
    what = "remove_greta_env()",
    with = "greta_remove()",
    details = "Use `greta_remove(\"env\")` instead."
  )
  remove_greta_env_impl(ask = ask)
}

#' @rdname deprecated-installers
#' @export
remove_miniconda <- function(ask = interactive()) {
  lifecycle::deprecate_warn(
    when = "0.6.0",
    what = "remove_miniconda()",
    with = "greta_remove()",
    details = "Use `greta_remove(\"miniconda\")` instead."
  )
  remove_miniconda_impl(ask = ask)
}

#' @rdname deprecated-installers
#' @export
remove_reticulate_uv_cache <- function(ask = interactive()) {
  lifecycle::deprecate_warn(
    when = "0.6.0",
    what = "remove_reticulate_uv_cache()",
    with = "greta_remove()",
    details = "Use `greta_remove(\"uv_cache\")` instead."
  )
  remove_reticulate_uv_cache_impl(ask = ask)
}

#' @rdname deprecated-installers
#' @export
greta_remove_all_deps <- function(ask = interactive()) {
  lifecycle::deprecate_warn(
    when = "0.6.0",
    what = "greta_remove_all_deps()",
    with = "greta_remove()",
    details = "Use `greta_remove()` (equivalently `greta_remove(\"all\")`) \\
    instead."
  )
  remove_greta_all(ask = ask)
}

#' @rdname deprecated-installers
#' @export
destroy_greta_deps <- function(ask = interactive()) {
  lifecycle::deprecate_warn(
    when = "0.6.0",
    what = "destroy_greta_deps()",
    with = "greta_remove()",
    details = "Use `greta_remove(\"env\")` and `greta_remove(\"miniconda\")`, \\
    or `greta_remove()` to also clear the uv cache and stored preference."
  )
  if (!user_agrees(ask = ask, question = "Remove greta env and miniconda?")) {
    return(invisible(FALSE))
  }
  env_removed <- remove_greta_env_impl(ask = FALSE)
  mc_removed <- remove_miniconda_impl(ask = FALSE)
  removed <- c(
    if (env_removed) "the 'greta-env-tf2' conda environment",
    if (mc_removed) "miniconda"
  )
  anything_removed <- length(removed) > 0
  if (anything_removed) {
    cli::cli_inform(c("v" = "Successfully removed {removed}."))
  }
  invisible(anything_removed)
}

#' @rdname deprecated-installers
#' @export
reinstall_greta_env <- function(timeout = 5, ask = interactive()) {
  lifecycle::deprecate_warn(
    when = "0.6.0",
    what = "reinstall_greta_env()",
    with = "reinstall_greta_deps()"
  )
  remove_greta_env_impl(ask = ask)
  greta_create_conda_env(timeout = timeout)
}

#' @rdname deprecated-installers
#' @export
reinstall_miniconda <- function(timeout = 5, ask = interactive()) {
  lifecycle::deprecate_warn(
    when = "0.6.0",
    what = "reinstall_miniconda()",
    with = "reinstall_greta_deps()"
  )
  remove_miniconda_impl(ask = ask)
  greta_install_miniconda(timeout)
}
