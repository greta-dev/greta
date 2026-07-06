#' Helpers to remove, and reinstall python environments and miniconda
#'
#' This can be useful when debugging greta installation to get to "clean slate".
#' There are five functions:
#'
#'   - [remove_greta_env()] removes the 'greta-env-tf2' conda environment
#'   - [remove_miniconda()] removes miniconda installation
#'   - [remove_reticulate_uv_cache()] removes reticulate's managed uv cache.
#' Note this cache is shared by all R packages that use reticulate's uv (it is
#' not greta-specific), and a system-wide uv cache is left untouched.
#'   - [reinstall_greta_env()] remove 'greta-env-tf2' and reinstall it
#' using [greta_create_conda_env()] (which is used internally).
#'   - [reinstall_miniconda()] removes miniconda and reinstalls it using
#' [greta_install_miniconda()] (which is used internally)
#'
#'   To remove everything at once, see [greta_remove_all_deps()].
#'
#' @param ask Ask for confirmation? Default is `interactive()`.
#'
#' @return Invisibly, TRUE if anything was removed, otherwise FALSE.
#' @export
#' @name reinstallers
#' @seealso [destroy_greta_deps()], [greta_remove_all_deps()]
#'
#' @examples
#' \dontrun{
#' remove_greta_env()
#' remove_miniconda()
#' reinstall_greta_env()
#' reinstall_miniconda()
#' }
remove_greta_env <- function(ask = interactive()) {
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

#' @export
#' @param timeout time in minutes to wait until timeout (default is 5 minutes)
#' @rdname reinstallers
reinstall_greta_env <- function(timeout = 5, ask = interactive()) {
  remove_greta_env(ask = ask)
  greta_create_conda_env(timeout = timeout)
}

#' @export
#' @rdname reinstallers
remove_miniconda <- function(ask = interactive()) {
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

#' @param timeout time in minutes to wait until timeout (default is 5 minutes)
#' @rdname reinstallers
#' @export
reinstall_miniconda <- function(timeout = 5, ask = interactive()) {
  remove_miniconda(ask = ask)
  greta_install_miniconda(timeout)
}

#' @rdname reinstallers
#' @export
remove_reticulate_uv_cache <- function(ask = interactive()) {
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
      "Remove reticulate's managed uv cache at ",
      uv_cache,
      "? This is shared by other R packages that use reticulate's uv."
    )
  )
  if (confirmed) {
    cli::cli_alert_info("removing reticulate's managed uv cache", wrap = TRUE)
    unlink(uv_cache, recursive = TRUE, force = TRUE)
    cli::cli_alert_success("reticulate uv cache removed!", wrap = TRUE)
    return(invisible(TRUE))
  }
  invisible(FALSE)
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
  remove_greta_env(ask = FALSE)
  remove_miniconda(ask = FALSE)
  install_greta_deps(
    deps = deps,
    timeout = timeout,
    restart = restart
  )
}

#' Remove greta dependencies and remove miniconda
#'
#' Sometimes when installing greta you might encounter an error and the best
#'   thing to do is start from a clean slate. This function does two things:
#'   1. Removes the "greta-env-tf2" with [remove_greta_env()]
#'   2. Removes the miniconda installation with [remove_miniconda()]
#' @param ask Ask for confirmation? Default is `interactive()`.
#' @return Invisibly, TRUE if anything was removed, otherwise FALSE.
#' @export
destroy_greta_deps <- function(ask = interactive()) {
  if (!user_agrees(ask = ask, question = "Remove greta env and miniconda?")) {
    return(invisible(FALSE))
  }
  env_removed <- remove_greta_env(ask = FALSE)
  mc_removed <- remove_miniconda(ask = FALSE)
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

#' Remove all greta Python dependencies (nuclear reset)
#'
#' A "nuclear" reset that returns greta to a blank slate. Asks once for
#'   confirmation, it removes:
#'   1. the 'greta-env-tf2' conda environment, with [remove_greta_env()]
#'   2. the miniconda installation, with [remove_miniconda()]
#'   3. reticulate's managed uv cache (if any), with [remove_reticulate_uv_cache()]
#'   4. greta's stored Python preference (set via [greta_set_python_uv()] and
#'      friends)
#'
#'   This is broader than [destroy_greta_deps()], which only removes the conda
#'   environment and miniconda. Note that a system-wide uv cache is *not* removed
#'   (it is managed by uv itself, and shared beyond reticulate). After running
#'   this, restart R; greta reinstalls what it needs on next use.
#'
#' @param ask Ask for confirmation? Default is `interactive()`.
#'
#' @return Invisibly, TRUE if anything was removed, otherwise FALSE.
#' @seealso [reinstallers], [destroy_greta_deps()]
#' @export
#' @examples
#' \dontrun{
#' greta_remove_all_deps()
#' }
greta_remove_all_deps <- function(ask = interactive()) {
  if (
    !user_agrees(
      ask = ask,
      question = paste0(
        "Remove ALL of greta's Python dependencies (conda env, miniconda, ",
        "reticulate's uv cache, stored preference)?"
      )
    )
  ) {
    return(invisible(FALSE))
  }
  env_removed <- remove_greta_env(ask = FALSE)
  mc_removed <- remove_miniconda(ask = FALSE)
  cache_removed <- remove_reticulate_uv_cache(ask = FALSE)
  clear_greta_python_backend()
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
      "v" = "Cleared any stored greta Python preference.",
      "i" = "Restart R; greta will reinstall what it needs on next use."
    ))
  } else {
    cli::cli_inform(c(
      "i" = "Nothing to remove.",
      "v" = "Cleared any stored greta Python preference."
    ))
  }
  invisible(anything_removed)
}
