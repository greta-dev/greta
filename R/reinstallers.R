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
#' @return invisible
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
remove_greta_env <- function() {
  if (!have_greta_conda_env()) {
    cli::cli_alert_info(
      "No 'greta-env-tf2' conda environment found.",
      wrap = TRUE
    )
    return(invisible())
  }
  if (
    yesno::yesno(
      "Are you sure you want to remove the 'greta-env-tf2' conda environment?"
    )
  ) {
    cli::cli_alert_info(
      "removing 'greta-env-tf2' conda environment",
      wrap = TRUE
    )
    reticulate::conda_remove(
      envname = "greta-env-tf2"
    )
    cli::cli_alert_success("greta-env-tf2 environment removed!", wrap = TRUE)
  } else {
    return(invisible())
  }
}

#' @export
#' @param timeout time in minutes to wait until timeout (default is 5 minutes)
#' @rdname reinstallers
reinstall_greta_env <- function(timeout = 5) {
  remove_greta_env()
  greta_create_conda_env(timeout = timeout)
}

#' @export
#' @rdname reinstallers
remove_miniconda <- function() {
  path_to_miniconda <- reticulate::miniconda_path()
  if (!file.exists(path_to_miniconda)) {
    cli::cli_alert_info(
      "No miniconda files found at {path_to_miniconda}",
      wrap = TRUE
    )
    return(invisible())
  }
  if (
    yesno::yesno(
      "Are you sure you want to delete miniconda from ",
      path_to_miniconda,
      "?"
    )
  ) {
    cli::cli_alert_info("removing 'miniconda' installation", wrap = TRUE)
    unlink(path_to_miniconda, recursive = TRUE)
    cli::cli_alert_success("'miniconda' successfully removed!", wrap = TRUE)
  } else {
    return(invisible())
  }
}

#' @param timeout time in minutes to wait until timeout (default is 5 minutes)
#' @rdname reinstallers
#' @export
reinstall_miniconda <- function(timeout = 5) {
  remove_miniconda()
  greta_install_miniconda(timeout)
}

#' @rdname reinstallers
#' @export
remove_reticulate_uv_cache <- function() {
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
    return(invisible())
  }
  confirmed <- yesno::yesno(
    "Remove reticulate's managed uv cache at ",
    uv_cache,
    "? This is shared by other R packages that use reticulate's uv."
  )
  if (confirmed) {
    cli::cli_alert_info("removing reticulate's managed uv cache", wrap = TRUE)
    unlink(uv_cache, recursive = TRUE, force = TRUE)
    cli::cli_alert_success("reticulate uv cache removed!", wrap = TRUE)
  }
  invisible()
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
  restart = c("ask", "force", "no")
) {
  remove_greta_env()
  remove_miniconda()
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
#'   1. Removes the "greta-tf2-env" with [remove_greta_env()]
#'   2. Removes the miniconda installation with [remove_miniconda()]
#'
#' @return nothing
#' @export
destroy_greta_deps <- function() {
  cli::cli_progress_step(
    msg = "You are removing greta env and miniconda",
    msg_done = c("You have successfully removed greta env and miniconda")
  )
  remove_greta_env()
  remove_miniconda()
}

#' Remove all greta Python dependencies (nuclear reset)
#'
#' A "nuclear" reset that returns greta to a blank slate. Asking for
#'   confirmation at each destructive step, it removes:
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
#' @return invisible
#' @seealso [reinstallers], [destroy_greta_deps()]
#' @export
#' @examples
#' \dontrun{
#' greta_remove_all_deps()
#' }
greta_remove_all_deps <- function() {
  remove_greta_env()
  remove_miniconda()
  remove_reticulate_uv_cache()
  clear_greta_python_backend()
  cli::cli_inform(c(
    "v" = "Removed greta's Python dependencies and stored preference.",
    "i" = "Restart R; greta will reinstall what it needs on next use."
  ))
  invisible()
}
