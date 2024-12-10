#' Helpers to remove, and reinstall python environments and miniconda
#'
#' This can be useful when debugging greta installation to get to "clean slate".
#'   There are four functions:
#'
#'   - [remove_greta_env()] removes the 'greta-env-tf2' conda environment
#'   - [remove_miniconda()] removes miniconda installation
#'   - [reinstall_greta_env()] remove 'greta-env-tf2' and reinstall it
#'     using [greta_create_conda_env()] (which is used internally).
#'   - [reinstall_miniconda()] removes miniconda and reinstalls it using
#'     [greta_install_miniconda()] (which is used internally)
#'
#' @return invisible
#' @export
#' @name reinstallers
#' @seealso [destroy_greta_deps()]
#'
#' @examples
#' \dontrun{
#' remove_greta_env()
#' remove_miniconda()
#' reinstall_greta_env()
#' reinstall_miniconda()
#' }
remove_greta_env <- function(){
  cli::cli_alert_info("removing 'greta-env-tf2' conda environment",
                      wrap = TRUE)
  reticulate::conda_remove(
    envname = "greta-env-tf2"
  )
  cli::cli_alert_success("greta-env-tf2 environment removed!",
                         wrap = TRUE)
}

#' @export
#' @param timeout time in minutes to wait until timeout (default is 5 minutes)
#' @rdname reinstallers
reinstall_greta_env <- function(timeout = 5){
  remove_greta_env()
  greta_create_conda_env(timeout = timeout)
}

#' @export
#' @rdname reinstallers
remove_miniconda <- function(){
  path_to_miniconda <- reticulate::miniconda_path()
  if (!file.exists(path_to_miniconda)){
    cli::cli_alert_info("No miniconda files found at {path_to_miniconda}",
                        wrap = TRUE)
    return(invisible())
  }
  if (yesno::yesno("Are you sure you want to delete miniconda from ",
                   path_to_miniconda,"?") ){
    cli::cli_alert_info("removing 'miniconda' installation",
                        wrap = TRUE)
    unlink(path_to_miniconda, recursive = TRUE)
    cli::cli_alert_success("'miniconda' successfully removed!",
                           wrap = TRUE)
  } else {
    return(invisible())
  }

}

#' @param timeout time in minutes to wait until timeout (default is 5 minutes)
#' @rdname reinstallers
#' @export
reinstall_miniconda <- function(timeout = 5){
  remove_miniconda()
  greta_install_miniconda(timeout)
}

#' @rdname install_greta_deps
#' @export
#' @examples
#' \dontrun{
#' # to help troubleshoot your greta installation, this can help resolve some
#' # issues with installing greta dependencies
#' reinstall_greta_deps()
#' }
reinstall_greta_deps <- function(deps = greta_deps_spec(),
                                 timeout = 5,
                                 restart = c("ask", "force", "no")){
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
destroy_greta_deps <- function(){
  cli::cli_progress_step(
    msg = "You are removing greta env and miniconda",
    msg_done = c("You have successfully removed greta env and miniconda")
  )
  remove_greta_env()
  remove_miniconda()
}
