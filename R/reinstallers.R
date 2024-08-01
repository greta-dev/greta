#' Helpers to remove, and reinstall python environments and miniconda
#'
#' This can be useful when debugging greta installation to get to "clean slate".
#'   There are four functions:
#'
#'   - `remove_greta_env()` removes the 'greta-env-tf2' conda environment
#'   - `remove_miniconda()` removes miniconda installation
#'   - `reinstall_greta_env()` remove 'greta-env-tf2' and reinstall it using `greta_create_conda_env()` (which is used internally).
#'   - `reinstall_miniconda()` removes miniconda and reinstalls it using `greta_install_miniconda()` (which is used internally)
#'
#' @return invisible
#' @export
#' @name reinstallers
#'
#' @examples
#' \dontrun{
#' remove_greta_env()
#' remove_miniconda()
#' reinstall_greta_env()
#' reinstall_miniconda()
#' }
remove_greta_env <- function(){
  cli::cli_alert_info("removing 'greta-env-tf2' conda environment")
  reticulate::conda_remove(
    envname = "greta-env-tf2"
  )
  cli::cli_alert_success("greta-env-tf2 environment removed!")
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
    cli::cli_alert_info("No miniconda files found at {path_to_miniconda}")
    return(invisible())
  }
  if (yesno::yesno("Are you sure you want to delete miniconda from ",
                   path_to_miniconda,"?") ){
    cli::cli_alert_info("removing 'miniconda' installation")
    unlink(path_to_miniconda, recursive = TRUE)
    cli::cli_alert_success("'miniconda' successfully removed!")
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
reinstall_greta_deps <- function(method = c("auto", "virtualenv", "conda"),
                                 conda = "auto",
                                 timeout = 5,
                                 restart = FALSE){
  remove_greta_env()
  remove_miniconda()
  install_greta_deps(method = method,
                     conda = conda,
                     timeout = timeout,
                     restart = restart)
}
