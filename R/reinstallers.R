#' Helpers to remove, and reinstall python environments and miniconda
#'
#' This can be useful when debugging greta installation to get to "clean slate".
#'   There are four functions:
#'
#'   - `remove_greta_env()` removes the 'greta-env' conda environment
#'   - `remove_miniconda()` removes miniconda installation
#'   - `reinstall_greta_env()` remove 'greta-env' and reinstall it using `greta_create_conda_env()` (which is used internally).
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
  cli::cli_alert_info("removing 'greta-env' conda environment")
      reticulate::conda_remove(
        envname = "greta-env"
      )
  cli::cli_alert_success("greta-env environment removed!")
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
    fs::dir_delete(path_to_miniconda)
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
