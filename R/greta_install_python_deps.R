#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param timeout
#' @return
#' @author Nicholas Tierney
#' @export
greta_install_python_deps <- function(timeout) {

  callr_conda_install <- r_process_options(
    func = function() {
      reticulate::conda_install(
        envname = "greta-env",
        packages = c(
          "numpy==1.16.4",
          "tensorflow-probability==0.7.0",
          "tensorflow==1.14.0"
        )
      )
    }
  )

  install_python_modules <- new_install_process(
    callr_process = callr_conda_install,
    timeout = timeout,
    cli_start_msg = "Installing python modules into 'greta-env' conda \\
                     environment, this may take a few minutes",
    cli_end_msg = "Python modules installed!"
  )
  greta_stash$conda_install_notes <- install_python_modules$output_notes
  cli_ul("To see full installation notes run:")
  cli_ul("{.code greta_notes_conda_install()}")

}
