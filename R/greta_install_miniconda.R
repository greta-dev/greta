#' Installs miniconda
#'
#' This installs miniconda using [reticulate::install_miniconda()] inside
#'   [callr::r_process_options()]. Used internally by [install_greta_deps()].
#'   We mostly recommend users use [install_greta_deps()] to manage their
#'   python dependency installation.
#'
#' @param timeout time (minutes) until installation stops. Default is 5 minutes.
#'
#' @return nothing - installs miniconda.
#' @export
greta_install_miniconda <- function(timeout = 5) {

  stdout_file <- create_temp_file("out-miniconda")
  stderr_file <- create_temp_file("err-miniconda")

  callr_install_miniconda <- callr::r_process_options(
    func = function() {
      reticulate::install_miniconda()
    },
    stdout = stdout_file,
    stderr = stderr_file
  )

  # if this function doesn't fail, then this code here can be run?
  install_miniconda_process <- new_install_process(
    callr_process = callr_install_miniconda,
    timeout = timeout,
    stdout_file = stdout_file,
    stderr_file = stderr_file,
    cli_start_msg = "No {.pkg miniconda} detected, installing \\
                      {.pkg miniconda}, this may take a minute.",
    cli_end_msg = "{.pkg miniconda} installed!"
  )

  greta_stash$miniconda_notes <- install_miniconda_process$output_notes
  greta_stash$miniconda_error <- install_miniconda_process$output_error

}
