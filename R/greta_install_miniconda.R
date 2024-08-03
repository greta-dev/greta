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

  cli::cli_ul("To see full installation notes run:")
  cli::cli_ul("{.code greta_notes_install_miniconda_output()}")
  cli::cli_ul("To see any error messages, run:")
  cli::cli_ul("{.code greta_notes_install_miniconda_error()}")
}
