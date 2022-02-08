greta_install_miniconda <- function(timeout) {

  callr_install_miniconda <- callr::r_process_options(
    func = function() {
      reticulate::install_miniconda()
    },
    stdout = tempfile("out-"),
    stderr = ">&1"
  )

  # if this function doesn't fail, then this code here can be run?
  install_miniconda_process <- new_install_process(
    callr_process = callr_install_miniconda,
    timeout = timeout,
    cli_start_msg = "No {.pkg miniconda} detected, installing \\
                      {.pkg miniconda}, this may take a minute.",
    cli_end_msg = "{.pkg miniconda} installed!"
  )
  greta_stash$miniconda_notes <- install_miniconda_process$output_notes
  cli::cli_ul("To see full installation notes run:")
  cli::cli_ul("{.code greta_notes_install_miniconda()}")
}
