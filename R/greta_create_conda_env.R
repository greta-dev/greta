greta_create_conda_env <- function(timeout) {

  callr_conda_create <- callr::r_process_options(
    func = function() {
      reticulate::conda_create(
        envname = "greta-env",
        python_version = "3.7"
      )
    }
  )

  install_conda_create <- new_install_process(
    callr_process = callr_conda_create,
    timeout = timeout,
    cli_start_msg = "Creating 'greta-env' conda environment using python \\
                      v3.7, this may take a minute",
    cli_end_msg = "greta-env environment created!"
  )
  greta_stash$conda_create_notes <- install_conda_create$output_notes
  cli::cli_ul("To see full installation notes run:")
  cli::cli_ul("{.code greta_notes_conda_create()}")

}
