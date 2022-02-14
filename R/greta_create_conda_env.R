greta_create_conda_env <- function(timeout) {

  stdout_file <- create_file("out-greta-conda.txt")
  stderr_file <- create_file("err-greta-conda.txt")

  callr_conda_create <- callr::r_process_options(
    func = function() {
      reticulate::conda_create(
        envname = "greta-env",
        python_version = "3.7"
      )
    },
    stdout = stdout_file,
    stderr = stderr_file
  )

  install_conda_create <- new_install_process(
    callr_process = callr_conda_create,
    stdout_file = stdout_file,
    stderr_file = stderr_file,
    timeout = timeout,
    cli_start_msg = "Creating 'greta-env' conda environment using python \\
                      v3.7, this may take a minute",
    cli_end_msg = "greta-env environment created!"
  )

  greta_stash$conda_create_notes <- install_conda_create$output_notes
  greta_stash$conda_create_error <- install_conda_create$output_error

  cli::cli_ul("To see full installation notes run:")
  cli::cli_ul("{.code greta_notes_conda_create_output()}")
  cli::cli_ul("To see any error messages, run:")
  cli::cli_ul("{.code greta_notes_conda_create_error()}")

}
