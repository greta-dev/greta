greta_create_conda_env <- function(timeout = 5,
                                   greta_py_version = "3.11") {

  options(warning.length = 2000)

  stdout_file <- create_temp_file("out-greta-conda")
  stderr_file <- create_temp_file("err-greta-conda")

  callr_conda_create <- callr::r_process_options(
    func = function(greta_py_version) {
      reticulate::conda_create(
        envname = "greta-env-tf2",
        python_version = greta_py_version
      )
    },
    args = list(greta_py_version = greta_py_version),
    stdout = stdout_file,
    stderr = stderr_file
  )

  start_msg <- glue::glue(
    "Creating 'greta-env-tf2' conda environment using python \\
    v{greta_py_version}, this may take a minute"
    )

  install_conda_create <- new_install_process(
    callr_process = callr_conda_create,
    stdout_file = stdout_file,
    stderr_file = stderr_file,
    greta_py_version = greta_py_version,
    timeout = timeout,
    cli_start_msg = start_msg,
    cli_end_msg = "greta-env-tf2 environment created!"
  )

  greta_stash$conda_create_notes <- install_conda_create$output_notes
  greta_stash$conda_create_error <- install_conda_create$output_error

  cli::cli_ul("To see full installation notes run:")
  cli::cli_ul("{.code greta_notes_conda_create_output()}")
  cli::cli_ul("To see any error messages, run:")
  cli::cli_ul("{.code greta_notes_conda_create_error()}")

}
