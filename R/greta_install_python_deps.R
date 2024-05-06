greta_install_python_deps <- function(timeout) {

  stdout_file <- create_temp_file("out-python-deps")
  stderr_file <- create_temp_file("err-python-deps")

  callr_conda_install <- callr::r_process_options(
    func = function() {
      tensorflow::install_tensorflow(
        version = "2.16.1",
        envname = "greta-env-tf2"
        )
      reticulate::py_install(packages = "tensorflow-probability[tf]",
                             pip = TRUE,
                             envname = "greta-env-tf2",
                             method = "conda")
      },
    stdout = stdout_file,
    stderr = stderr_file
    )

  install_python_modules <- new_install_process(
    callr_process = callr_conda_install,
    timeout = timeout,
    stdout_file = stdout_file,
    stderr_file = stderr_file,
    cli_start_msg = "Installing python modules into 'greta-env-tf2' conda \\
                     environment, this may take a few minutes",
    cli_end_msg = "Python modules installed!"
  )

  greta_stash$conda_install_notes <- install_python_modules$output_notes
  greta_stash$conda_install_error <- install_python_modules$output_error

  cli::cli_ul("To see full installation notes run:")
  cli::cli_ul("{.code greta_notes_conda_install_output()}")
  cli::cli_ul("To see any error messages, run:")
  cli::cli_ul("{.code greta_notes_conda_install_error()}")

}
