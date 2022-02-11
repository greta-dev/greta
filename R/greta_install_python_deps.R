greta_install_python_deps <- function(timeout) {

  stdout_file <- file.path("out-python-deps.txt")
  file.create(stdout_file)
  stderr_file <- file.path("err-python-deps.txt")
  file.create(stderr_file)

  callr_conda_install <- callr::r_process_options(
    func = function() {
      reticulate::conda_install(
        envname = "greta-env",
        packages = c(
          "numpy==1.16.4",
          "tensorflow-probability==0.7.0",
          "tensorflow==1.14.0"
        )
      )
    },
    stdout = stdout_file,
    stderr = stderr_file
  )

  install_python_modules <- new_install_process(
    callr_process = callr_conda_install,
    timeout = timeout,
    stdout_file = stdout_file,
    stderr_file = stderr_file,
    cli_start_msg = "Installing python modules into 'greta-env' conda \\
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
