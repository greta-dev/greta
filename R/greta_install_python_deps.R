greta_install_python_deps <- function(timeout) {

  stdout_file <- tempfile("out-python-deps")
  stderr_file <- tempfile("err-python-deps")

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
    stdout = tempfile("out-"),
    stderr = ">&1"
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
  cli::cli_ul("To see full installation notes run:")
  cli::cli_ul("{.code greta_notes_conda_install()}")

}
