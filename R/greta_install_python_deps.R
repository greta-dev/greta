greta_install_python_deps <- function(timeout) {

  callr_conda_install <- callr::r_process_options(
    func = function() {
      tensorflow::install_tensorflow(envname = "greta-env")
      reticulate::py_install(packages = "tensorflow-probability==0.14.1",
                             pip = TRUE,
                             envname = "greta-env",
                             method = "conda")

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
  cli::cli_ul("To see full installation notes run:")
  cli::cli_ul("{.code greta_notes_conda_install()}")

}
