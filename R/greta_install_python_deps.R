greta_install_python_deps <- function(timeout = 5,
                                      python_deps = greta_python_deps()) {

  stdout_file <- create_temp_file("out-python-deps")
  stderr_file <- create_temp_file("err-python-deps")

  callr_conda_install <- callr::r_process_options(
    func = function(python_deps) {
      cli::cli_progress_step(
        msg = "Installing TF (v{python_deps$tf_version})",
        msg_done = "Installed TF (v{python_deps$tf_version})!",
        msg_failed = "Error installing TF (v{python_deps$tf_version})"
        )
      tensorflow::install_tensorflow(
        version = python_deps$tf_version,
        envname = "greta-env-tf2",
        method = "conda"
      )
      dep_tfp <- glue::glue("tensorflow-probability=={python_deps$tfp_version}")
      cli::cli_progress_step(
        msg = "Installing TFP (v{python_deps$tfp_version})",
        msg_done = "Installed TFP (v{python_deps$tfp_version})!",
        msg_failed = "Error installing TFP (v{python_deps$tfp_version})"
      )
      reticulate::py_install(
        packages = dep_tfp,
        pip = TRUE,
        envname = "greta-env-tf2",
        method = "conda"
        )
      },
    args = list(python_deps = python_deps),
    stdout = stdout_file,
    stderr = stderr_file
    )

  install_python_modules <- new_install_process(
    callr_process = callr_conda_install,
    timeout = timeout,
    stdout_file = stdout_file,
    stderr_file = stderr_file,
    cli_start_msg = glue::glue(
    "Installing python modules into 'greta-env-tf2' conda environment, \\
    this may take a few minutes"
    ),
    cli_end_msg = "Python modules installed!"
  )

  greta_stash$conda_install_notes <- install_python_modules$output_notes
  greta_stash$conda_install_error <- install_python_modules$output_error

  cli::cli_ul("To see full installation notes run:")
  cli::cli_ul("{.code greta_notes_conda_install_output()}")
  cli::cli_ul("To see any error messages, run:")
  cli::cli_ul("{.code greta_notes_conda_install_error()}")

}
