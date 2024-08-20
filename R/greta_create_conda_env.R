#' Create conda environment for greta
#'
#' This function runs [reticulate::conda_create()] inside
#'   [callr::r_process_options()], to create the conda environment,
#'   "greta-env-tf2". This is used within [install_greta_deps()] as part of
#'   setting up python dependencies. It uses a version of python that is
#'   compatible with the versions of tensorflow and tensorflow-probability,
#'   which is established with  [greta_deps_spec()]. We mostly recommend
#'   users use [install_greta_deps()] to manage their python dependency
#'   installation.
#'
#'
#' @param timeout time (minutes) until installation stops. Default is 5 minutes.
#' @param deps dependency specification, see [greta_deps_spec()] for
#'   more details.
#'
#' @return nothing - creates a conda environment for a specific python version
#' @export
greta_create_conda_env <- function(timeout = 5,
                                   deps = greta_deps_spec()) {

  check_greta_deps_spec(deps)

  stdout_file <- create_temp_file("out-greta-conda")
  stderr_file <- create_temp_file("err-greta-conda")

  callr_conda_create <- callr::r_process_options(
    func = function(python_version) {
      reticulate::conda_create(
        envname = "greta-env-tf2",
        python_version = python_version
      )
    },
    args = list(python_version = deps$python_version),
    stdout = stdout_file,
    stderr = stderr_file
  )

  install_conda_create <- new_install_process(
    callr_process = callr_conda_create,
    stdout_file = stdout_file,
    stderr_file = stderr_file,
    timeout = timeout,
    cli_start_msg = glue::glue(
    "Creating 'greta-env-tf2' conda environment using python \\
    v{deps$python_version}, this may take a minute"
    ),
    cli_end_msg = "greta-env-tf2 environment created!"
  )

  greta_stash$conda_create_notes <- install_conda_create$output_notes
  greta_stash$conda_create_error <- install_conda_create$output_error

}
