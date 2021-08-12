#' Install Python dependencies for greta
#'
#' This is a helper function to install Python dependencies needed. This
#'   includes Tensorflow version 1.14.0, Tensorflow Probability 0.7.0, and
#'   numpy version 1.16.4. These Python modules will be installed into a
#'   virtual or conda environment, named "greta-env". Note that "virtualenv" is
#'   not available on Windows.
#'
#' @param method Installation method ("virtualenv" or "conda")
#' @param conda The path to a `conda` executable. Use `"auto"` to allow
#'   `reticulate` to automatically find an appropriate `conda` binary. See
#'   **Finding Conda** for more details.
#' @param timeout maximum time in minutes until the installation for each
#'    installation component times out and exits. Default is 5 minutes per
#'    installation component.
#' @param ... Optional arguments, reserved for future expansion.
#'
#' @note This will automatically install Miniconda (a minimal version of the
#'  Anaconda scientific software management system), create a 'conda'
#'  environment for greta named 'greta-env' with required python and python
#'  package versions, and forcibly switch over to using that conda environment.
#'
#'  If you don't want to use conda or the "greta-env" conda environment, you
#'  can install these specific versions of tensorflow (version 1.14.0), and
#'  tensorflow-probability (version 0.7.0), and ensure that the python
#'  environment that is initialised in this R session has these versions
#'  installed. This is now always straightforward, so we recommend installing
#'  the python packages using `install_greta_deps()` for most users.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' install_greta_deps()
#' }
#' @importFrom reticulate py_available
#' @importFrom tensorflow install_tensorflow
#' @importFrom reticulate conda_create
#' @importFrom reticulate conda_install
#' @importFrom cli cli_alert_info
#' @importFrom cli cli_process_start
#' @importFrom cli cli_process_done
#' @importFrom cli cli_ul
#' @importFrom callr r_process_options
#' @importFrom callr r_process
#' @importFrom cli cli_alert_success
#' @importFrom cli cli_ul
install_greta_deps <- function(method = c("auto", "virtualenv", "conda"),
                               conda = "auto",
                               timeout = 5,
                               ...) {

  # set warning message length
  options(warning.length = 2000)

  # install miniconda if needed
  if (!have_conda()) {
    callr_install_miniconda <- r_process_options(
      func = function() {
        reticulate::install_miniconda()
      }
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
    cli_ul("To see full installation notes run:")
    cli_ul("{.code greta_notes_install_miniconda()}")
  }

  if (!have_greta_conda_env()) {
    callr_conda_create <- r_process_options(
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
    cli_ul("To see full installation notes run:")
    cli_ul("{.code greta_notes_conda_create()}")
  }

  callr_conda_install <- r_process_options(
    func = function() {
      reticulate::conda_install(
        envname = "greta-env",
        packages = c(
          "numpy==1.16.4",
          "tensorflow-probability==0.7.0",
          "tensorflow==1.14.0"
        )
      )
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
  cli_ul("To see full installation notes run:")
  cli_ul("{.code greta_notes_conda_install()}")

  cli_alert_success("Installation of {.pkg greta} dependencies is complete!")
  cli_ul("Restart R, then load {.pkg greta} with: {.code library(greta)}")
}
