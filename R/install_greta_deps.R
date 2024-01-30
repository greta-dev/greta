#' Install Python dependencies for greta
#'
#' This is a helper function to install Python dependencies needed. This
#'   includes the latest version of Tensorflow version (2.8.0 or higher),
#'   Tensorflow Probability 0.16.0 (or higher), and the latest version of
#'   numpy (1.21.0 or higher). These Python modules will be installed into a
#'   virtual or conda environment, named "greta-env-tf2". Note that "virtualenv"
#'   is not available on Windows.
#'
#' @param method Installation method ("virtualenv" or "conda")
#' @param conda The path to a `conda` executable. Use `"auto"` to allow
#'   `reticulate` to automatically find an appropriate `conda` binary. See
#'   **Finding Conda** for more details.
#' @param timeout maximum time in minutes until the installation for each
#'    installation component times out and exits. Default is 5 minutes per
#'    installation component.
#' @param manual logical. Skip the fancy installation and just run:
#'     ```
#'     reticulate::py_install(
#'         packages = c(
#'           'numpy',
#'           'tensorflow',
#'           'tensorflow-probability'
#'           ),
#'           pip = TRUE
#'        )
#'     ```
#'
#' @param ... Optional arguments, reserved for future expansion.
#'
#' @note This will automatically install Miniconda (a minimal version of the
#'  Anaconda scientific software management system), create a 'conda'
#'  environment for greta named 'greta-env-tf2' with required python and python
#'  package versions, and forcibly switch over to using that conda environment.
#'
#'  If you don't want to use conda or the "greta-env-tf2" conda environment, you
#'  can install these specific versions of tensorflow (version 2.6.0), and
#'  tensorflow-probability (version 0.14.1), and ensure that the python
#'  environment that is initialised in this R session has these versions
#'  installed. This is now always straightforward, so we recommend installing
#'  the python packages using `install_greta_deps()` for most users.
#'
#' @name install_greta_deps
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
                               manual = FALSE,
                               ...) {

  # set warning message length
  options(warning.length = 2000)

  if (manual) {
    reticulate::py_install(
      packages = c(
        'numpy',
        'tensorflow==2.15',
        'tensorflow-probability'
      ),
      envname = "greta-env-tf2",
      pip = TRUE
    )
  } else if (!manual) {

    # install miniconda if needed
    if (!have_conda()) {
      greta_install_miniconda(timeout)
    }

    if (!have_greta_conda_env()) {
      greta_create_conda_env(timeout)
    }

    greta_install_python_deps(timeout)

    cli_alert_success("Installation of {.pkg greta} dependencies is complete!")
    cli_ul("Restart R, then load {.pkg greta} with: {.code library(greta)}")

  }


}
