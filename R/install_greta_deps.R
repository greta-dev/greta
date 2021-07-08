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
                               ...) {

  # can we capture the output from all these installation steps, suppress them,
  # but give the user the option to print them after the fact for debugging
  # purposes?

  # install miniconda if needed
  if (!have_conda()) {
    # perhaps add something here to only do this interactively and add a
    # prompt?
    callr_install_miniconda <- r_process_options(
      func = function(){
        reticulate::install_miniconda()
      }
    )

    cli_process_start("No {.pkg miniconda} detected, installing \\
                      {.pkg miniconda}")
    r_install_miniconda <- r_process$new(callr_install_miniconda)
    r_install_miniconda$wait()
    greta_stash$install_miniconda_notes <- r_install_miniconda$read_output()
    cli_process_done(msg_done = "{.pkg miniconda} installed!")
    cli_ul("To see full installation notes run:")
    cli_ul("{.code greta_notes_install_miniconda()}")
  }

  callr_conda_create <- r_process_options(
    func = function(){
      reticulate::conda_create(
        envname = "greta-env",
        python_version = "3.7"
      )
    }
  )

  cli_process_start("Creating 'greta-env' conda environment using python v3.7")
  r_conda_create <- r_process$new(callr_conda_create)
  r_conda_create$wait()
  greta_stash$conda_create_notes <- r_conda_create$read_output()
  cli_process_done()
  cli_ul("To see full installation notes run:")
  cli_ul("{.code greta_notes_conda_create()}")

  callr_conda_install <- r_process_options(
    func = function(){
      reticulate::conda_install(
        envname = "greta-env",
        packages = c("numpy==1.16.4",
                     "tensorflow-probability==0.7.0",
                     "tensorflow==1.14.0")
      )
    }
  )

  cli_process_start(
    "Installing python packages into 'greta-env' conda environment"
    )

  r_conda_install <- r_process$new(callr_conda_install)
  r_conda_install$wait()
  greta_stash$conda_install_notes <- r_conda_install$read_output()
  cli_process_done()
  cli_ul("To see full installation notes run:")
  cli_ul("{.code greta_notes_conda_install()}")

  cli_alert_success("Installation complete!")
  cli_ul("Restart R, then load greta with: {.code library(greta)}")
}
