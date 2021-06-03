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
#'  If you prefer to maintain your own conda environment, you can do this
#'  instead of greta setting up the "greta-env" conda environment. You will
#'  need to ensure that you have the currently required versions of tensorflow
#'  (version 1.14.0), and tensorflow-probability (version 0.7.0). If these
#'  versions are installed, you will not be prompted to set up the special
#'  installation
#'
#' @export
#'
#' @examples
#' \dontrun{
#' install_greta_deps()
#' }
#' @importFrom reticulate py_available
#' @importFrom tensorflow install_tensorflow
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
    message("\nNo miniconda detected, installing miniconda\n")
    reticulate::install_miniconda()
  }

  message("\nCreating the `greta-env` conda environment, using python v3.7\n")
  reticulate::conda_create(
    envname = "greta-env",
    python_version = "3.7"
  )

  message("\nInstalling python packages into greta-env conda environment\n")
  reticulate::conda_install(
    envname = "greta-env",
    packages = c("numpy==1.16.4",
                 "tensorflow-probability==0.7.0",
                 "tensorflow==1.14.0")
  )

  # # switch to using this greta environment now
  # if (reticulate::)
  # use_greta_conda_env()
  #
  # success <- check_tf_version()
  #
  # # evaluate installation and report back to the user
  # if (success) {
  #   message("greta dependencies successfully installed, no need to restart")
  # } else {
  #   message("installation of dependencies failed, complain to Nick Tierney")
  # }
  #
  # invisible(success)

  message(
    "\nInstallation complete. Please open a fresh R session and load greta with:",
    "\n\n  ",
    "library(greta)",
    "\n"
  )

}
