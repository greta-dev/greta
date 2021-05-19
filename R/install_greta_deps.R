#' Install Python dependencies for greta
#'
#' This is a helper function to install Python dependencies needed. This
#'   includes Tensorflow version 1.14.0, Tensorflow Probability 0.7.0, and
#'   numpy version 1.16.4. These Python modules will be installed into an
#'   virtual or conda environment, named "greta-env". Note that "virtualenv" is
#'   not available on Windows.
#'
#' @note This willl automatically install Miniconda (a minimal version of the
#' Anaconda scientific software management system), create a 'conda' environment
#' for greta named 'greta-env' with required python and python package versions,
#' and forcibly switch over to using that conda environment. There should be no
#' need to restart if this was successfull.
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
    reticulate::install_miniconda()
  }

  # create the greta conda environment with the required version of python
  reticulate::conda_create(
    envname = "greta-env",
    python_version = "3.7"
  )

  # install the relevant python packages into the conda environment, using pip
  reticulate::conda_install(
    envname = "greta-env",
    packages = c("numpy==1.16.4",
                 "tensorflow-probability==0.7.0",
                 "tensorflow==1.14.0")
    # pip = TRUE
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
