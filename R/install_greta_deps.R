#' Install Python dependencies for greta
#'
#' This is a helper function to install Python dependencies needed. This
#'   includes Tensorflow version 1.14.0, Tensorflow Probability 0.7.0, and
#'   numpy version 1.16.4. These Python modules will be installed into an
#'   "r-tensorflow" virtual or conda environment. Note that "virtualenv" is
#'   not available on Windows.
#'
#' @param method Installation method ("virtualenv" or "conda")
#' @param conda The path to a conda executable. Use "auto" to allow reticulate
#'   to automatically find an appropriate conda binary. See Finding Conda for
#'   more details.
#' @param ... Other arguments passed to [tensorflow::install_tensorflow()].
#'
#' @note This function was heavily inspired by `keras::install_keras()`.
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
  method <- match.arg(method)
  if (is_windows()) {
    method <- "conda"
    have_conda <- !is.null(tryCatch(
      conda_binary(conda),
      error = function(e) {
        NULL
      }
    ))
    if (!have_conda) {
      stop(
        "Dependency installation for greta failed (no conda binary found)\n\n",
        "Install Anaconda for Python 3.x (https://www.anaconda.com/download/#windows)\n",
        "before installing greta dependencies.",
        call. = FALSE
      )
    }
    if (py_available()) {
      stop(
        "You should call install_greta_deps() only in a fresh R session that ",
        "has not yet initialized TensorFlow (this is to avoid DLL in use ",
        "errors during installation)"
      )
    }
  }

  # Check if greta-env conda environment exists, if it doesn't, create it
  if (!using_greta_conda_env()) {
    create_conda_greta_env()
  }

  # If the greta-env does exist, then install into it
  if (using_greta_conda_env()) {
  # install the relevant python packages into the conda environment, using pip
  reticulate::conda_install(
    envname = "greta-env",
    packages = c("numpy==1.16.4",
                 "tensorflow-probability==0.7.0",
                 "tensorflow==1.14.0")
    # pip = TRUE
  )
  }

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
