#' Install Python dependencies for greta
#'
#' This is a helper function to install Python dependencies needed. This
#'   includes the latest version of Tensorflow version (2.15.0),
#'   Tensorflow Probability (0.23.0), keras 2.15.0) and the latest version of
#'   numpy. These Python modules will be installed into a user specified
#'   virtual or conda environment, named "greta-env-tf2". Note that
#'   "virtualenv" is not available on Windows, but conda envs are. See details
#'   below on restarting defaults.
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
#' @param restart character. Restart R after installation? Default is "ask".
#'  Other options are, "force", and "no". Using "force" will will force a
#'  restart after installation. Using  "no" will not restart. Note that this
#'  only restarts R during interactive sessions, and only in RStudio.
#'
#' @param ... Optional arguments, reserved for future expansion.
#'
#' @details
#'  By default, if using RStudio, it will now ask you if you want to restart
#'  the R session. If the session is not interactive, or is not in RStudio,
#'  it will not restart. You can also override this with `restart = TRUE`.
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
                               scheme = c("default", "manual", "aws"),
                               manual = FALSE,
                               restart = c("ask", "force", "no"),
                               ...) {

  # set warning message length
  options(warning.length = 2000)

  restart <- rlang::arg_match(
    arg = restart,
    values = c("ask", "force", "no")
  )

  scheme <- rlang::arg_match(
    arg = scheme,
    values = c("default", "manual", "aws")
  )

  install_method <- switch(scheme,
                           default = default_install(timeout = timeout),
                           manual = manual_install,
                           aws = aws_install)

  install_method()

  cli_alert_success("Installation of {.pkg greta} dependencies is complete!")

  restart_or_not(restart = restart)

}

default_install <- function(timeout = 5){

  installer <- function(timeout = timeout){
    # install miniconda if needed
    if (!have_conda()) {
      greta_install_miniconda(timeout = timeout)
    }

    if (!have_greta_conda_env()) {
      greta_create_conda_env(timeout = timeout)
    }

    greta_install_python_deps(timeout = timeout)

  }

  installer

}

create_py_installer <- function(tf_version = "2.15.0",
                             tfp_version = "0.23.0",
                             keras_version = "2.15.0",
                             py_version = "3.11",
                             versions = c("exact", "gte")){

  versions <- rlang::arg_match(
    arg = versions,
    values = c("exact", "gte")
  )

  gte_exact <- switch(versions,
                      exact = "==",
                      gte = ">=")

  v_tf <- glue::glue("tensorflow{gte_exact}{tf_version}")
  v_tfp <- glue::glue("tensorflow-probability{gte_exact}{tfp_version}")

  # in case you don't want to install keras, set it to NULL
  if (!is.null(keras_version)){
    v_keras <- glue::glue("keras{gte_exact}{keras_version}")
  } else
  v_keras <- NULL

  py_installer <- function(){
    reticulate::py_install(
      packages = c(
        'numpy',
        v_tf,
        v_tfp,
        v_keras
      ),
      envname = "greta-env-tf2",
      python_version = py_version,
      pip = TRUE
    )
  }

  py_installer

}

manual_install <- create_py_installer(
  tf_version = "2.15.0",
  tfp_version = "0.23.0",
  keras_version = "2.15.0",
  py_version = "3.11",
  versions = "exact"
)

aws_install <- create_py_installer(
  tf_version = "2.11.0",
  tfp_version = "0.19.0",
  keras_version = NULL,
  py_version = "3.8.15",
  versions = "exact"
)
