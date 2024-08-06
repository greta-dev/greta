#' Install Python dependencies for greta
#'
#' This is a helper function to install Python dependencies needed. This
#'   includes the latest version of Tensorflow version (2.13.0 or higher),
#'   Tensorflow Probability 0.21.0 (or higher), and the latest version of
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
                               manual = FALSE,
                               restart = c("ask", "force", "no"),
                               ...) {

  restart <- rlang::arg_match(
    arg = restart,
    values = c("ask", "force", "no")
  )

  # set warning message length
  options(warning.length = 2000)

  if (manual) {
    reticulate::py_install(
      packages = c(
        'numpy',
        'tensorflow==2.15',
        'tensorflow-probability==0.23.0',
        "keras==2.15.0"
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

  }

    cli_alert_success("Installation of {.pkg greta} dependencies is complete!")

    restart_or_not(restart)

}


restart_or_not <- function(restart){
  # Managing how to restart R
  # requires RStudio and also an interactive session
  has_rstudioapi_pkg <- requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::hasFun("restartSession")

  # Default (if using rstudio) - we ask the user if they want to restart?
  ask_restart <- interactive() && has_rstudioapi_pkg && (restart == "ask")

  # where the user has specified a restart
  user_force_restart <- (restart == "force") &&
    interactive() &&
    has_rstudioapi_pkg

  # Where there is no rstudio/not interactive, suggest restarting.
  suggest_restart <- (restart == "force" | restart == "no") &&
    (!interactive()  | !has_rstudioapi_pkg)

  if (suggest_restart) {
    cli::cli_inform(
      "Restart R, then load {.pkg greta} with: {.code library(greta)}"
    )
    return(invisible())
  }

  if (ask_restart) {
    if (yesno::yesno("Restart R and load greta?")) {
      rstudioapi::restartSession(
        command = "library(greta)",
        clean = TRUE
      )
    }
  }

  if (user_force_restart) {
    cli::cli_inform("Restarting R, then loading {.pkg greta}")
    rstudioapi::restartSession(
      command = "library(greta)",
      clean = TRUE
    )
  }

}
