#' @title Create a special conda greta env thing
#'
#' Helper function to install appropriate python packages and python version
#'   for `greta`.
#'
#' @note This willl automatically install Miniconda (a minimal version of the
#' Anaconda scientific software management system), create a 'conda' environment
#' for greta named 'greta-env' with required python and python package versions,
#' and forcibly switch over to using that conda environment. There should be no
#' need to restart if this was successfull.
#'
#' @export
create_conda_greta_env <- function(){

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

}

use_greta_conda_env <- function() {
  reticulate::use_condaenv("greta-env", required = TRUE)
}

using_greta_conda_env <- function() {
  config <- reticulate::py_discover_config()
  grepl("greta-env", config$python)
}

have_greta_conda_env <- function(){
  "greta-env" %in% reticulate::conda_list()$name
}
