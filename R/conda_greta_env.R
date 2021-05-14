#' @title Create a special conda greta env thing
#'
#' Helper function to install appropriate python packages and python version
#'   for `greta`.
#'
#' @export
create_conda_greta_env <- function(){

  reticulate::conda_create(envname = "greta-env",
                           python_version = "3.7")

  reticulate::conda_install(envname = "greta-env",
                           packages = c("numpy==1.16.4",
                                        "tensorflow-probability==0.7.0",
                                        "tensorflow==1.14.0"),
                           pip = TRUE)

}

have_greta_conda_env <- function(){
  "greta-env" %in% reticulate::conda_list()$name
}
