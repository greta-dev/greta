use_greta_conda_env <- function() {
  tryCatch(
    expr = reticulate::use_condaenv("greta-env", required = TRUE),
    error = function(e) NULL
  )
}

using_greta_conda_env <- function() {
  config <- reticulate::py_discover_config()
  grepl("greta-env", config$python)
}

have_greta_conda_env <- function(){
  tryCatch(
    expr = "greta-env" %in% reticulate::conda_list()$name,
    error = function(e) FALSE
  )
}
