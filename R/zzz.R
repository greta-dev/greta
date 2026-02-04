tf <- tfp <- NULL

.onLoad <- function(libname, pkgname) {
  # resolve issue with .keras directory
  Sys.setenv(
    "KERAS_HOME" = normalizePath(
      tools::R_user_dir("greta", "cache"),
      mustWork = FALSE
    )
  )

  # silence TF's CPU instructions message
  Sys.setenv(TF_CPP_MIN_LOG_LEVEL = 2)

  # default float type
  options(greta_tf_float = "float64")

  # switch back to 0-based extraction in tensorflow, and don't warn about
  # indexing with tensors
  options(tensorflow.one_based_extract = FALSE)
  options(tensorflow.extract.warn_tensors_passed_asis = FALSE)

  # Use conda environment - more reliable for parallel execution
  if (have_greta_conda_env()) {
    use_greta_conda_env()
  }

  tfp <<- reticulate::import("tensorflow_probability", delay_load = TRUE)
  tf <<- reticulate::import("tensorflow", delay_load = TRUE)
}
