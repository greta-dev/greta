# load tf probability
tfp <- reticulate::import("tensorflow_probability", delay_load = TRUE)
tf <- reticulate::import("tensorflow", delay_load = TRUE)

# crate the node list object whenever the package is loaded
.onLoad <- function(libname, pkgname) {
  # nolint

  # unset reticulate python environment, for more details, see:
  # https://github.com/greta-dev/greta/issues/444
  Sys.unsetenv("RETICULATE_PYTHON")

  if (have_greta_conda_env()) {
    use_greta_conda_env()
  }

  # silence TF's CPU instructions message
  Sys.setenv(TF_CPP_MIN_LOG_LEVEL = 2)

  # silence messages about deprecation etc.
  # disable_tensorflow_logging()

  # warn if TF version is bad
  # check_tf_version("startup")

  # switch back to 0-based extraction in tensorflow, and don't warn about
  # indexing with tensors
  options(tensorflow.one_based_extract = FALSE)
  options(tensorflow.extract.warn_tensors_passed_asis = FALSE)

  # default float type
  options(greta_tf_float = "float64")
}
