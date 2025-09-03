tf <- tfp <- NULL

# crate the node list object whenever the package is loaded
.onLoad <- function(libname, pkgname) {
  # nolint

  # silence TF's CPU instructions message
  Sys.setenv(TF_CPP_MIN_LOG_LEVEL = 2)

  # switch back to 0-based extraction in tensorflow, and don't warn about
  # indexing with tensors
  options(tensorflow.one_based_extract = FALSE)
  options(tensorflow.extract.warn_tensors_passed_asis = FALSE)

  # default float type
  options(greta_tf_float = "float64")

  # force reticulate manged python environment, for more details, see:
  # https://github.com/greta-dev/greta/issues/444
  Sys.setenv("RETICULATE_PYTHON" = "managed")

  reticulate::py_require(
    packages = c(
      "tensorflow==2.15.*",
      "tensorflow_probability==0.23.*"
    ),
    python_version = ">=3.9,<=3.11"
  )
  ## most tests pass with the latest package versions, but some of the snapshots
  ## change # need to be reviewed and updated.
  # reticulate::py_require(c("tensorflow", "tensorflow_probability[tf]"))

  tfp <<- reticulate::import("tensorflow_probability", delay_load = TRUE)
  tf <<- reticulate::import("tensorflow", delay_load = TRUE)

  # if (have_greta_conda_env()) {
  #   use_greta_conda_env()
  # }

  # silence messages about deprecation etc.
  # disable_tensorflow_logging()

  # warn if TF version is bad
  # check_tf_version("startup")

}
