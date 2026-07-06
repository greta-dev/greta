tf <- tfp <- NULL

# crate the node list object whenever the package is loaded
.onLoad <- function(libname, pkgname) {
  # nolint

  # resolve issue with .keras directory
  Sys.setenv(
    "KERAS_HOME" = normalizePath(
      tools::R_user_dir("greta", "cache"),
      mustWork = FALSE
    )
  )

  # silence TF's CPU instructions message
  Sys.setenv(TF_CPP_MIN_LOG_LEVEL = 2)

  # switch back to 0-based extraction in tensorflow, and don't warn about
  # indexing with tensors
  options(tensorflow.one_based_extract = FALSE)
  options(tensorflow.extract.warn_tensors_passed_asis = FALSE)

  # default float type
  options(greta_tf_float = "float64")

  # Resolve which Python backend to use and apply it. By default this is
  # reticulate's managed (uv) environment, which auto-installs a compatible
  # Python + TensorFlow + TensorFlow Probability (see #444). A user-set
  # RETICULATE_PYTHON, a stored greta preference, or an existing greta-env-tf2
  # conda env are respected instead (see R/python_backend.R, #801).
  # RETICULATE_PYTHON as the user set it, captured before apply_greta_python_plan()
  # overwrites it; pending_python_plan() reads it back to predict the next restart.
  greta_stash$reticulate_python_at_load <- Sys.getenv("RETICULATE_PYTHON")
  plan <- greta_python_plan()
  apply_greta_python_plan(plan)
  greta_stash$python_backend <- plan

  tfp <<- reticulate::import("tensorflow_probability", delay_load = TRUE)
  tf <<- reticulate::import("tensorflow", delay_load = TRUE)

  # silence messages about deprecation etc.
  # disable_tensorflow_logging()

  # warn if TF version is bad
  # check_tf_version("startup")
}

.onAttach <- function(libname, pkgname) {
  # nolint

  # When greta has auto-detected an existing greta-env-tf2 conda env, let the
  # user know (once) that they can opt into the managed (uv) environment (#801).
  if (should_nudge_to_managed()) {
    packageStartupMessage(
      cli::format_message(
        c(
          "i" = "greta is using your existing {.val greta-env-tf2} conda \\
            environment.",
          "i" = "To switch to the {.pkg uv} environment, run \\
            {.code greta_set_python_uv()}.",
          ">" = "(This note is only shown once.)"
        )
      )
    )
    mark_greta_hint_shown("conda_to_managed")
  }
}
