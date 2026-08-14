# Store the dependency versions this matrix cell asks for, in its own R session.
#
# greta reads the stored versions in .onLoad(), where it declares its Python
# requirements with reticulate::py_require(). Calling greta_set_deps() after
# library(greta) writes a preference the running session has already read past:
# the environment then resolves on greta's defaults, and every cell of the
# matrix silently exercises the same TensorFlow version while reporting the one
# it asked for. Setting the versions here, in a session that exits before the
# exercise step loads greta, is what makes the pin take effect.
#
# Only pinned cells run this; 'default' means "whatever greta_deps_default$tf
# is", which is what an unset preference already gives.
#
# greta refuses a TensorFlow version outside the range it supports, so for a
# cell below the floor this is where the run ends. That refusal is a result --
# it is the answer to "can I use this version?" -- so it is recorded as the
# cell's row rather than crashing the job, and the exercise step is skipped so
# it cannot overwrite that row with a run of the default versions.

library(greta)

# run from the repository root, as the workflow does
source(".github/scripts/cell-row.R")

tf_version <- Sys.getenv("GRETA_TF_VERSION", "default")
expected <- Sys.getenv("GRETA_EXPECTED", "works")

if (identical(tf_version, "default")) {
  stop(
    "set-deps.R is for pinned cells only, but GRETA_TF_VERSION is 'default'",
    call. = FALSE
  )
}

deps <- tryCatch(
  greta_deps_spec(tf_version = tf_version),
  error = function(e) {
    cell <- new_cell()
    cell$outcome <- "refused by greta"
    cell$detail <- first_line(conditionMessage(e))
    write_cell_row(cell)
    skip_exercise()
    cat("greta will not accept TensorFlow ", tf_version, ":\n", sep = "")
    cat(cell$detail, "\n")
    if (identical(expected, "unsupported")) {
      cat("expected not to work; recorded in the summary, not failed\n")
      quit(status = 0)
    }
    stop("expected this version to be supported, but greta refused it",
      call. = FALSE
    )
  }
)

greta_set_deps(deps)
