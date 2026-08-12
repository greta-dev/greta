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

library(greta)

tf_version <- Sys.getenv("GRETA_TF_VERSION", "default")

if (identical(tf_version, "default")) {
  stop(
    "set-deps.R is for pinned cells only, but GRETA_TF_VERSION is 'default'",
    call. = FALSE
  )
}

greta_set_deps(greta_deps_spec(tf_version = tf_version))
