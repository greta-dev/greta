# Build the greta-env-tf2 conda environment and select it.
#
# Runs in its own R session, before install-check.R. greta_set_python() stores a
# preference that the next load reads, so the exercise has to happen in a fresh
# session -- otherwise it runs against uv and reports a conda pass that never
# touched conda.

library(greta)

tf_version <- Sys.getenv("GRETA_TF_VERSION", "default")

deps <- if (identical(tf_version, "default")) {
  greta_deps_spec()
} else {
  greta_deps_spec(tf_version = tf_version)
}

install_greta_deps(deps = deps, timeout = 45)
greta_set_python("conda")
