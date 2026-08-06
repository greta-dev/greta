# Fail when TensorFlow releases a version newer than greta pins.
#
# The install-check matrix lists TF versions by hand, so it goes stale silently
# unless something says otherwise. This is that something: a red job named for
# the problem, rather than a note nobody reads.
#
# Failing here does not mean greta is broken. It means the ceiling in
# greta_deps_default is now behind, and should be raised deliberately -- with a
# new row in the matrix -- or left alone on purpose.

library(greta)

pinned <- greta_deps_spec()$tf_version
latest <- jsonlite::fromJSON(
  "https://pypi.org/pypi/tensorflow/json"
)$info$version

cat("greta pins  :", pinned, "\n")
cat("PyPI latest :", latest, "\n")

if (numeric_version(latest) > numeric_version(pinned)) {
  stop(
    "TensorFlow ", latest, " is released; greta pins ", pinned, ". ",
    "Add a row to the install-check matrix and decide whether to raise ",
    "greta_deps_default$tf.",
    call. = FALSE
  )
}

cat("--- greta pins the latest TensorFlow ---\n")
