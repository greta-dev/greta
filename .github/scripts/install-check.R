# Install greta on one TensorFlow version and one backend, then check it runs.
#
# Driven by GRETA_TF_VERSION ("default" or a version string) and GRETA_BACKEND
# ("uv" or "conda"), set by .github/workflows/install-check.yaml. Runnable
# locally with those environment variables set.
#
# Reports which stack the resolver actually picked, since that is the question
# this job exists to answer, and it is worth knowing even when the run fails.
#
# Run it against an installed greta. Locally that means installing the branch
# first: library(greta) loads the library copy, so running this from a source
# checkout silently exercises whatever version is installed instead.

library(greta)

tf_version <- Sys.getenv("GRETA_TF_VERSION", "default")
backend <- Sys.getenv("GRETA_BACKEND", "uv")

if (!identical(tf_version, "default")) {
  greta_set_deps(greta_deps_spec(tf_version = tf_version))
}

# The conda environment is built and selected by an earlier workflow step, in
# its own R session. greta_set_python() stores a preference that only takes
# effect on the next load, so installing and exercising in one session silently
# runs against uv instead -- which passes wherever uv works, and reports a
# backend that was never tested.

set.seed(2026)
x <- rnorm(30, mean = 2, sd = 0.5)
mu <- normal(0, 10)
sigma <- normal(0, 3, truncation = c(0, Inf))
distribution(x) <- normal(mu, sigma)

# greta initialises Python lazily, and model() is what triggers it. Asking for
# versions before this point reports whichever environment reticulate resolved
# on its own, which need not be the one greta goes on to use.
m <- model(mu, sigma)

python_path <- reticulate::py_config()$python

# Fail rather than report a green for a backend that was never exercised.
using_conda <- grepl("greta-env-tf2", python_path, fixed = TRUE)
if (identical(backend, "conda") && !using_conda) {
  stop("asked for the conda backend, but python is ", python_path, call. = FALSE)
}
if (identical(backend, "uv") && using_conda) {
  stop("asked for the uv backend, but python is ", python_path, call. = FALSE)
}

resolved_python <- as.character(reticulate::py_config()$version)
resolved_tf <- as.character(tensorflow::tf$`__version__`)
resolved_tfp <- as.character(
  reticulate::import("tensorflow_probability")$`__version__`
)
has_tf_keras <- reticulate::py_module_available("tf_keras")

cat("--- resolved stack ---\n")
cat("backend   :", backend, "\n")
cat("python at :", python_path, "\n")
cat("python    :", resolved_python, "\n")
cat("tensorflow:", resolved_tf, "\n")
cat("tfp       :", resolved_tfp, "\n")
cat("tf_keras  :", has_tf_keras, "\n")

cat("--- exercising greta ---\n")

# Whether these run at all is the question; whether they fit well is the test
# suite's, on a stack it can rely on. opt() covers the Keras optimiser path and
# mcmc() the TFP sampler path, which between them are the two Python surfaces a
# version bump breaks. An error in either fails the job on its own.
fit <- opt(m, optimiser = adam(), max_iterations = 500)
cat("opt mu:", fit$par$mu, "\n")

draws <- mcmc(m, n_samples = 100, warmup = 100, chains = 2, verbose = FALSE)
cat("mcmc draws:", nrow(as.matrix(draws)), "\n")

cat("--- ok ---\n")

# Write the resolved stack to the run page, so the answer to "does this
# combination work" is readable without opening a job log. GitHub renders one
# summary per job, so this is a row per combination rather than a single table.
summary_file <- Sys.getenv("GITHUB_STEP_SUMMARY")
if (nzchar(summary_file)) {
  summary_lines <- c(
    sprintf(
      "### %s, TensorFlow %s, %s backend: works",
      Sys.getenv("RUNNER_OS", "unknown"),
      tf_version,
      backend
    ),
    "",
    "| resolved | version |",
    "|---|---|",
    sprintf("| Python | %s |", resolved_python),
    sprintf("| TensorFlow | %s |", resolved_tf),
    sprintf("| TensorFlow Probability | %s |", resolved_tfp),
    sprintf("| tf-keras present | %s |", has_tf_keras),
    ""
  )
  cat(paste(summary_lines, collapse = "\n"), file = summary_file, append = TRUE)
}
