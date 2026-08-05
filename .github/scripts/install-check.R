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

if (identical(backend, "conda")) {
  install_greta_deps(timeout = 45)
  greta_set_python("conda")
}

set.seed(2026)
x <- rnorm(30, mean = 2, sd = 0.5)
mu <- normal(0, 10)
sigma <- normal(0, 3, truncation = c(0, Inf))
distribution(x) <- normal(mu, sigma)

# greta initialises Python lazily, and model() is what triggers it. Asking for
# versions before this point reports whichever environment reticulate resolved
# on its own, which need not be the one greta goes on to use.
m <- model(mu, sigma)

cat("--- resolved stack ---\n")
cat("python    :", as.character(reticulate::py_config()$version), "\n")
cat("tensorflow:", as.character(tensorflow::tf$`__version__`), "\n")
cat("tfp       :", as.character(
  reticulate::import("tensorflow_probability")$`__version__`
), "\n")
cat("tf_keras  :", reticulate::py_module_available("tf_keras"), "\n")

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
