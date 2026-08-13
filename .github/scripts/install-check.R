# Install greta on one TensorFlow version and one backend, then check it runs.
#
# Driven by GRETA_TF_VERSION ("default" or a version string), GRETA_BACKEND
# ("uv" or "conda") and GRETA_EXPECTED ("works" or "unsupported"), set by
# .github/workflows/install-check.yaml. Runnable locally with those set.
#
# Every cell writes a row, whether it worked or not, and
# install-check-summary.R collects them into one table on the run page. A
# combination that does *not* work is as much a part of the answer as one that
# does -- "which versions can I use?" cannot be read off a table listing only
# successes -- so a cell that cannot even load Python still reports what it was
# asked for and why it failed.
#
# The job fails when reality disagrees with GRETA_EXPECTED, in either
# direction. A supported version that breaks is a regression; an unsupported
# version that starts working means greta's floor is now too high and
# check_greta_tf_supported() is refusing something it need not. Both deserve a
# red cell; neither deserves a silent one.
#
# Run it against an installed greta. Locally that means installing the branch
# first: library(greta) loads the library copy, so running this from a source
# checkout silently exercises whatever version is installed instead.

library(greta)

tf_version <- Sys.getenv("GRETA_TF_VERSION", "default")
backend <- Sys.getenv("GRETA_BACKEND", "uv")
expected <- Sys.getenv("GRETA_EXPECTED", "works")

# run from the repository root, as the workflow does
source(".github/scripts/cell-row.R")

cell <- new_cell()
write_row <- function() write_cell_row(cell)

# Everything that can fail lives in here, so a single tryCatch below covers the
# lot and the row is written exactly once, on every path. on.exit() will not do:
# registered at the top level of a script it has no enclosing call to unwind, so
# it does not fire when Rscript aborts -- which is precisely the case that has to
# report.
#
# The conda environment is built and selected by an earlier workflow step, and
# the pinned dependency versions are stored by another, each in its own R
# session. Both greta_set_python() and greta_set_deps() store preferences that
# greta reads while loading, so setting either here would be too late: the run
# would exercise uv wherever uv works, on greta's default versions, and report
# the backend and version it was asked for rather than the ones it used.
run_cell <- function() {
  set.seed(2026)
  x <- rnorm(30, mean = 2, sd = 0.5)

  # greta initialises Python lazily, and this is what triggers it --
  # check_tf_version() runs when the first greta array is built, not at
  # model(), so a version greta refuses fails here.
  mu <- normal(0, 10)
  sigma <- normal(0, 3, truncation = c(0, Inf))
  distribution(x) <- normal(mu, sigma)
  m <- model(mu, sigma)

  python_path <- reticulate::py_config()$python

  # Fail rather than report a green for a backend that was never exercised.
  # This is the harness being wrong rather than the version being unsupported,
  # so it fails whatever `expected` says.
  using_conda <- grepl("greta-env-tf2", python_path, fixed = TRUE)
  if (identical(backend, "conda") && !using_conda) {
    cell$outcome <- "wrong backend"
    stop("asked for the conda backend, but python is ", python_path, call. = FALSE)
  }
  if (identical(backend, "uv") && using_conda) {
    cell$outcome <- "wrong backend"
    stop("asked for the uv backend, but python is ", python_path, call. = FALSE)
  }

  cell$python <- as.character(reticulate::py_config()$version)
  cell$tf <- as.character(tensorflow::tf$`__version__`)
  cell$tfp <- as.character(
    reticulate::import("tensorflow_probability")$`__version__`
  )
  cell$tf_keras <- as.character(reticulate::py_module_available("tf_keras"))

  cat("--- resolved stack ---\n")
  cat("backend   :", backend, "\n")
  cat("python at :", python_path, "\n")
  cat("python    :", cell$python, "\n")
  cat("tensorflow:", cell$tf, "\n")
  cat("tfp       :", cell$tfp, "\n")
  cat("tf_keras  :", cell$tf_keras, "\n")

  # Fail rather than report a green for a version that was never installed.
  # greta pins a minor series (tensorflow==2.16.*), so the patch version uv
  # picks is its choice to make and only the series is checked. Without this, a
  # preference that does not take effect looks identical to one that does.
  expected_tf <- if (identical(tf_version, "default")) {
    greta_deps_spec()$tf_version
  } else {
    tf_version
  }
  minor_series <- function(x) sub("\\.[^.]*$", "", x)
  if (!identical(minor_series(cell$tf), minor_series(expected_tf))) {
    cell$outcome <- "wrong version installed"
    stop(
      "asked for TensorFlow ",
      expected_tf,
      ", but resolved ",
      cell$tf,
      call. = FALSE
    )
  }

  cat("--- exercising greta ---\n")

  # Whether these run at all is the question; whether they fit well is the test
  # suite's, on a stack it can rely on. opt() covers the Keras optimiser path
  # and mcmc() the TFP sampler path, which between them are the two Python
  # surfaces a version bump breaks.
  fit <- opt(m, optimiser = adam(), max_iterations = 500)
  cat("opt mu:", fit$par$mu, "\n")

  draws <- mcmc(m, n_samples = 100, warmup = 100, chains = 2, verbose = FALSE)
  cat("mcmc draws:", nrow(as.matrix(draws)), "\n")

  cell$outcome <- "works"
  cell$detail <- ""
  cat("--- ok ---\n")
  invisible(TRUE)
}

failed <- FALSE
tryCatch(
  run_cell(),
  error = function(e) {
    failed <<- TRUE
    cell$detail <- first_line(conditionMessage(e))
    if (identical(cell$outcome, "did not load")) {
      cell$outcome <- "does not work"
    }
    cat("--- failed ---\n")
    cat(cell$detail, "\n")
  }
)

write_row()

# A harness fault is never excused by `expected`: it means this run tested
# something other than what it claimed to.
harness_fault <- cell$outcome %in% c("wrong backend", "wrong version installed")

if (failed) {
  if (harness_fault) {
    stop(cell$outcome, ": ", cell$detail, call. = FALSE)
  }
  if (identical(expected, "unsupported")) {
    cat("expected not to work; recorded in the summary, not failed\n")
    quit(status = 0)
  }
  stop("expected this combination to work, but it did not", call. = FALSE)
}

if (identical(expected, "unsupported")) {
  stop(
    "this combination is marked unsupported, but it worked; ",
    "greta_deps_default$tf_min may now be too high",
    call. = FALSE
  )
}
