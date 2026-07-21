# Changelog

## greta 0.6.0

CRAN release: 2026-07-20

### Changes

- Add warmup information to the MCMC print method
  ([\#652](https://github.com/greta-dev/greta/issues/652), resolved by
  [\#755](https://github.com/greta-dev/greta/issues/755)).
- Cap TensorFlow’s internal CPU threadpool inside vignette builds (via
  `TF_NUM_INTRAOP_THREADS` and `TF_NUM_INTEROP_THREADS`) so CRAN’s
  CPU/elapsed timing on vignette rebuilds stays under the two-core limit
  ([\#796](https://github.com/greta-dev/greta/issues/796)).
- greta now also caps TensorFlow’s CPU thread pools at 2 when running
  under `R CMD check` (detected via the `_R_CHECK_LIMIT_CORES_`
  environment variable), so checks on CRAN machines respect the two-core
  limit ([\#796](https://github.com/greta-dev/greta/issues/796)).
- Reshaping a greta array with `dim<-` now keeps the `?` placeholder
  display for unknown values instead of showing `NA`
  ([\#582](https://github.com/greta-dev/greta/issues/582)).
- Resolve issues with the TensorFlow version in DESCRIPTION: greta no
  longer requires `== 2.16.0` and now accepts `>= 2.16.0`.
- The `n_cores` argument now defaults to 2 cores and `chains` defaults
  to 2 chains; when the number of cores requested exceeds the number
  detected, the number detected is used.
- Use `.batch_size` instead of `batch_size` internally, to avoid rare
  name clash errors
  ([\#634](https://github.com/greta-dev/greta/issues/634)).
- [`as.unknowns()`](https://greta-dev.github.io/greta/reference/as.unknowns.md)
  now handles plain numeric vectors (and `dim<-` being set to `NULL`),
  fixing an
  [`mcmc()`](https://greta-dev.github.io/greta/reference/inference.md)
  error “no applicable method for ‘as.unknowns’” that surfaced when
  re-building vignettes under R-devel
  ([\#582](https://github.com/greta-dev/greta/issues/582)).
- [`greta_notes_tf_num_error()`](https://greta-dev.github.io/greta/reference/stash-notes.md)
  now emits its note with
  [`message()`](https://rdrr.io/r/base/message.html) rather than
  [`cat()`](https://rdrr.io/r/base/cat.html), so it can be silenced with
  [`suppressMessages()`](https://rdrr.io/r/base/message.html) and is
  written to the message stream.
- [`greta_sitrep()`](https://greta-dev.github.io/greta/reference/greta_sitrep.md)
  gains a `verbosity` argument with three levels of detail: “minimal”
  (default), “detailed”, and “quiet”
  ([\#612](https://github.com/greta-dev/greta/issues/612), resolved by
  [\#679](https://github.com/greta-dev/greta/issues/679)).
- `log.greta_array()` now warns when the `base` argument is supplied, as
  it was being ignored
  ([\#597](https://github.com/greta-dev/greta/issues/597)).
- [`outer()`](https://greta-dev.github.io/greta/reference/overloaded.md)
  (and `%o%`) now works with greta arrays when `FUN = "*"`, instead of
  silently returning a base array of `NA`s; base R’s `"*"` fast path
  used [`as.vector()`](https://rdrr.io/r/base/vector.html), which
  dropped the greta operation
  ([\#582](https://github.com/greta-dev/greta/issues/582)).

#### Installation and dependencies

- greta now resolves its Python environment more flexibly instead of
  always forcing reticulate’s managed (uv) environment: it respects a
  user-set `RETICULATE_PYTHON`, a stored greta preference, or an
  existing `greta-env-tf2` conda environment, and otherwise uses the
  managed (uv) environment
  ([\#801](https://github.com/greta-dev/greta/issues/801)).
- The managed (uv) backend now works without an internet connection, by
  enabling uv’s offline mode (via environment variable `UV_OFFLINE`)
  once reticulate’s uv cache is installed. You can set `UV_OFFLINE=0`
  (or `=1`) yourself to force online or offline resolution, respectively
  ([\#814](https://github.com/greta-dev/greta/issues/814)).
- Fixed [`library(greta)`](https://greta-dev.github.io/greta/) failing
  with an error when greta’s stored Python preference file exists but is
  empty ([\#809](https://github.com/greta-dev/greta/issues/809)).
- [`greta_deps_spec()`](https://greta-dev.github.io/greta/reference/greta_deps_spec.md)
  now only checks that the requested TensorFlow version is one greta
  supports (TensorFlow 2.16 and later are not supported, as they ship
  Keras 3); compatible TensorFlow Probability and Python versions are
  left to uv or conda to resolve rather than being validated against a
  fixed compatibility table
  ([\#675](https://github.com/greta-dev/greta/issues/675)).
- [`greta_list_py_modules()`](https://greta-dev.github.io/greta/reference/greta_list_py_modules.md)
  shows the Python packages installed in a specific TF2 environment
  ([\#801](https://github.com/greta-dev/greta/issues/801),
  [\#809](https://github.com/greta-dev/greta/issues/809)).
- [`greta_remove()`](https://greta-dev.github.io/greta/reference/greta_remove.md)
  consolidates greta’s Python removal helpers behind a single `what`
  argument to remove the `greta-env-tf2` conda environment, miniconda,
  reticulate’s uv cache, the stored Python preference, the stored
  dependency versions, or all of them. This means
  [`destroy_greta_deps()`](https://greta-dev.github.io/greta/reference/deprecated-installers.md),
  [`greta_remove_all_deps()`](https://greta-dev.github.io/greta/reference/deprecated-installers.md),
  [`remove_greta_env()`](https://greta-dev.github.io/greta/reference/deprecated-installers.md),
  [`remove_miniconda()`](https://greta-dev.github.io/greta/reference/deprecated-installers.md),
  and
  [`remove_reticulate_uv_cache()`](https://greta-dev.github.io/greta/reference/deprecated-installers.md)
  are now superseded by
  [`greta_remove()`](https://greta-dev.github.io/greta/reference/greta_remove.md)
  ([\#814](https://github.com/greta-dev/greta/issues/814)).
- [`greta_remove()`](https://greta-dev.github.io/greta/reference/greta_remove.md)
  no longer leaves a stale, deleted environment active for the rest of
  the session: it now invalidates greta’s cached Python backend and
  nudges you to restart R if you try to use greta again without
  restarting, instead of silently failing or falsely reporting the
  removed environment as still available.
- [`greta_set_deps()`](https://greta-dev.github.io/greta/reference/greta_set_deps.md)
  persistently chooses which TensorFlow, TensorFlow Probability, and
  Python versions greta uses; the managed (uv) environment installs them
  on next load and
  [`install_greta_deps()`](https://greta-dev.github.io/greta/reference/install_greta_deps.md)
  uses them as its default; clear with `greta_remove("deps")`
  ([\#817](https://github.com/greta-dev/greta/issues/817)).
- [`greta_set_python()`](https://greta-dev.github.io/greta/reference/greta_set_python.md)
  and
  [`greta_reset_python()`](https://greta-dev.github.io/greta/reference/greta_set_python.md)
  let you choose, persistently, which Python environment greta uses -
  the managed (uv) environment (`backend = "uv"`, the default), a conda
  environment (`backend = "conda"`), or a specific Python
  (`backend = "path"`); each reports the stored preference, warns if
  `RETICULATE_PYTHON` takes precedence, and shows what greta will
  resolve to on its next load
  ([\#801](https://github.com/greta-dev/greta/issues/801),
  [\#809](https://github.com/greta-dev/greta/issues/809),
  [\#817](https://github.com/greta-dev/greta/issues/817)).
- `greta_set_python("path", path = ...)` accepts either a Python binary
  or an environment directory (a virtualenv or conda prefix), looking
  for `bin/python` (or `Scripts/python.exe` on Windows) inside it, which
  eases offline and pre-installed setups
  ([\#814](https://github.com/greta-dev/greta/issues/814)).
- [`greta_sitrep()`](https://greta-dev.github.io/greta/reference/greta_sitrep.md)
  now reports the resolved Python backend and whether greta can start
  offline: for the managed (uv) environment, the `UV_OFFLINE` setting
  and whether reticulate’s uv cache is populated; a missing conda
  environment is reported neutrally as “not used” on the managed (uv)
  backend rather than as a failure
  ([\#801](https://github.com/greta-dev/greta/issues/801),
  [\#817](https://github.com/greta-dev/greta/issues/817)).
- [`greta_sitrep()`](https://greta-dev.github.io/greta/reference/greta_sitrep.md)
  now requires Python 3.9 or later (previously 3.8), matching the Python
  versions greta supports
  ([\#809](https://github.com/greta-dev/greta/issues/809)).
- [`install_greta_deps()`](https://greta-dev.github.io/greta/reference/install_greta_deps.md)
  now records the location of the `greta-env-tf2` conda environment at
  install time, so greta auto-detects it in any conda installation, not
  just reticulate’s miniconda
  ([\#809](https://github.com/greta-dev/greta/issues/809)).
- [`install_greta_deps()`](https://greta-dev.github.io/greta/reference/install_greta_deps.md)
  is no longer required for most users, as greta now installs TensorFlow
  and TensorFlow Probability automatically via uv on first use; it
  remains for installing a conda environment (for example, for offline
  use), which you can then select with `greta_set_python("conda")`
  ([\#801](https://github.com/greta-dev/greta/issues/801)).
- [`install_greta_deps()`](https://greta-dev.github.io/greta/reference/install_greta_deps.md)
  now restores the `warning.length` option when it exits, instead of
  leaving its own value in place for the rest of the session.
- [`reinstall_greta_env()`](https://greta-dev.github.io/greta/reference/deprecated-installers.md)
  and
  [`reinstall_miniconda()`](https://greta-dev.github.io/greta/reference/deprecated-installers.md)
  are now deprecated in favour of
  [`reinstall_greta_deps()`](https://greta-dev.github.io/greta/reference/install_greta_deps.md)
  ([\#814](https://github.com/greta-dev/greta/issues/814)).
- [`remove_greta_env()`](https://greta-dev.github.io/greta/reference/deprecated-installers.md),
  [`remove_miniconda()`](https://greta-dev.github.io/greta/reference/deprecated-installers.md),
  and
  [`remove_reticulate_uv_cache()`](https://greta-dev.github.io/greta/reference/deprecated-installers.md)
  ask for confirmation before removing, gain an `ask` argument (default
  [`interactive()`](https://rdrr.io/r/base/interactive.html)) so they
  work non-interactively, and invisibly return whether anything was
  removed ([\#809](https://github.com/greta-dev/greta/issues/809)).
- [`remove_reticulate_uv_cache()`](https://greta-dev.github.io/greta/reference/deprecated-installers.md)
  removes reticulate’s uv cache; note this cache is shared by all
  reticulate packages and is not greta-specific, and a system-wide uv
  cache is left untouched
  ([\#801](https://github.com/greta-dev/greta/issues/801),
  [\#809](https://github.com/greta-dev/greta/issues/809)).

## greta 0.5.0

CRAN release: 2024-11-12

This version of greta uses Tensorflow 2.0.0, which comes with it a host
of new very exciting features!

### Optimizers

The latest interface to optimizers in tensorflow are now used, these
changes are described.

- `gradient_descent` gains `momentum` and `nesterov` arguments, as
  described here in [TF
  docs](https://www.tensorflow.org/api_docs/python/tf/keras/optimizers/SGD)
- `adagrad` gains [`epsilon`
  argument](https://www.tensorflow.org/api_docs/python/tf/keras/optimizers/Adagrad)
- removes `momentum` optimizer, as this has been folded into
  `gradient_descent` arguments
- Adds `amsgrad` argument to `adam` optimizer, as described in [TF
  docs](https://www.tensorflow.org/api_docs/python/tf/keras/optimizers/Adam)
- Adds `adamax` optimiser, see [TF
  docs](https://www.tensorflow.org/api_docs/python/tf/keras/optimizers/Adamax)
- Adds `l2_shrinkage_regularization_strength` and `beta` arguments to
  `ftrl` optimiser.
- adds `nadam` optimiser - see
  [docs](https://www.tensorflow.org/api_docs/python/tf/keras/optimizers/Nadam).
- In `rms_prop` optimiser, changes `decay` parameter to `rho`, and adds
  `centered` parameter - see
  [docs](https://www.tensorflow.org/api_docs/python/tf/keras/optimizers/RMSprop)

The following optimisers are removed, as they are no longer supported by
Tensorflow:

- [`powell()`](https://greta-dev.github.io/greta/reference/optimisers.md)
- [`cg()`](https://greta-dev.github.io/greta/reference/optimisers.md)
- [`newton_cg()`](https://greta-dev.github.io/greta/reference/optimisers.md)
- [`l_bfgs_b()`](https://greta-dev.github.io/greta/reference/optimisers.md)
- [`tnc()`](https://greta-dev.github.io/greta/reference/optimisers.md)
- [`cobyla()`](https://greta-dev.github.io/greta/reference/optimisers.md)
- [`slsqp()`](https://greta-dev.github.io/greta/reference/optimisers.md)

### Installation revamp

This release provides a few improvements to installation in greta. It
should now provide more information about installation progress, and be
more robust. The intention is, it should *just work*, and if it doesn’t,
it should fail gracefully with some useful advice on problem solving.

- Added option to restart R + run
  [`library(greta)`](https://greta-dev.github.io/greta/) after
  installation ([\#523](https://github.com/greta-dev/greta/issues/523)).
- Added installation deps object,
  [`greta_deps_spec()`](https://greta-dev.github.io/greta/reference/greta_deps_spec.md)
  to help simplify specifying package versions
  ([\#664](https://github.com/greta-dev/greta/issues/664)).
- Removed `method` and `conda` arguments from
  [`install_greta_deps()`](https://greta-dev.github.io/greta/reference/install_greta_deps.md)
  as they were not used.
- Removed `manual` argument in
  [`install_greta_deps()`](https://greta-dev.github.io/greta/reference/install_greta_deps.md).
- Added default 5 minute timer to installation processes.
- Added checking suite to ensure you are using valid versions of TF,
  TFP, and
  Python([\#666](https://github.com/greta-dev/greta/issues/666)).
- Added data `greta_deps_tf_tfp`
  ([\#666](https://github.com/greta-dev/greta/issues/666)), which
  contains valid versions combinations of TF, TFP, and Python.
- Remove `greta_nodes_install/conda_*()` options as
  [\#493](https://github.com/greta-dev/greta/issues/493) makes them
  defunct.
- Added option to write to a single logfile with
  [`greta_set_install_logfile()`](https://greta-dev.github.io/greta/reference/greta_set_install_logfile.md),
  and
  [`write_greta_install_log()`](https://greta-dev.github.io/greta/reference/write_greta_install_log.md),
  and
  [`open_greta_install_log()`](https://greta-dev.github.io/greta/reference/open_greta_install_log.md)
  ([\#493](https://github.com/greta-dev/greta/issues/493)).
- Added
  [`destroy_greta_deps()`](https://greta-dev.github.io/greta/reference/deprecated-installers.md)
  function to remove miniconda and python conda environment.
- Improved
  [`write_greta_install_log()`](https://greta-dev.github.io/greta/reference/write_greta_install_log.md)
  and
  [`open_greta_install_log()`](https://greta-dev.github.io/greta/reference/open_greta_install_log.md)
  to use [`tools::R_user_dir()`](https://rdrr.io/r/tools/userdir.html)
  to always write to a file location.
  [`open_greta_install_log()`](https://greta-dev.github.io/greta/reference/open_greta_install_log.md)
  will open one found from an environment variable or go to the default
  location ([\#703](https://github.com/greta-dev/greta/issues/703)).
- [`greta_deps_receipt()`](https://greta-dev.github.io/greta/reference/greta_deps_receipt.md)
  records the TensorFlow, TensorFlow Probability, and Python versions
  that are actually installed without validating them against greta’s
  supported range, so it works even when the installed versions are
  newer than greta officially supports (such as a TensorFlow 2.15 patch
  release) ([\#668](https://github.com/greta-dev/greta/issues/668)).

### New Print methods

- New print method for `greta_mcmc_list`. This means MCMC output will be
  shorter and more informative
  ([\#644](https://github.com/greta-dev/greta/issues/644)).
- greta arrays now have a print method that stops them from printing too
  many rows into the console. Similar to MCMC print method, you can
  control the print output with the `n` argument:
  `print(object, n = <elements to print>)`
  ([\#644](https://github.com/greta-dev/greta/issues/644)).

### Minor

- [`greta_sitrep()`](https://greta-dev.github.io/greta/reference/greta_sitrep.md)
  now checks for installations of Python, TF, and TFP.
- Slice sampler no longer needs precision = “single” to work.
- greta now depends on R 4.1.0, which was released May 2021, over 3
  years ago.
- export
  [`is.greta_array()`](https://greta-dev.github.io/greta/reference/is.greta_array.md)
  and
  [`is.greta_mcmc_list()`](https://greta-dev.github.io/greta/reference/is.greta_mcmc_list.md).
- `restart` argument for
  [`install_greta_deps()`](https://greta-dev.github.io/greta/reference/install_greta_deps.md)
  and
  [`reinstall_greta_deps()`](https://greta-dev.github.io/greta/reference/install_greta_deps.md)
  to automatically restart R
  ([\#523](https://github.com/greta-dev/greta/issues/523)).

### Internals

- Internally we are replacing most of the error handling code as
  separate `check_*` functions.

- Implemented `cli::cli_abort/warn/inform()` in place of
  `cli::format_error/warning/message()` +
  `stop/warning/message(msg, call. = FALSE)` pattern.

- Uses legacy optimizer internally (Use
  `tf$keras$optimizers$legacy$METHOD` over
  `tf$keras$optimizers$METHOD`). No user impact expected.

- Update photo of Grete Hermann
  ([\#598](https://github.com/greta-dev/greta/issues/598)).

- Use `%||%` internally to replace the pattern:
  `if (is.null(x)) x <- thing` with `x <- x %||% thing`
  ([\#630](https://github.com/greta-dev/greta/issues/630)).

- Add more explaining variables - replace
  `if (thing & thing & what == this)` with `if (explanation_of_thing)`.

- Refactored repeated uses of `vapply` into functions
  ([\#377](https://github.com/greta-dev/greta/issues/377),
  [\#658](https://github.com/greta-dev/greta/issues/658)).

- Add internal data files `.deps_tf` and `.deps_tfp` to track
  dependencies of TF and TFP. Related to
  [\#666](https://github.com/greta-dev/greta/issues/666).

- Posterior density checks
  ([\#720](https://github.com/greta-dev/greta/issues/720)):

  - Don’t run Geweke on CI as it takes 30 minutes to run.
  - Add thinning to Geweke tests.
  - Fix broken geweke tests from TF1–\>TF2 change.
  - Increase the number of effective samples for check_samples for lkj
    distribution
  - Add more checks to posterior to run on CI/on each test of greta

### Bug fixes

- Fix bug where matrix multiply had dimension error before coercing to
  greta array. ([\#464](https://github.com/greta-dev/greta/issues/464))
- Fixes for Wishart and LKJ Correlation distributions
  ([\#729](https://github.com/greta-dev/greta/issues/729)
  [\#733](https://github.com/greta-dev/greta/issues/733)
  [\#734](https://github.com/greta-dev/greta/issues/734)):
  - Add bijection density to choleskied distributions.
  - Note about some issues with LKJ and our normalisation constant for
    the density.
  - Removed our custom `forward_log_det_jacobian()` function from
    `tf_correlation_cholesky_bijector()` (used in
    [`lkj_correlation()`](https://greta-dev.github.io/greta/reference/distributions.md)).
    Previously, it did not work with unknown dimensions, but it now
    works with them.
  - Ensure wishart uses sigma_chol in scale_tril
  - Wishart uses `tf$matmul(chol_draws, chol_draws, adjoint_b = TRUE)`
    instead of `tf_chol2symm(chol_draws)`.
  - Test log prob function returns valid numeric numbers.
  - Addresses issue with log prob returning NaNs–replace
    `FillTriangular` with `FillScaleTriL` and apply Chaining to first
    transpose input.

## greta 0.4.5

CRAN release: 2024-03-11

### Bug Fixes

- Remove trailing comma bug in glue
  [\#618](https://github.com/greta-dev/greta/issues/618)

## greta 0.4.4

CRAN release: 2024-02-02

### Bug fixes

- Some small documentation bugs were fixed, namely the sentinel
  “\_PACKAGE” documentation, and various small changes to correctly
  export S3 methods.

## greta 0.4.3

CRAN release: 2022-09-08

### Features

- Adds
  [`reinstall_greta_deps()`](https://greta-dev.github.io/greta/reference/install_greta_deps.md),
  which helps with starting from a clean slate when installing greta
  dependencies ([\#524](https://github.com/greta-dev/greta/issues/524))

### Fixes

- Issue where `future` and `parallely` packages error when a CPU with
  only one core is provided
  ([\#513](https://github.com/greta-dev/greta/issues/513),
  [\#516](https://github.com/greta-dev/greta/issues/516)).
- Removes any use of `multiprocess` as it is deprecated in the `future`
  package ([\#394](https://github.com/greta-dev/greta/issues/394))

## greta 0.4.2

CRAN release: 2022-03-22

### Fixes

- workaround for M1 issues
  ([\#507](https://github.com/greta-dev/greta/issues/507))

## greta 0.4.1 (2022-03-14)

CRAN release: 2022-03-15

### Fixes:

- Python is now initialised when a `greta_array` is created
  ([\#468](https://github.com/greta-dev/greta/issues/468)).

- head and tail S3 methods for `greta_array` are now consistent with
  head and tail methods for R versions 3 and 4
  ([\#384](https://github.com/greta-dev/greta/issues/384)).

- `greta_mcmc_list` objects (returned by
  [`mcmc()`](https://greta-dev.github.io/greta/reference/inference.md))
  are now no longer modified by operations (like
  [`coda::gelman.diag()`](https://rdrr.io/pkg/coda/man/gelman.diag.html)).

- joint distributions of uniform variables now have the correct
  constraints when sampling
  ([\#377](https://github.com/greta-dev/greta/issues/377)).

- array-scalar dispatch with 3D arrays is now less buggy
  ([\#298](https://github.com/greta-dev/greta/issues/298)).

- `greta` now provides R versions of all of R’s primitive functions (I
  think), to prevent them from silently not executing
  ([\#317](https://github.com/greta-dev/greta/issues/317)).

- Uses `Sys.unsetenv("RETICULATE_PYTHON")` in `.onload` on package
  startup, to prevent an issue introduced with the “ghost orchid”
  version of RStudio where they do not find the current version of
  RStudio. See [\#444](https://github.com/greta-dev/greta/issues/444)
  for more details.

- Internal change to code to ensure `future` continues to support
  parallelisation of chains. See
  [\#447](https://github.com/greta-dev/greta/issues/447) for more
  details.

- `greta` now depends on `future` version 1.22.1, `tensorflow` (the R
  package) 2.7.0, and `parallelly` 1.29.0. This should see no changes on
  the user side.

### API changes:

- Now depends on R \>= 3.1.0
  ([\#386](https://github.com/greta-dev/greta/issues/386))

- `chol2inv.greta_array()` now warns user about LINPACK argument being
  ignored, and also reminds user it has been deprecated since R 3.1

- [`calculate()`](https://greta-dev.github.io/greta/reference/calculate.md)
  now accepts multiple greta arrays for which to calculate values, via
  the `...` argument. As a consequence any other arguments must now be
  named.

- A number of optimiser methods are now deprecated, since they will be
  unavailable when greta moves to using TensorFlow v2.0:
  [`powell()`](https://greta-dev.github.io/greta/reference/optimisers.md),
  [`cg()`](https://greta-dev.github.io/greta/reference/optimisers.md),
  [`newton_cg()`](https://greta-dev.github.io/greta/reference/optimisers.md),
  [`l_bfgs_b()`](https://greta-dev.github.io/greta/reference/optimisers.md),
  [`tnc()`](https://greta-dev.github.io/greta/reference/optimisers.md),
  [`cobyla()`](https://greta-dev.github.io/greta/reference/optimisers.md),
  and
  [`slsqp()`](https://greta-dev.github.io/greta/reference/optimisers.md).

- [`dirichlet()`](https://greta-dev.github.io/greta/reference/distributions.md)
  now returns a variable (rather than an operation) greta array, and the
  graphs created by
  [`lkj_correlation()`](https://greta-dev.github.io/greta/reference/distributions.md)
  and
  [`wishart()`](https://greta-dev.github.io/greta/reference/distributions.md)
  are now simpler as cholesky-shaped variables are now available
  internally.

- Adds the
  [`reinstall_greta_env()`](https://greta-dev.github.io/greta/reference/deprecated-installers.md),
  [`reinstall_miniconda()`](https://greta-dev.github.io/greta/reference/deprecated-installers.md),
  [`remove_greta_env()`](https://greta-dev.github.io/greta/reference/deprecated-installers.md),
  and
  [`remove_miniconda()`](https://greta-dev.github.io/greta/reference/deprecated-installers.md)
  helper functions for helping installation get to “clean slate”
  ([\#443](https://github.com/greta-dev/greta/issues/443)).

- `greta` currently doesn’t work on Apple Silicon (M1 Macs) as they need
  to use TF 2.0, which is currently being implemented. `greta` now
  throws an error if M1 macs are detected and directs users to
  <https://github.com/greta-dev/greta/issues/458>
  ([\#487](https://github.com/greta-dev/greta/issues/487))

### Features:

- New
  [`install_greta_deps()`](https://greta-dev.github.io/greta/reference/install_greta_deps.md) -
  provides installation of python dependencies
  ([\#417](https://github.com/greta-dev/greta/issues/417)). This saves
  exact versions of Python (3.7), and the python modules NumPy (1.16.4),
  Tensorflow (1.14.0), and Tensorflow Probability (0.7.0) into a conda
  environment, “greta-env”. When initialising Python, greta now searches
  for this conda environment first, which presents a great advantage as
  it isolates these exact versions of these modules from other Python
  installations. It is not required to use the conda environment,
  “greta-env”. Overall this means that users can run the function
  [`install_greta_deps()`](https://greta-dev.github.io/greta/reference/install_greta_deps.md),
  follow the prompts, and have all the python modules they need
  installed, without contaminating other software that use different
  python modules.

- [`calculate()`](https://greta-dev.github.io/greta/reference/calculate.md)
  now enables simulation of greta array values from their priors,
  optionally conditioned on fixed values or posterior samples. This
  enables prior and posterior predictive checking of models, and
  simulation of data.

- A [`simulate()`](https://rdrr.io/r/stats/simulate.html) method for
  greta models is now also provided, to simulate the values of all greta
  arrays in a model from their priors.

- [`variable()`](https://greta-dev.github.io/greta/reference/variable.md)
  now accepts arrays for `upper` and `lower`, enabling users to define
  variables with different constraints.

- There are three new variable constructor functions:
  [`cholesky_variable()`](https://greta-dev.github.io/greta/reference/variable.md),
  [`simplex_variable()`](https://greta-dev.github.io/greta/reference/variable.md),
  and
  [`ordered_variable()`](https://greta-dev.github.io/greta/reference/variable.md),
  for variables with these constraints but no probability distribution.

- New
  [`chol2symm()`](https://greta-dev.github.io/greta/reference/chol2symm.md)
  is the inverse of [`chol()`](https://rdrr.io/r/base/chol.html).

- [`mcmc()`](https://greta-dev.github.io/greta/reference/inference.md),
  [`stashed_samples()`](https://greta-dev.github.io/greta/reference/inference.md),
  and
  [`calculate()`](https://greta-dev.github.io/greta/reference/calculate.md)
  now return objects of class `greta_mcmc_list` which inherit from
  `coda`’s `mcmc.list` class, but enable custom greta methods for
  manipulating mcmc outputs, including a
  [`window()`](https://rdrr.io/r/stats/window.html) function.

- [`mcmc()`](https://greta-dev.github.io/greta/reference/inference.md)
  and
  [`calculate()`](https://greta-dev.github.io/greta/reference/calculate.md)
  now have a `trace_batch_size` argument enabling users to trade-off
  computation speed versus memory requirements when calculating
  posterior samples for target greta arrays
  ([\#236](https://github.com/greta-dev/greta/issues/236)).

- Many message, warning, and error prompts have been replaced internally
  with the {cli} R package for nicer printing. This is a minor change
  that should result in a more pleasant user experience
  ([\#423](https://github.com/greta-dev/greta/issues/423)
  [\#425](https://github.com/greta-dev/greta/issues/425)).

- Internally, where sensible, `greta` now uses the `glue` package to
  create messages/ouputs
  ([\#378](https://github.com/greta-dev/greta/issues/378)).

- New FAQ page and updated installation instructions for installing
  Python dependencies
  ([\#424](https://github.com/greta-dev/greta/issues/424))

- New
  [`greta_sitrep()`](https://greta-dev.github.io/greta/reference/greta_sitrep.md)
  function to generate a situation report of the software that is
  available for use, and also initialising python so `greta` is ready to
  use. ([\#441](https://github.com/greta-dev/greta/issues/441))

## greta 0.3.1

CRAN release: 2019-08-09

This release is predominantly a patch to make greta work with recent
versions of TensorFlow and TensorFlow Probability, which were not
backward compatible with the versions on which greta previously
depended. From this release forward, greta will depend on specific
(rather than minimum) versions of these two pieces of software to avoid
it breaking if more changes are made to the APIS of these packages.

- greta now (only) works with TensorFlow 1.14.0 and TensorFlow
  Probability 0.7.0
  ([\#289](https://github.com/greta-dev/greta/issues/289),
  [\#290](https://github.com/greta-dev/greta/issues/290))

- behaviour of the `pb_update` argument to
  [`mcmc()`](https://greta-dev.github.io/greta/reference/inference.md)
  has been changed slightly to avoid a bad interaction with thinning
  ([\#284](https://github.com/greta-dev/greta/issues/284))

- various edits to the documentation to fix spelling mistakes and typos

## greta 0.3.0

CRAN release: 2018-10-30

This is a very large update which adds a number of features and major
speed improvements. We now depend on the TensorFlow Probability Python
package, and use functionality in that package wherever possible.
Sampling a simple model now takes ~10s, rather than ~2m (\>10x speedup).

### Fixes:

#### operation bugs

- `dim<-()` now always rearranges elements in column-major order
  (R-style, not Python-style)

#### performance bugs

- removed excessive checking of TF installation by operation greta
  arrays (was slowing down greta array creation for complex models)
- sped up detection of sub-DAGs in model creation (was slowing down
  model definition for complex models)
- reduced passing between R, Python, and TensorFlow during sampling (was
  slowing down sampling)

### New Functionality:

#### inference methods

- 18 new optimisers have been added
- initial values can now be passed for some or all parameters
- 2 new MCMC samplers have been added: random-walk Metropolis-Hastings
  (thanks to [@michaelquinn32](https://github.com/michaelquinn32)) and
  slice sampling
- improved tuning of MCMC during warmup (thanks to
  [@martiningram](https://github.com/martiningram))
- integration with the `future` package for execution of MCMC chains on
  remote machines. Note: it is not advised to use `future` for parallel
  execution of chains on the same machine, that is now automatically
  handled by greta.
- the `one_by_one` argument to MCMC can handle serious numerical errors
  (such as failed matrix inversions) as ‘bad’ samples
- new
  [`extra_samples()`](https://greta-dev.github.io/greta/reference/inference.md)
  function to continue sampling from a model.
- [`calculate()`](https://greta-dev.github.io/greta/reference/calculate.md)
  works on the output of MCMC, to enable post-hoc posterior prediction

#### distributions

- multivariate distributions now accept matrices of parameter values
- added
  [`mixture()`](https://greta-dev.github.io/greta/reference/mixture.md)
  and [`joint()`](https://greta-dev.github.io/greta/reference/joint.md)
  distribution constructors

#### operations

- added functions:
  [`abind()`](https://greta-dev.github.io/greta/reference/overloaded.md),
  [`aperm()`](https://rdrr.io/r/base/aperm.html),
  [`apply()`](https://greta-dev.github.io/greta/reference/overloaded.md),
  [`chol2inv()`](https://greta-dev.github.io/greta/reference/overloaded.md),
  [`cov2cor()`](https://greta-dev.github.io/greta/reference/overloaded.md),
  [`eigen()`](https://greta-dev.github.io/greta/reference/overloaded.md),
  [`identity()`](https://greta-dev.github.io/greta/reference/overloaded.md),
  [`kronecker()`](https://rdrr.io/r/base/kronecker.html),
  [`rdist()`](https://greta-dev.github.io/greta/reference/overloaded.md),
  and
  [`tapply()`](https://greta-dev.github.io/greta/reference/overloaded.md)
  (thanks to [@jdyen](https://github.com/jdyen))
- we now automatically skip operations if possible, e.g. computing
  binomial and poisson densities with log-, logit- or probit-transformed
  parameters where they exist, or skipping cholesky decomposition of a
  matrix if it was created from its cholesky factor. This increases
  numerical stability as well as speed.

#### misc

- ability to change the colour of the model plot (thanks to
  [@dirmeier](https://github.com/dirmeier))
- ability to reshape greta arrays using
  [`greta_array()`](https://greta-dev.github.io/greta/reference/structures.md)

### API changes:

#### inference methods

- mcmc now runs 4 chains (simultaneously on all available cores), 1000
  warmup steps, and 1000 samples by default
- optimisation and mcmc methods are now passed to
  [`opt()`](https://greta-dev.github.io/greta/reference/inference.md)
  and
  [`mcmc()`](https://greta-dev.github.io/greta/reference/inference.md)
  as objects, with defined tuning parameters. The `control` argument to
  these functions is now defunct.
- columns names for parameters now give the array indices for each
  scalar rather than a number (i.e. `x[2, 3]`, rather than `x.6`)

#### distributions

- multivariate distributions now define each realisation as a row, and
  parameters must therefore have the same orientation

#### misc

- [`plot.greta_model()`](https://greta-dev.github.io/greta/reference/model.md)
  now returns a
  [`DiagrammeR::grViz`](https://rich-iannone.github.io/DiagrammeR/reference/grViz.html)
  object (thanks to [@flyaflya](https://github.com/flyaflya)). This is
  less modifiable, but renders the plot more much consistently across
  different environments and notebook types. The `DiagrammeR`
  `dgr_graph` object use to create the `grViz` object is included as an
  attribute of this object, named `"dgr_graph"`.

### documentation

- lots more model examples (thanks to
  [@leehazel](https://github.com/leehazel),
  [@dirmeier](https://github.com/dirmeier),
  [@jdyen](https://github.com/jdyen))
- two analysis case studies (thanks to
  [@ShirinG](https://github.com/ShirinG), Tiphaine Martin,
  [@mmulvahill](https://github.com/mmulvahill),
  [@michaelquinn32](https://github.com/michaelquinn32),
  [@revodavid](https://github.com/revodavid))
- new and improved pkgdown website (thanks to
  [@pteetor](https://github.com/pteetor))

### testing

- added tests of the validity of posterior samples drawn by MCMC (for
  known distributions and with Geweke tests)

## greta 0.2.5

Minor patch to handle an API change in the progress package. No changes
in functionality.

## greta 0.2.4

### Fixes:

- improved error checking/messages in
  [`model()`](https://greta-dev.github.io/greta/reference/model.md),
  `%*%`
- switched docs and examples to always use `<-` for assignment
- fixed the `n_cores` argument to
  [`model()`](https://greta-dev.github.io/greta/reference/model.md)

### New functionality:

- added a
  [`calculate()`](https://greta-dev.github.io/greta/reference/calculate.md)
  function to compute the values of greta arrays conditional on provided
  values for others
- added
  [`imultilogit()`](https://greta-dev.github.io/greta/reference/transforms.md)
  transform
- added a `chains` argument to
  [`model()`](https://greta-dev.github.io/greta/reference/model.md)
- improved HMC self-tuning, including a diagonal euclidean metric

## greta 0.2.3

CRAN release: 2018-01-23

### Fixes:

- fixed breaking change in extraDistr API (caused test errors on CRAN
  builds)
- added dontrun statements to pass CRAN checks on winbuilder
- fixed breaking change in tensorflow API (1-based indexing)

### New functionality:

- added [`cumsum()`](https://rdrr.io/r/base/cumsum.html) and
  [`cumprod()`](https://rdrr.io/r/base/cumsum.html) functions

## greta 0.2.2

### New functionality:

- added
  [`forwardsolve()`](https://greta-dev.github.io/greta/reference/overloaded.md)
  and
  [`backsolve()`](https://greta-dev.github.io/greta/reference/overloaded.md)
- added
  [`colSums()`](https://greta-dev.github.io/greta/reference/overloaded.md),
  [`rowSums()`](https://greta-dev.github.io/greta/reference/overloaded.md),
  [`colMeans()`](https://greta-dev.github.io/greta/reference/overloaded.md),
  and
  [`rowMeans()`](https://greta-dev.github.io/greta/reference/overloaded.md)
- added `dim<-()` to reshape greta arrays
- [`sweep()`](https://greta-dev.github.io/greta/reference/overloaded.md)
  now handles greta array `STATS` when `x` is numeric

## greta 0.2.1

### New functionality:

- export internal functions via `.internals` object to enable extension
  packages

API changes:

- removed the deprecated `define_model()`, an alias for
  [`model()`](https://greta-dev.github.io/greta/reference/model.md)
- removed the dynamics module, to be replaced by the gretaDynamics
  package
