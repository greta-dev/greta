# greta (development version)

# greta 0.5.0

This version of greta uses Tensorflow 2.0.0, which comes with it a host of new very exciting features!

## Optimizers

The latest interface to optimizers in tensorflow are now used, these changes are described.

* `gradient_descent` gains `momentum` and `nesterov` arguments, as described here in [TF docs](https://www.tensorflow.org/api_docs/python/tf/keras/optimizers/SGD)
* `adagrad` gains [`epsilon` argument](https://www.tensorflow.org/api_docs/python/tf/keras/optimizers/Adagrad)
* removes `momentum` optimizer, as this has been folded into `gradient_descent` arguments
* Adds `amsgrad` argument to `adam` optimizer, as described in [TF docs](https://www.tensorflow.org/api_docs/python/tf/keras/optimizers/Adam)
* Adds `adamax` optimiser, see [TF docs](https://www.tensorflow.org/api_docs/python/tf/keras/optimizers/Adamax)
* Adds `l2_shrinkage_regularization_strength` and `beta` arguments to `ftrl`
  optimiser.
* adds `nadam` optimiser - see [docs](https://www.tensorflow.org/api_docs/python/tf/keras/optimizers/Nadam).
* In `rms_prop` optimiser, changes `decay` parameter to `rho`, and adds `centered` parameter - see [docs](https://www.tensorflow.org/api_docs/python/tf/keras/optimizers/RMSprop)

The following optimisers are removed, as they are no longer supported by Tensorflow:

 * `powell()`
 * `cg()`
 * `newton_cg()`
 * `l_bfgs_b()`
 * `tnc()`
 * `cobyla()`
 * `slsqp()`

## Installation revamp

This release provides a few improvements to installation in greta. It should now provide more information about installation progress, and be more robust. The intention is, it should _just work_, and if it doesn't, it should fail gracefully with some useful advice on problem solving.

* Added option to restart R + run `library(greta)` after installation (#523).
* Added installation deps object, `greta_deps_sepc()` to help simplify specifying package versions (#664).
* Removed `method` and `conda` arguments from `install_greta_deps()` as they 
  were not used.
* Removed `manual` argument in `install_greta_deps()`.
* Added default 5 minute timer to installation processes.
* Added `greta_deps_receipt()` to list the current main python packages installed (#668).
* Added checking suite to ensure you are using valid versions of TF, TFP, and Python(#666).
* Added data `greta_deps_tf_tfp` (#666), which contains valid versions combinations of TF, TFP, and Python.
* Remove `greta_nodes_install/conda_*()` options as #493 makes them defunct.
* Added option to write to a single logfile with `greta_set_install_logfile()`, and `write_greta_install_log()`, and `open_greta_install_log()` (#493).
* Added `destroy_greta_deps()` function to remove miniconda and python conda environment.
* Improved `write_greta_install_log()` and `open_greta_install_log()` to use `tools::R_user_dir()` to always write to a file location. `open_greta_install_log()` will open one found from an environment variable or go to the default location (#703).

## New Print methods

* New print method for `greta_mcmc_list`. This means MCMC output will be shorter and more informative (#644).
* greta arrays now have a print method that stops them from printing too many rows into the console. Similar to MCMC print method, you can control the print output with the `n` argument: `print(object, n = <elements to print>)` (#644).

## Minor

* `greta_sitrep()` now checks for installations of Python, TF, and TFP.
* Slice sampler no longer needs precision = "single" to work.
* greta now depends on R 4.1.0, which was released May 2021, over 3 years ago.
* export `is.greta_array()` and `is.greta_mcmc_list()`.
* `restart` argument for `install_greta_deps()` and `reinstall_greta_deps()` to automatically restart R (#523).

## Internals

* Internally we are replacing most of the error handling code as separate 
  `check_*` functions.
* Implemented `cli::cli_abort/warn/inform()` in place of `cli::format_error/warning/message()` + `stop/warning/message(msg, call. = FALSE)` pattern.
* Uses legacy optimizer internally (Use `tf$keras$optimizers$legacy$METHOD` over `tf$keras$optimizers$METHOD`). No user impact expected.
* Update photo of Grete Hermann (#598).
* Use `%||%` internally to replace the pattern: `if (is.null(x)) x <- thing` with `x <- x %||% thing` (#630).
* Add more explaining variables - replace `if (thing & thing & what == this)` with `if (explanation_of_thing)`.
* Refactored repeated uses of `vapply` into functions (#377, #658).
* Add internal data files `.deps_tf` and `.deps_tfp` to track dependencies of TF and TFP. Related to #666.

- Posterior density checks (#720):
  - Don't run Geweke on CI as it takes 30 minutes to run.
  - Add thinning to Geweke tests.
  - Fix broken geweke tests from TF1-->TF2 change.
  - Increase the number of effective samples for check_samples for lkj distribution
  - Add more checks to posterior to run on CI/on each test of greta

## Bug fixes

* Fix bug where matrix multiply had dimension error before coercing to greta array. (#464)
- Fixes for Wishart and LKJ Correlation distributions (#729 #733 #734):
  - Add bijection density to choleskied distributions.
  - Note about some issues with LKJ and our normalisation constant for the density.
  - Removed our custom `forward_log_det_jacobian()` function from `tf_correlation_cholesky_bijector()` (used in `lkj_correlation()`). Previously, it did not work with unknown dimensions, but it now works with them.
  - Ensure wishart uses sigma_chol in scale_tril
  - Wishart uses `tf$matmul(chol_draws, chol_draws, adjoint_b = TRUE)` instead of `tf_chol2symm(chol_draws)`.
  - Test log prob function returns valid numeric numbers.
  - Addresses issue with log prob returning NaNs--replace `FillTriangular` with `FillScaleTriL` and apply Chaining to first transpose input.

# greta 0.4.5

## Bug Fixes

- Remove trailing comma bug in glue #618

# greta 0.4.4

## Bug fixes

- Some small documentation bugs were fixed, namely the sentinel "_PACKAGE" documentation, and various small changes to correctly export S3 methods.

# greta 0.4.3

## Features

- Adds `reinstall_greta_deps()`, which helps with starting from a clean slate when installing greta dependencies (#524)

## Fixes

* Issue where `future` and `parallely` packages error when a CPU with only one core is provided (#513, #516).
* Removes any use of `multiprocess` as it is deprecated in the `future` package (#394)

# greta 0.4.2

## Fixes

* workaround for M1 issues (#507)

# greta 0.4.1 (2022-03-14)

## Fixes:

* Python is now initialised when a `greta_array` is created (#468).

* head and tail S3 methods for `greta_array` are now consistent with head and tail methods for R versions 3 and 4 ([#384](https://github.com/greta-dev/greta/issues/384)).

* `greta_mcmc_list` objects (returned by `mcmc()`) are now no longer modified by operations (like `coda::gelman.diag()`). 

* joint distributions of uniform variables now have the correct constraints when sampling (#377).

* array-scalar dispatch with 3D arrays is now less buggy (#298).

* `greta` now provides R versions of all of R's primitive functions (I think), to prevent them from silently not executing (#317).

* Uses `Sys.unsetenv("RETICULATE_PYTHON")` in `.onload` on package startup, 
  to prevent an issue introduced with the "ghost orchid" version of RStudio where they do not find the current version of RStudio. See [#444](https://github.com/greta-dev/greta/issues/444) for more details.
  
* Internal change to code to ensure `future` continues to support parallelisation of chains. See [#447](https://github.com/greta-dev/greta/issues/447) for more details.

* `greta` now depends on `future` version 1.22.1, `tensorflow` (the R package) 2.7.0, and `parallelly` 1.29.0. This should see no changes on the user side.

## API changes:

* Now depends on R >= 3.1.0 ([#386](https://github.com/greta-dev/greta/issues/386))

* `chol2inv.greta_array()` now warns user about LINPACK argument being ignored, and also reminds user it has been deprecated since R 3.1

* `calculate()` now accepts multiple greta arrays for which to calculate values, via the `...` argument. As a consequence any other arguments must now be named.

* A number of optimiser methods are now deprecated, since they will be unavailable when greta moves to using TensorFlow v2.0: `powell()`, `cg()`, `newton_cg()`, `l_bfgs_b()`, `tnc()`, `cobyla()`, and `slsqp()`.

* `dirichlet()` now returns a variable (rather than an operation) greta array, and the graphs created by `lkj_correlation()` and `wishart()` are now simpler as cholesky-shaped variables are now available internally.

* Adds the `reinstall_greta_env()`, `reinstall_miniconda()`, `remove_greta_env()`, and  `remove_miniconda()` helper functions for helping installation get to "clean slate" (#443).

* `greta` currently doesn't work on Apple Silicon (M1 Macs) as they need to use TF 2.0, which is currently being implemented. `greta` now throws an error if M1 macs are detected and directs users to https://github.com/greta-dev/greta/issues/458 (#487)

## Features:

* New `install_greta_deps()` - provides installation of python dependencies (#417). This saves exact versions of Python (3.7), and the python modules NumPy (1.16.4), Tensorflow (1.14.0), and Tensorflow Probability (0.7.0) into a conda environment, "greta-env". When initialising Python, greta now searches for this conda environment first, which presents a great advantage as it isolates these exact versions of these modules from other Python installations. It is not required to use the conda environment, "greta-env". Overall this means that users can run the function `install_greta_deps()`, follow the prompts, and have all the python modules they need installed, without contaminating other  software that use different python modules.

* `calculate()` now enables simulation of greta array values from their priors, optionally conditioned on fixed values or posterior samples. This enables prior and posterior predictive checking of models, and simulation of data.

* A `simulate()` method for greta models is now also provided, to simulate the values of all greta arrays in a model from their priors. 

* `variable()` now accepts arrays for `upper` and `lower`, enabling users to define variables with different constraints.

* There are three new variable constructor functions: `cholesky_variable()`, `simplex_variable()`, and `ordered_variable()`, for variables with these constraints but no probability distribution.

* New `chol2symm()` is the inverse of `chol()`.

* `mcmc()`, `stashed_samples()`, and `calculate()` now return objects of class `greta_mcmc_list` which inherit from `coda`'s `mcmc.list` class, but enable custom greta methods for manipulating mcmc outputs, including a `window()` function.

* `mcmc()` and `calculate()` now have a `trace_batch_size` argument enabling users to trade-off computation speed versus memory requirements when calculating posterior samples for target greta arrays (#236).

* Many message, warning, and error prompts have been replaced internally with the {cli} R package for nicer printing. This is a minor change that should result in a more pleasant user experience (#423 #425).

* Internally, where sensible, `greta` now uses the `glue` package to create messages/ouputs (#378).

* New FAQ page and updated installation instructions for installing Python dependencies (#424)

* New `greta_sitrep()` function to generate a situation report of the software
  that is available for use, and also initialising python so `greta` is ready to
  use. (#441)

# greta 0.3.1

This release is predominantly a patch to make greta work with recent versions of TensorFlow and TensorFlow Probability, which were not backward compatible with the versions on which greta previously depended. From this release forward, greta will depend on specific (rather than minimum) versions of these two pieces of software to avoid it breaking if more changes are made to the APIS of these packages.

* greta now (only) works with TensorFlow 1.14.0 and TensorFlow Probability 0.7.0 (#289, #290)

* behaviour of the `pb_update` argument to `mcmc()` has been changed slightly to avoid a bad interaction with thinning (#284)

* various edits to the documentation to fix spelling mistakes and typos

# greta 0.3.0

This is a very large update which adds a number of features and major speed improvements. We now depend on the TensorFlow Probability Python package, and use functionality in that package wherever possible. Sampling a simple model now takes ~10s, rather than ~2m (>10x speedup).

## Fixes:

### operation bugs

* `dim<-()` now always rearranges elements in column-major order (R-style, not Python-style)

### performance bugs

* removed excessive checking of TF installation by operation greta arrays (was slowing down greta array creation for complex models)
* sped up detection of sub-DAGs in model creation (was slowing down model definition for complex models)
* reduced passing between R, Python, and TensorFlow during sampling (was slowing down sampling)

## New Functionality:

### inference methods

* 18 new optimisers have been added
* initial values can now be passed for some or all parameters
* 2 new MCMC samplers have been added: random-walk Metropolis-Hastings (thanks to @michaelquinn32) and slice sampling
* improved tuning of MCMC during warmup (thanks to @martiningram)
* integration with the `future` package for execution of MCMC chains on remote machines. Note: it is not advised to use `future` for parallel execution of chains on the same machine, that is now automatically handled by greta.
* the `one_by_one` argument to MCMC can handle serious numerical errors (such as failed matrix inversions) as 'bad' samples
* new `extra_samples()` function to continue sampling from a model.
* `calculate()` works on the output of MCMC, to enable post-hoc posterior prediction

### distributions

* multivariate distributions now accept matrices of parameter values
* added `mixture()` and `joint()` distribution constructors

### operations

* added functions: `abind()`, `aperm()`, `apply()`, `chol2inv()`, `cov2cor()`, `eigen()`, `identity()`, `kronecker()`, `rdist()`, and `tapply()` (thanks to @jdyen)
* we now automatically skip operations if possible, e.g. computing binomial and poisson densities with log-, logit- or probit-transformed parameters where they exist, or skipping cholesky decomposition of a matrix if it was created from its cholesky factor. This increases numerical stability as well as speed.

### misc

* ability to change the colour of the model plot (thanks to @dirmeier)
* ability to reshape greta arrays using `greta_array()`


## API changes:

### inference methods

* mcmc now runs 4 chains (simultaneously on all available cores), 1000 warmup steps, and 1000 samples by default
* optimisation and mcmc methods are now passed to `opt()` and `mcmc()` as objects, with defined tuning parameters. The `control` argument to these functions is now defunct.
* columns names for parameters now give the array indices for each scalar rather than a number (i.e. `x[2, 3]`, rather than `x.6`)

### distributions

* multivariate distributions now define each realisation as a row, and parameters must therefore have the same orientation

### misc

* `plot.greta_model()` now returns a `DiagrammeR::grViz` object (thanks to @flyaflya). This is less modifiable, but renders the plot more much consistently across different environments and notebook types. The `DiagrammeR` `dgr_graph` object use to create the `grViz` object is included as an attribute of this object, named `"dgr_graph"`.

## documentation

* lots more model examples (thanks to @leehazel, @dirmeier, @jdyen)
* two analysis case studies (thanks to @ShirinG, Tiphaine Martin, @mmulvahill, @michaelquinn32, @revodavid)
* new and improved pkgdown website (thanks to @pteetor)


## testing

* added tests of the validity of posterior samples drawn by MCMC (for known distributions and with Geweke tests)


# greta 0.2.5

Minor patch to handle an API change in the progress package. No changes in functionality.


# greta 0.2.4

## Fixes:

* improved error checking/messages in `model()`, `%*%`
* switched docs and examples to always use `<-` for assignment
* fixed the `n_cores` argument to `model()`

## New functionality:

* added a `calculate()` function to compute the values of greta arrays conditional on provided values for others
* added `imultilogit()` transform
* added a `chains` argument to `model()`
* improved HMC self-tuning, including a diagonal euclidean metric

# greta 0.2.3

## Fixes:

* fixed breaking change in extraDistr API (caused test errors on CRAN builds)
* added dontrun statements to pass CRAN checks on winbuilder
* fixed breaking change in tensorflow API (1-based indexing)

## New functionality:

* added `cumsum()` and `cumprod()` functions


# greta 0.2.2

## New functionality:

* added `forwardsolve()` and `backsolve()`
* added `colSums()`, `rowSums()`, `colMeans()`, and `rowMeans()`
* added `dim<-()` to reshape greta arrays
* `sweep()` now handles greta array `STATS` when `x` is numeric


# greta 0.2.1

## New functionality:

* export internal functions via `.internals` object to enable extension packages

API changes:

* removed the deprecated `define_model()`, an alias for `model()`
* removed the dynamics module, to be replaced by the gretaDynamics package

