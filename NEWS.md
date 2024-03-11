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

