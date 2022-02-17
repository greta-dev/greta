# greta 0.4.0.9000 (2022-02-09)

## Summary

This release presents a variety of improvements over the past 2 years. We are now aiming to have smaller, more regular releases of `greta`. This release showcases new features implemented by Nick Golding on the `calculate` and `simulate` functions. There are also many internal changes on installation, error printing, and testing.

### Installation

We have overhauled the installation checking process, and created a new helper function for installation, `install_greta_deps()`.

We need the Tensorflow and Tensorflow Probability Python modules to use `greta`.
When these aren't installed, this now triggers a new prompt which encourages users
to use a new installation helper, that looks like this:

```
#> We have detected that you do not have the expected python packages setup.
#> You can set these up by running this R code in the console:
#> `install_greta_deps()`
#> Then, restart R and run:
#> `library(greta)`
#> (Note: Your R session should not have initialised Tensorflow yet.)
#> For more information, see `?install_greta_deps`
```

Running `install_greta_deps()` will then go through the process of installing the dependencies, and ask the user to restart R and load `greta` to get it working:

```
#> ✓ Installation of greta dependencies is complete!
#> • Restart R, then load greta with: `library(greta)`
```

The `install_greta_deps()` function helps ensure Python dependencies are installed correctly. This saves exact versions of Python (3.7), and the python modules NumPy (1.16.4), Tensorflow (1.14.0), and Tensorflow Probability (0.7.0) into a conda environment, "greta-env". 

So what is a conda environment? It is similar to the R projects, `packrat` and `renv` (although I believe conda environments are a much older idea!). It allows you to use specific versions of Python and Pythong modules (Python module = R Package) that do not interact with other projects. Essentially, you "activate" a specific conda environment, which loads the specified Python version and modules. This means you avoid situations where you might update a python module and then all your other code breaks because breaking changes were introduced in a new version.

Why do we need this? Currently `greta` needs specific versions of Tensorflow and Tensorflow Probability, and we know that those specific versions work with a specific version of Python. We wanted to keep things stable for users, so they don't have to go through the (often) painful process of installing dependencies.

How does it work? When `greta` is loaded, say with `library(greta)`, it searches for a "greta-env" conda environment and loads it. It is not required to use the conda environment, "greta-env", so you can install these Python modules yourself.

Overall this means that users can run the function `install_greta_deps()`, follow the prompts, and have all the python modules they need installed, without contaminating other  software that use different python modules.

### Error printing

We have rewritten the error message printing methods to use the `cli` package for prettier, more informative testing. We have also used the `glue` package in place of most uses of `sprintf` or `paste/0`, as the literal string interpolation makes it easier to maintain. This means we get nifty outputs like this from the new `greta_sitrep()` function:

``` r
greta::greta_sitrep()
#> ℹ checking if python available
#> ✓ python (version 3.7) available
#> 
#> ℹ checking if TensorFlow available
#> ✓ TensorFlow (version 1.14.0) available
#> 
#> ℹ checking if TensorFlow Probability available
#> ✓ TensorFlow Probability (version 0.7.0) available
#> 
#> ℹ checking if greta conda environment available
#> ✓ greta conda environment available
#> 
#> ℹ Initialising python and checking dependencies, this may take a moment.
#> ✓ Initialising python and checking dependencies ... done!
#> 
#> ℹ greta is ready to use!
```

### Testing

We have also overhauled the testing interface to use snapshotting, which makes it easier to write and test new error messages, and makes it easier to identify issues with existing print methods, errors, and warnings.

### Looking to the future

In a future release we will switch to using TensorFlow 2.6 (or higher), to ensure `greta` works with Apple computers with an M1 chip. We note that we have gone from  "skipped" version 0.4.0, however this is because we had a soft release of 0.4.0 on GitHub in December, and wanted to signify that this package has changed since that time.

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

