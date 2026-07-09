# Package index

## creating greta arrays

Create greta arrays representing observed data or fixed values

- [`zeros()`](https://greta-dev.github.io/greta/reference/structures.md)
  [`ones()`](https://greta-dev.github.io/greta/reference/structures.md)
  [`greta_array()`](https://greta-dev.github.io/greta/reference/structures.md)
  : create data greta arrays
- [`as_data()`](https://greta-dev.github.io/greta/reference/as_data.md)
  : convert other objects to greta arrays

## variables & distributions

Create variables and assign probability distributions over greta arrays

- [`variable()`](https://greta-dev.github.io/greta/reference/variable.md)
  [`cholesky_variable()`](https://greta-dev.github.io/greta/reference/variable.md)
  [`simplex_variable()`](https://greta-dev.github.io/greta/reference/variable.md)
  [`ordered_variable()`](https://greta-dev.github.io/greta/reference/variable.md)
  : create greta variables
- [`uniform()`](https://greta-dev.github.io/greta/reference/distributions.md)
  [`normal()`](https://greta-dev.github.io/greta/reference/distributions.md)
  [`lognormal()`](https://greta-dev.github.io/greta/reference/distributions.md)
  [`bernoulli()`](https://greta-dev.github.io/greta/reference/distributions.md)
  [`binomial()`](https://greta-dev.github.io/greta/reference/distributions.md)
  [`beta_binomial()`](https://greta-dev.github.io/greta/reference/distributions.md)
  [`negative_binomial()`](https://greta-dev.github.io/greta/reference/distributions.md)
  [`hypergeometric()`](https://greta-dev.github.io/greta/reference/distributions.md)
  [`poisson()`](https://greta-dev.github.io/greta/reference/distributions.md)
  [`gamma()`](https://greta-dev.github.io/greta/reference/distributions.md)
  [`inverse_gamma()`](https://greta-dev.github.io/greta/reference/distributions.md)
  [`weibull()`](https://greta-dev.github.io/greta/reference/distributions.md)
  [`exponential()`](https://greta-dev.github.io/greta/reference/distributions.md)
  [`pareto()`](https://greta-dev.github.io/greta/reference/distributions.md)
  [`student()`](https://greta-dev.github.io/greta/reference/distributions.md)
  [`laplace()`](https://greta-dev.github.io/greta/reference/distributions.md)
  [`beta()`](https://greta-dev.github.io/greta/reference/distributions.md)
  [`cauchy()`](https://greta-dev.github.io/greta/reference/distributions.md)
  [`chi_squared()`](https://greta-dev.github.io/greta/reference/distributions.md)
  [`logistic()`](https://greta-dev.github.io/greta/reference/distributions.md)
  [`f()`](https://greta-dev.github.io/greta/reference/distributions.md)
  [`multivariate_normal()`](https://greta-dev.github.io/greta/reference/distributions.md)
  [`wishart()`](https://greta-dev.github.io/greta/reference/distributions.md)
  [`lkj_correlation()`](https://greta-dev.github.io/greta/reference/distributions.md)
  [`multinomial()`](https://greta-dev.github.io/greta/reference/distributions.md)
  [`categorical()`](https://greta-dev.github.io/greta/reference/distributions.md)
  [`dirichlet()`](https://greta-dev.github.io/greta/reference/distributions.md)
  [`dirichlet_multinomial()`](https://greta-dev.github.io/greta/reference/distributions.md)
  : probability distributions
- [`` `distribution<-`() ``](https://greta-dev.github.io/greta/reference/distribution.md)
  [`distribution()`](https://greta-dev.github.io/greta/reference/distribution.md)
  : define a distribution over data
- [`mixture()`](https://greta-dev.github.io/greta/reference/mixture.md)
  : mixtures of probability distributions
- [`joint()`](https://greta-dev.github.io/greta/reference/joint.md) :
  define joint distributions

## manipulating greta arrays

Functions and operations for modifying greta arrays

- [`operators`](https://greta-dev.github.io/greta/reference/operators.md)
  : arithmetic, logical and relational operators for greta arrays
- [`functions`](https://greta-dev.github.io/greta/reference/functions.md)
  : functions for greta arrays
- [`extract-replace-combine`](https://greta-dev.github.io/greta/reference/extract-replace-combine.md)
  [`extract`](https://greta-dev.github.io/greta/reference/extract-replace-combine.md)
  [`replace`](https://greta-dev.github.io/greta/reference/extract-replace-combine.md)
  [`cbind`](https://greta-dev.github.io/greta/reference/extract-replace-combine.md)
  [`rbind`](https://greta-dev.github.io/greta/reference/extract-replace-combine.md)
  [`c`](https://greta-dev.github.io/greta/reference/extract-replace-combine.md)
  [`rep`](https://greta-dev.github.io/greta/reference/extract-replace-combine.md)
  : extract, replace and combine greta arrays
- [`iprobit()`](https://greta-dev.github.io/greta/reference/transforms.md)
  [`ilogit()`](https://greta-dev.github.io/greta/reference/transforms.md)
  [`icloglog()`](https://greta-dev.github.io/greta/reference/transforms.md)
  [`icauchit()`](https://greta-dev.github.io/greta/reference/transforms.md)
  [`log1pe()`](https://greta-dev.github.io/greta/reference/transforms.md)
  [`imultilogit()`](https://greta-dev.github.io/greta/reference/transforms.md)
  : transformation functions for greta arrays
- [`` `%*%` ``](https://greta-dev.github.io/greta/reference/overloaded.md)
  [`chol2inv()`](https://greta-dev.github.io/greta/reference/overloaded.md)
  [`cov2cor()`](https://greta-dev.github.io/greta/reference/overloaded.md)
  [`identity()`](https://greta-dev.github.io/greta/reference/overloaded.md)
  [`colMeans()`](https://greta-dev.github.io/greta/reference/overloaded.md)
  [`rowMeans()`](https://greta-dev.github.io/greta/reference/overloaded.md)
  [`colSums()`](https://greta-dev.github.io/greta/reference/overloaded.md)
  [`rowSums()`](https://greta-dev.github.io/greta/reference/overloaded.md)
  [`sweep()`](https://greta-dev.github.io/greta/reference/overloaded.md)
  [`outer()`](https://greta-dev.github.io/greta/reference/overloaded.md)
  [`` `%o%` ``](https://greta-dev.github.io/greta/reference/overloaded.md)
  [`backsolve()`](https://greta-dev.github.io/greta/reference/overloaded.md)
  [`forwardsolve()`](https://greta-dev.github.io/greta/reference/overloaded.md)
  [`apply()`](https://greta-dev.github.io/greta/reference/overloaded.md)
  [`tapply()`](https://greta-dev.github.io/greta/reference/overloaded.md)
  [`eigen()`](https://greta-dev.github.io/greta/reference/overloaded.md)
  [`rdist()`](https://greta-dev.github.io/greta/reference/overloaded.md)
  [`abind()`](https://greta-dev.github.io/greta/reference/overloaded.md)
  [`diag()`](https://greta-dev.github.io/greta/reference/overloaded.md)
  : Functions overloaded by greta

## modelling

Define and visualise models and fit them to data

- [`model()`](https://greta-dev.github.io/greta/reference/model.md)
  [`print(`*`<greta_model>`*`)`](https://greta-dev.github.io/greta/reference/model.md)
  [`plot(`*`<greta_model>`*`)`](https://greta-dev.github.io/greta/reference/model.md)
  : greta model objects
- [`mcmc()`](https://greta-dev.github.io/greta/reference/inference.md)
  [`stashed_samples()`](https://greta-dev.github.io/greta/reference/inference.md)
  [`extra_samples()`](https://greta-dev.github.io/greta/reference/inference.md)
  [`initials()`](https://greta-dev.github.io/greta/reference/inference.md)
  [`opt()`](https://greta-dev.github.io/greta/reference/inference.md) :
  Statistical inference on greta models.
- [`calculate()`](https://greta-dev.github.io/greta/reference/calculate.md)
  : calculate greta arrays given fixed values
- [`hmc()`](https://greta-dev.github.io/greta/reference/samplers.md)
  [`rwmh()`](https://greta-dev.github.io/greta/reference/samplers.md)
  [`slice()`](https://greta-dev.github.io/greta/reference/samplers.md) :
  MCMC samplers
- [`nelder_mead()`](https://greta-dev.github.io/greta/reference/optimisers.md)
  [`bfgs()`](https://greta-dev.github.io/greta/reference/optimisers.md)
  [`powell()`](https://greta-dev.github.io/greta/reference/optimisers.md)
  [`momentum()`](https://greta-dev.github.io/greta/reference/optimisers.md)
  [`cg()`](https://greta-dev.github.io/greta/reference/optimisers.md)
  [`newton_cg()`](https://greta-dev.github.io/greta/reference/optimisers.md)
  [`l_bfgs_b()`](https://greta-dev.github.io/greta/reference/optimisers.md)
  [`tnc()`](https://greta-dev.github.io/greta/reference/optimisers.md)
  [`cobyla()`](https://greta-dev.github.io/greta/reference/optimisers.md)
  [`slsqp()`](https://greta-dev.github.io/greta/reference/optimisers.md)
  [`gradient_descent()`](https://greta-dev.github.io/greta/reference/optimisers.md)
  [`adadelta()`](https://greta-dev.github.io/greta/reference/optimisers.md)
  [`adagrad()`](https://greta-dev.github.io/greta/reference/optimisers.md)
  [`adagrad_da()`](https://greta-dev.github.io/greta/reference/optimisers.md)
  [`adam()`](https://greta-dev.github.io/greta/reference/optimisers.md)
  [`adamax()`](https://greta-dev.github.io/greta/reference/optimisers.md)
  [`ftrl()`](https://greta-dev.github.io/greta/reference/optimisers.md)
  [`proximal_gradient_descent()`](https://greta-dev.github.io/greta/reference/optimisers.md)
  [`proximal_adagrad()`](https://greta-dev.github.io/greta/reference/optimisers.md)
  [`nadam()`](https://greta-dev.github.io/greta/reference/optimisers.md)
  [`rms_prop()`](https://greta-dev.github.io/greta/reference/optimisers.md)
  : optimisation methods

## extending greta

Write R packages that extend or use greta

- [`internals`](https://greta-dev.github.io/greta/reference/internals.md)
  [`.internals`](https://greta-dev.github.io/greta/reference/internals.md)
  : internal greta methods

## Misc

Misc functions to sort through later

- [`greta-package`](https://greta-dev.github.io/greta/reference/greta.md)
  [`greta`](https://greta-dev.github.io/greta/reference/greta.md) :
  greta: simple and scalable statistical modelling in R

- [`greta_notes_tf_num_error()`](https://greta-dev.github.io/greta/reference/stash-notes.md)
  : Retrieve python messages.

- [`run_optimiser()`](https://greta-dev.github.io/greta/reference/run_optimiser.md)
  : Dispatch optimisation method to right class

- [`simulate(`*`<greta_model>`*`)`](https://greta-dev.github.io/greta/reference/simulate.greta_model.md)
  :

  Simulate Responses From `greta_model` Object

- [`gpu_only()`](https://greta-dev.github.io/greta/reference/gpu_cpu.md)
  [`cpu_only()`](https://greta-dev.github.io/greta/reference/gpu_cpu.md)
  : Set GPU or CPU usage

- [`chol2symm()`](https://greta-dev.github.io/greta/reference/chol2symm.md)
  : Cholesky Factor to Symmetric Matrix

- [`chol(`*`<greta_array>`*`)`](https://greta-dev.github.io/greta/reference/chol.greta_array.md)
  : Compute the Cholesky Factor of a Matrix

- [`as.greta_model()`](https://greta-dev.github.io/greta/reference/as.greta_model.md)
  : Convert object to a "greta_model" object

- [`as.unknowns()`](https://greta-dev.github.io/greta/reference/as.unknowns.md)
  : Create objects of class 'unknowns' to nicely print ? valued arrays

- [`` `dim<-`( ``*`<unknowns>`*`)`](https://greta-dev.github.io/greta/reference/dim-set-.unknowns.md)
  : set dims like on a matrix/array

- [`dim(`*`<node>`*`)`](https://greta-dev.github.io/greta/reference/dim.node.md)
  : generic to grab dimensions of nodes

- [`is.greta_mcmc_list()`](https://greta-dev.github.io/greta/reference/is.greta_mcmc_list.md)
  :

  Is object a `greta_mcmc_list`?

- [`print(`*`<greta_deps_spec>`*`)`](https://greta-dev.github.io/greta/reference/print.greta_deps_spec.md)
  : Print method for greta python deps

- [`is.greta_array()`](https://greta-dev.github.io/greta/reference/is.greta_array.md)
  : Is object a greta array?

- [`print(`*`<greta_mcmc_list>`*`)`](https://greta-dev.github.io/greta/reference/print.greta_mcmc_list.md)
  : Print method for greta MCMC list

## Installation helpers

Functions for managing installation of dependencies

- [`greta_remove()`](https://greta-dev.github.io/greta/reference/greta_remove.md)
  : Remove greta's Python dependencies
- [`greta_create_conda_env()`](https://greta-dev.github.io/greta/reference/greta_create_conda_env.md)
  : Create conda environment for greta
- [`greta_deps_receipt()`](https://greta-dev.github.io/greta/reference/greta_deps_receipt.md)
  : Capture greta python dependencies.
- [`greta_deps_spec()`](https://greta-dev.github.io/greta/reference/greta_deps_spec.md)
  : Specify python dependencies for greta
- [`greta_sitrep()`](https://greta-dev.github.io/greta/reference/greta_sitrep.md)
  : Greta Situation Report
- [`greta_deps_tf_tfp`](https://greta-dev.github.io/greta/reference/greta_deps_tf_tfp.md)
  : Suggested valid Python dependencies for greta
- [`greta_install_miniconda()`](https://greta-dev.github.io/greta/reference/greta_install_miniconda.md)
  : Installs miniconda
- [`install_greta_deps()`](https://greta-dev.github.io/greta/reference/install_greta_deps.md)
  [`reinstall_greta_deps()`](https://greta-dev.github.io/greta/reference/install_greta_deps.md)
  : Install Python dependencies for greta
- [`greta_list_py_modules()`](https://greta-dev.github.io/greta/reference/greta_list_py_modules.md)
  : List Python modules installed in greta env
- [`greta_set_install_logfile()`](https://greta-dev.github.io/greta/reference/greta_set_install_logfile.md)
  : Set logfile path when installing greta
- [`write_greta_install_log()`](https://greta-dev.github.io/greta/reference/write_greta_install_log.md)
  : Write greta dependency installation log file
- [`open_greta_install_log()`](https://greta-dev.github.io/greta/reference/open_greta_install_log.md)
  : Read a greta logfile
- [`greta_set_python()`](https://greta-dev.github.io/greta/reference/greta_set_python.md)
  [`greta_reset_python()`](https://greta-dev.github.io/greta/reference/greta_set_python.md)
  : Choose the Python environment greta uses
- [`greta_set_deps()`](https://greta-dev.github.io/greta/reference/greta_set_deps.md)
  : Choose the dependency versions greta installs
- [`remove_greta_env()`](https://greta-dev.github.io/greta/reference/deprecated-installers.md)
  [`remove_miniconda()`](https://greta-dev.github.io/greta/reference/deprecated-installers.md)
  [`remove_reticulate_uv_cache()`](https://greta-dev.github.io/greta/reference/deprecated-installers.md)
  [`greta_remove_all_deps()`](https://greta-dev.github.io/greta/reference/deprecated-installers.md)
  [`destroy_greta_deps()`](https://greta-dev.github.io/greta/reference/deprecated-installers.md)
  [`reinstall_greta_env()`](https://greta-dev.github.io/greta/reference/deprecated-installers.md)
  [`reinstall_miniconda()`](https://greta-dev.github.io/greta/reference/deprecated-installers.md)
  **\[deprecated\]** : Deprecated removal and reinstallation helpers
