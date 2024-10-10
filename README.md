![](logos/top_banner.png)

### greta is an R package for writing statistical models and fitting them by MCMC and optimisation

greta lets you write your own model like in BUGS, JAGS and Stan, except
that you write models right in R, it scales well to massive datasets,
and it’s easy to extend and build on.

### See the [website](https://greta-stats.org/) for more information, including [tutorials](https://greta-stats.org/articles/get_started.html), [examples](https://greta-stats.org/articles/example_models.html), [package documentation](https://greta-stats.org/reference/index.html), and the [greta forum](https://forum.greta-stats.org).

You can install the current release version of the package from
CRAN:

``` r
install.packages("greta")
```

Or install the development version of `greta` from [r-universe](http://greta-dev.r-universe.dev/):

```r
install.packages("greta", repos = c("https://greta-dev.r-universe.dev", "https://cloud.r-project.org"))
```

(Note - installing from r-universe is just like installing from CRAN, and should be faster and more convenient than installing from GitHub)

You can also install the development version of `greta` via GitHub:

``` r
devtools::install_github("greta-dev/greta")
```

# Installing Python Dependencies

The `install_greta_deps()` function helps install the Python dependencies (Google's [TensorFlow](https://www.tensorflow.org/) and [tensorflow-probability](https://github.com/tensorflow/probability)). 

By default, `install_greta_deps()` installs versions TF 2.15.0, and TFP version 0.23.0, using python 3.10. To change the versions of TF, TFP, or python that you want to use, you specify the `deps` argument of `install_greta_deps()`, which used `greta_deps_spec()`. See `?install_greta_deps()` or `?greta_deps_spec()` for more information.

This helper function, `install_greta_deps()`, installs the exact pythons package versions needed. It also places these inside a conda environment, "greta-env-tf2". This isolates these exact python modules from other python installations, so that only `greta` will see them. This helps avoids installation issues, where previously you might update tensorflow on your computer and overwrite the current version needed by `greta`. Using this "greta-env-tf2" conda environment means installing other python packages should not be impact the Python packages needed by `greta`.

If these python modules aren't yet installed, when `greta` is used, it provides instructions on how to install them for your system. If in doubt follow those. 

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/greta)](https://CRAN.R-project.org/package=greta)
[![R-CMD-check](https://github.com/greta-dev/greta/workflows/R-CMD-check/badge.svg)](https://github.com/greta-dev/greta/actions)
[![greta status badge](https://greta-dev.r-universe.dev/badges/greta)](https://greta-dev.r-universe.dev/greta)
[![Codecov test coverage](https://codecov.io/gh/greta-dev/greta/branch/master/graph/badge.svg)](https://app.codecov.io/gh/greta-dev/greta?branch=master)


[![license](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/license/apache-2-0)
[![doi](https://zenodo.org/badge/73758247.svg)](https://zenodo.org/badge/latestdoi/73758247)
[![joss](https://joss.theoj.org/papers/10.21105/joss.01601/status.svg)](https://joss.theoj.org/papers/10.21105/joss.01601)
<!-- badges: end -->

![](logos/bottom_banner.png)
