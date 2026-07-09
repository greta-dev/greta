![](logos/top_banner.png)

### greta is an R package for writing statistical models and fitting them by MCMC and optimisation

greta lets you write your own model like in BUGS, JAGS and Stan, except
that you write models right in R, it scales well to massive datasets,
and it’s easy to extend and build on.

### See the [website](https://greta-dev.github.io/greta/) for more information, including [tutorials](https://greta-dev.github.io/greta/articles/get_started.html), [examples](https://greta-dev.github.io/greta/articles/example_models.html), [package documentation](https://greta-dev.github.io/greta/reference/index.html), and the [greta forum](https://forum.greta-stats.org).

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
remotes::install_github("greta-dev/greta")
```

# Installing Python dependencies

`greta` uses Google's [TensorFlow](https://www.tensorflow.org/) and [tensorflow-probability](https://github.com/tensorflow/probability) Python packages under the hood. For most users there is nothing extra to install: the first time you use `greta` in a session, it automatically installs a compatible Python, TensorFlow, and TensorFlow Probability via [`uv`](https://docs.astral.sh/uv/). So `library(greta)` followed by your first model usually just works.

If you need a conda environment instead (for example on an offline network), or want to pin specific versions, use `install_greta_deps()` and then point `greta` at the environment with `greta_set_python("conda")`. By default it installs TF 2.15.0, TFP 0.23.0, and Python 3.10; choose versions via its `deps` argument and `greta_deps_spec()`. See the "Installing dependencies" vignette, `?install_greta_deps`, or `?greta_set_python` for more.

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
