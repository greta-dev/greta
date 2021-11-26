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

Or install the development version of `greta` from [r-universe](https://greta-dev.r-universe.dev/ui#builds):

```r
# Enable this universe
options(
  repos = c(
    gretadev = 'https://greta-dev.r-universe.dev',
    CRAN = 'https://cloud.r-project.org'
    )
  )

# Install greta
install.packages('greta')
```

(Note - installing from r-universe is just like installing from CRAN, and should be faster and more convenient than installing from GitHub)

You can also install the development version of `greta` via GitHub:

``` r
devtools::install_github("greta-dev/greta")
```

<!-- badges: start -->
[![Codecov test coverage](https://codecov.io/gh/greta-dev/greta/branch/master/graph/badge.svg)](https://app.codecov.io/gh/greta-dev/greta?branch=master)
[![R-CMD-check](https://github.com/greta-dev/greta/workflows/R-CMD-check/badge.svg)](https://github.com/greta-dev/greta/actions)
[![cran
version](http://www.r-pkg.org/badges/version/greta)](https://CRAN.R-project.org/package=greta)
[![license](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)
[![doi](https://zenodo.org/badge/73758247.svg)](https://zenodo.org/badge/latestdoi/73758247)
[![joss](https://joss.theoj.org/papers/10.21105/joss.01601/status.svg)](https://joss.theoj.org/papers/10.21105/joss.01601)
![](logos/bottom_banner.png)
<!-- badges: end -->
