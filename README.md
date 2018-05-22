<img src="README_files/top_banner.png" width="4032" />

### greta is an R package for writing statistical models and fitting them by MCMC.

greta lets you write your own model like in BUGS, JAGS and Stan, except that you write models right in R, it scales well to massive datasets, and it's easy to extend and build on.

### See the [website](https://greta-dev.github.io/greta/) for more information, [tutorials](https://greta-dev.github.io/greta/get_started.html), [examples](https://greta-dev.github.io/greta/example_models.html), and [package documentation](https://greta-dev.github.io/greta/reference-index.html).

You can install the current release version of the package (0.2.5) from GitHub:

``` r
devtools::install_github("greta-dev/greta")
```
I would love to hear any feedback, bug reports or feature requests via the [issues tracker](https://github.com/greta-dev/greta/issues). I would also be very keen for contributions from anyone with time to spare!

### Work during rOpenSci Unconference 2018
Dates: May, 21th-22th 2018

List of contributors : Shirin Glander (www.shirin-glander.de), Tiphaine Martin (https://github.com/TiphaineCMartin), Matt Mulvahill (https://github.com/mmulvahill), Michael Quinn (https://github.com/michaelquinn32), David Smith (https://github.com/revodavid). 

Several vignettes with examples using `greta` have been added :
* linear mixed model with one random effect : https://github.com/revodavid/greta-examples/blob/master/milk.R
* linear mixed model compared to `lm` : https://github.com/revodavid/greta-examples/blob/master/mtcars.R
* linear mixed model based on an example from a TensorFlow Probability Jupyter notebook and compared to Edward2 HMC: https://github.com/ropenscilabs/greta/blob/unconf/vignettes/8_schools_example_model.Rmd 
* linear mixed model running in parallel sessions using `future` R package : https://github.com/ropenscilabs/greta/blob/arm-models-election/vignettes/election88.Rmd

Several issues during the development of these examples were documented in the page "get started" (https://github.com/ropenscilabs/greta/blob/unconf/docs/get_started.html)
* Installation of TensorFlow libraries (`tensorflow_probability`, `tf-nightly`, `tfp-nightly`)
* Installation of `DiagrammeR` and its dependancies (`igraph`, `XML`) for ubuntu and MacOS
* Clearing a bunch of tf objects after modification and before plotting and evaluating the models without restarting R session 

A new type of sampler was added for Random Walk Metropolis Hastings.

We also updated the documentation about the `n_samples` option in the `mcmc()` function to inform that the number of samples is given per chain and not total. 


[![build status](https://travis-ci.org/greta-dev/greta.svg?branch=master)](https://travis-ci.org/greta-dev/greta) [![codecov.io](https://codecov.io/github/greta-dev/greta/coverage.svg?branch=master)](https://codecov.io/github/greta-dev/greta?branch=master) [![cran version](http://www.r-pkg.org/badges/version/greta)](https://cran.rstudio.com/web/packages/greta) [![license](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0) [![doi](https://zenodo.org/badge/73758247.svg)](https://zenodo.org/badge/latestdoi/73758247) <img src="README_files/bottom_banner.png" width="940" />
