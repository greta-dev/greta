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
List of contributors : Matt Mulvahill, Shirin Glander, Michael Quinn, David Smith, Tiphaine Martin

Several vignettes of examples using greta have been added :
* linear mixed model with one random effect : https://github.com/revodavid/greta-examples/blob/master/milk.R
* linear mixed model with comparaison with lm : https://github.com/revodavid/greta-examples/blob/master/mtcars.R
* linear mixed model based on an example from a TensorFlow Probability Jupyter notebook and with a Comparison with Edward2 HMC : https://github.com/ropenscilabs/greta/blob/unconf/vignettes/8_schools_example_model.Rmd 
* linear mixed model running with parallel sesions using future R package : https://github.com/ropenscilabs/greta/blob/arm-models-election/vignettes/election88.Rmd

Several issues in the development of these examples have been met and have been documented in the web page "get started" (https://github.com/ropenscilabs/greta/blob/unconf/docs/get_started.html)
* Installation of Tensorflow libraries (tensorflow probability, tf-nightly, tfp-nightly)
* Installation of DiagrammeR and this dependancies (igraph, XML) for ubuntu and MacOS
* clear a bunch of tf objects after modification of models and before plotting the model and evaluating without restarting R session in using 

We also updated the documentation about n_samples option in mcmc function to inform that the number of samples is per chain and not the total. 


[![build status](https://travis-ci.org/greta-dev/greta.svg?branch=master)](https://travis-ci.org/greta-dev/greta) [![codecov.io](https://codecov.io/github/greta-dev/greta/coverage.svg?branch=master)](https://codecov.io/github/greta-dev/greta?branch=master) [![cran version](http://www.r-pkg.org/badges/version/greta)](https://cran.rstudio.com/web/packages/greta) [![license](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0) [![doi](https://zenodo.org/badge/73758247.svg)](https://zenodo.org/badge/latestdoi/73758247) <img src="README_files/bottom_banner.png" width="940" />
