### Probabilistic Modelling with TensorFlow

### 

Existing tools for fitting bespoke statistical models (such as BUGS,
JAGS and STAN) are very effective for moderately-sized problems, but
don't scale so well to large datasets. These tools also require users to
learn a domain-specific language and fix errors at compile time.

`grete` enables users to write probabilistic models interactively in
native R code, then sample from those models efficiently using
Hamiltonian Monte Carlo. Most of the calculations are performed using
TensorFlow, so `grete` is particularly fast where the model contains
lots of linear algebra. `grete` can also be run across distributed
machines or on GPUs, just by installing the relevant version of
TensorFlow.

This package is in the early stages of development. Future releases will
likely enable fitting models with fast approximate inference schemes,
different samplers, and more distributions and operations.

### Installation

`grete` depends on the `tensorflow` R package, which will need to be
succesfully installed before `grete` will work. To successfully install
`tensorflow`, you will need a working installation of python; to have
installed the correct version of TensorFlow; and then install
tensorflow, pointing to the version of python against which TensorFlow
was installed. Full installation details can be found [at the
`tensorflow` R API site](https://rstudio.github.io/tensorflow/).

Once you've successfully installed these prerequisites, `grete` can be
installed from github using the devtools package

    devtools::install_github('goldingn/grete')

Example
-------

The following example fits a large hierarchical linear regression model
using grete. This runs in around a minute on my laptop.

    # generate a large (10,000 observations, 100 covariates) fake data set
    n <- 10000
    m <- 100

    # true parameters
    true_alpha <- -3
    true_beta <- rnorm(m, 1, 2)
    true_sigma <- 3

    covariates <- matrix(rnorm(n * m, 0, 10),
                         nrow = n,
                         ncol = m)

    true_z <- true_alpha + covariates %*% true_beta
    response <- rnorm(n, true_z, true_sigma)

    # fit a model with grete
    library(grete)

    # define data as observed variables
    x = observed(covariates)
    y = observed(response)

    # define priors
    sigma = lognormal(0, 10)
    alpha = normal(0, 10)
    mu_beta <- normal(0, 10)
    sigma_beta <- lognormal(0, 10)

    # hierarchical structure on regression coefficients
    beta = normal(mu_beta, sigma_beta, dim = m)

    # linear model
    z = alpha + x %*% beta

    # specify likelihood (y already defined as observed)
    y %~% normal(z, sigma)

    # draw samples
    draws <- sample(alpha, beta, sigma,
                    method = 'hmc',
                    n_samples = 1000)

Why 'grete'?
------------

There's a recent convention of naming probabilistic modelling software
after pioneers in the field (e.g.
[STAN](https://en.wikipedia.org/wiki/Stanislaw_Ulam) and
[Edward](https://en.wikipedia.org/wiki/George_E._P._Box)).

[Grete Hermann](https://en.wikipedia.org/wiki/Grete_Hermann) wasn't a
probabilist, but she wrote [the first
algorithms](http://dl.acm.org/citation.cfm?id=307342&coll=portal&dl=ACM)
for computer algebra; in the 1920s, well before the first electronic
computer was built. This work laid the foundations for computer algebra
libraries (like TensorFlow) that enable modern probabilistic modelling.

In case that's not enough reason to admire her, Grete Hermann also
[disproved a popular theorem in quantum
theory](https://arxiv.org/pdf/0812.3986.pdf) and was part of the German
resistance against the Nazi regime prior to World War Two.

I pronounce the package as *Greh*•tuh. You can pronounce it however you
like.
