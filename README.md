![](README_files/figure-markdown_github/top_banner-1.png)

greta is an R package for writing statistical models and fitting them by MCMC, it's:

**easy** - greta models can be [written interactively](#example) in R, so there's no need to learn a new language like BUGS or Stan and if you make a mistake you get [feedback](#feedback) immediately, not from a compiler.

**fast** - greta does [Hamiltonian Monte Carlo](http://onlinelibrary.wiley.com/doi/10.1111/2041-210X.12681/full) and uses Google's [TensorFlow](https://www.tensorflow.org/) computational engine, meaning it's particularly fast on big datasets and can run in parallel across lots of CPUs, or on a GPU.

**extensible** - because greta is written in R, you can define your own [functions](#functions) and [modules](#modules) to add new methods.

![](README_files/figure-markdown_github/banner_1-1.png)

### Example

Here's a simple Bayesian linear regression model for the sepal length variable in R's `iris` dataset.

``` r
library(greta)

# create parameters, stating their prior distributions
intercept = normal(0, 5)
coefficient = normal(0, 3)
sd = lognormal(0, 3)

# write the equation for the expected mean sepal length
mean <- intercept + coefficient * iris$Petal.Length

# define the likelihood of the observed data
distribution(iris$Sepal.Length) = normal(mean, sd)
```

With the model written, we can draw samples of the parameters we care about.

``` r
model <- define_model(intercept, coefficient, sd)

draws <- mcmc(model, n_samples = 1000)
```

This outputs an `mcmc.list` object, so you can plot and summarise the samples using your favourite MCMC visualisation software.

``` r
library(MCMCvis)

MCMCtrace(draws)
MCMCplot(draws, xlim = c(-1, 5))
```

<img src="README_files/figure-markdown_github/vis-1.png" width="400px" /><img src="README_files/figure-markdown_github/vis-2.png" width="400px" />

<img src="README_files/figure-markdown_github/banner_1-1.png" width="1344" />

### Installation

greta can be installed from GitHub using the devtools package

``` r
devtools::install_github('goldingn/greta')
```

or, you can install the development version with:

``` r
devtools::install_github('goldingn/greta@dev')
```

however greta depends on TensorFlow (version 1.0.0 or higher) which will need to be successfully installed before greta will work. See [here](https://www.tensorflow.org/install/) for instructions on installing TensorFlow.

<img src="README_files/figure-markdown_github/banner_1-1.png" width="1344" />

### How fast is it?

For small to medium size (a few hundred data points) problems, Stan will probably be faster than greta. Where the model involves thousands of datapoints or multiplication of large matrices, greta is likely to be faster than STAN. That's because TensorFlow is heavily optimised for linear algebra operations.

For example, the example code above takes around 60 seconds to run on my laptop for the 150-row iris data. If you run the same model and sampler on a dataset of 15,000 rows, it still only takes around 65 seconds. That's not bad. Not bad at all.

Since TensorFlow can be run across multiple CPUs or on a GPU, greta models can be made to scale to massive datasets. I'll add some benchmarks soon to give a clearer idea of how greta compares with other MCMC software.

<img src="README_files/figure-markdown_github/banner_1-1.png" width="1344" />

### Why 'greta'?

There's a recent convention of naming probabilistic modelling software after pioneers in the field (e.g. [STAN](https://en.wikipedia.org/wiki/Stanislaw_Ulam) and [Edward](https://en.wikipedia.org/wiki/George_E._P._Box)).

[Grete Hermann](https://en.wikipedia.org/wiki/Grete_Hermann) wasn't a probabilist, but she wrote [the first algorithms](http://dl.acm.org/citation.cfm?id=307342&coll=portal&dl=ACM) for computer algebra; in the 1920s, well before the first electronic computer was built. This work laid the foundations for computer algebra libraries (like TensorFlow) that enable modern probabilistic modelling.

In case that's not enough reason to admire her, Grete Hermann also [disproved a popular theorem in quantum theory](https://arxiv.org/pdf/0812.3986.pdf) and was part of the German resistance against the Nazi regime prior to World War Two.

Grete (usually said *Greh*•tuh, like its alternate spelling *Greta*) can be confusing for non-German speakers to pronounce, so I've taken the liberty of naming the package greta instead. You can call it whatever you like.

<img src="README_files/figure-markdown_github/banner_1-1.png" width="1344" />

### How does it work?

With greta, you create and manipulate `greta_array` objects, which behave more-or-less like R's arrays. greta arrays can contain either data, random variables (with some probability distribution), or the result of applying some function to another greta array.

##### data

For example, we can convert other R objects, like vectors or matrices to greta arrays using the `as_data()` function:

``` r
sl <- as_data(iris$Sepal.Length)
head(sl)
```

    ## greta array (operation)
    ## 
    ##      [,1]
    ## [1,]  5.1
    ## [2,]  4.9
    ## [3,]  4.7
    ## [4,]  4.6
    ## [5,]  5.0
    ## [6,]  5.4

However many functions and mathematical operations will automagically transform data too, which is we we don't need to call `as_data()` in the example above. See `?as_data` for details on what types of object can be converted to greta arrays.

##### variables

We can also create greta arrays representing random or unknown variables, like model parameters. For a Bayesian model, we can define these via their prior distributions:

``` r
# a scalar variable
a = normal(mean = 0, sd = 10)
a
```

    ## greta array (variable following a normal distribution)
    ## 
    ##      [,1]
    ## [1,]   ?

``` r
# a 3x3 matrix with all elements having the same distribution
b = normal(mean = 0, sd = 1, dim = c(3, 3))
b
```

    ## greta array (variable following a normal distribution)
    ## 
    ##      [,1] [,2] [,3]
    ## [1,]   ?    ?    ? 
    ## [2,]   ?    ?    ? 
    ## [3,]   ?    ?    ?

The values of these distributions are as-yet unknown, so they are represented by `?`s when we print them. See `` ?`greta-distributions` `` for a list of the implemented distributions. If you don't want to define a prior over a variable (e.g. for a frequentist analysis), you can define variables using `free()` instead.

##### operations

greta arrays can be manipulated using R's standard arithmetic, logical and relational operators (`+`, `*`, etc., see `` ?`greta-operators` ``) and common functions (`sum()`, `log()` etc.; see `` ?`greta-functions` ``). When we are writing our model, we define new greta arrays as the output of these functions, but the functions aren't actually executed just yet. Instead, greta just works out what shape they should be and remembers what to do later when it comes to fit the model.

For example, we might want to multiply some data with a parameter, then transform and sum the result:

``` r
# sepal length multiplied by a parameter
c <- sl * a
head(c)
```

    ## greta array (operation)
    ## 
    ##      [,1]
    ## [1,]   ? 
    ## [2,]   ? 
    ## [3,]   ? 
    ## [4,]   ? 
    ## [5,]   ? 
    ## [6,]   ?

``` r
# log-transform and then sum these values
d <- sum(log(c))
d
```

    ## greta array (operation)
    ## 
    ##      [,1]
    ## [1,]   ?

As with the random variables, the outputs of these operations aren't yet known, so the values are represented by `?`s.

##### extract and replace

You can use R's extract and replace syntax (using `[`) on greta arrays, just as you can with R's vectors, matrices and arrays. E.g. to extract the two middle columns from a greta array we can do:

``` r
e <- ones(4, 3)
e[, 2:3]
```

    ## greta array (operation)
    ## 
    ##      [,1] [,2]
    ## [1,]    1    1
    ## [2,]    1    1
    ## [3,]    1    1
    ## [4,]    1    1

or for a single element:

``` r
e[1, 3]
```

    ## greta array (operation)
    ## 
    ##      [,1]
    ## [1,]    1

To make a matrix that has random variables in the first column, but zeros everywhere else, we could do:

``` r
# a 4x3 greta array of zeros
x <- zeros(4, 3)

# now with random variables in the first column
x[, 1] = normal(0, 1, dim = 4)
```

##### feedback

Because greta tracks the size and shape of these greta arrays, it will tell us if something we do doesn't make sense, like trying to add two objects with the wrong dimensions:

``` r
# try to add two differently shaped greta arrays
c[1:5] + c[1:2]
```

    ## Error: incompatible dimensions: 5x1, 2x1

##### functions

You're free to write your own functions for greta arrays, to simplify your code and share methods with others. For example to recreate the `inprod()` function from BUGS and JAGS, we could do:

``` r
inprod <- function (a, b) {
  a %*% b
}
```

Which we could use to get a linear combination of data and covariates

``` r
beta = normal(0, 1, dim = 3)
inprod(iris[1:10, 2:4], beta)
```

    ## greta array (operation)
    ## 
    ##       [,1]
    ##  [1,]   ? 
    ##  [2,]   ? 
    ##  [3,]   ? 
    ##  [4,]   ? 
    ##  [5,]   ? 
    ##  [6,]   ? 
    ##  [7,]   ? 
    ##  [8,]   ? 
    ##  [9,]   ? 
    ## [10,]   ?

##### distribution

The `distribution()` syntax lets us tell greta that some greta array (usually data) should follow a certain distribution, e.g. defining a likelihood so that we can fit the model to data. `distribution()` always goes on the left hand side, and with a distribution on the right hand side, like this:`distribution(<some_data>) = <some_distribution>(<some_parameters>)`

#### what happens next

When we're writing out the model by creating new greta arrays, greta doesn't actually execute any of the functions, it just remembers what to do to create the new greta array, and which existing greta arrays to use. When we run `define_model()`, greta rounds up all of the greta arrays connected to the parameters we care about - that defines our statistical model. We can then run `mcmc()` on the model, which uses an mcmc algorithm to try different values of the parameters and evaluate the 'joint density' of the model (either the posterior or the likelihood depending on whether the model was Bayesian).

#### software

greta relies on some pretty incredible pieces of software, including Rstudio's [`reticulate`](https://github.com/rstudio/reticulate) and [`tensorflow`](https://rstudio.github.io/tensorflow/) packages, which bring Google TensorFlow and all things python to R. Under the hood, greta also uses Winston Chang's [`R6`](https://github.com/wch/R6) object system.

The design and scope of greta was inspired by other general-purpose MCMC software like [BUGS](http://www.openbugs.net/) and [JAGS](http://mcmc-jags.sourceforge.net/), but particularly by [Stan](http://mc-stan.org/). The python package [Edward](http://edwardlib.org/) also uses TensorFlow as a backend for general-purpose statistical modelling, as does [GPflow](https://github.com/GPflow/GPflow), which was a source of inspiration for how greta is implemented.

<img src="README_files/figure-markdown_github/banner_1-1.png" width="1344" />

### Contributors

[![Build Status](https://travis-ci.org/goldingn/greta.svg?branch=master)](https://travis-ci.org/goldingn/greta) [![codecov.io](https://codecov.io/github/goldingn/greta/coverage.svg?branch=master)](https://codecov.io/github/goldingn/greta?branch=master) [![cran version](http://www.r-pkg.org/badges/version/greta)](https://cran.rstudio.com/web/packages/greta)

I would welcome contributions to this project from anyone with time to spare! The issues tracker lists a number of known bugs and extensions I have planned. Please feel free to add to those any bugs or issues you come across, or let me know if you'd like to help fix some of them or add new features.

#### modules

greta has a basic module system to package up more 'niche' functionality. Check out `?dynamics` for an example of a module for stage-structured dynamical models. I'm still working out whether these modules should be kept in this package, or split out into one or more separate packages. Either way I would be keen for people to contribute new modules!

#### some gory implementation details for contributors

There are three layers to how greta defines a model: users manipulate *greta arrays*, these define *nodes*, and nodes then define *Tensors*.

greta arrays are the user-facing representation of the model, but under the hood each greta array corresponds to an R6 `node` object, which is where the magic happens. Each node points to its 'child' nodes - the nodes for the greta arrays that were used to create this one. When `define_model()` is called, that inheritance information is used to construct the directed acyclic graph (DAG) that defines the model.

In addition to remembering where they are in the DAG, each node has a method to define a corresponding Tensor in a TensorFlow graph. `define_model()` triggers those methods to create a DAG for the model in TensorFlow. The pass-by-reference nature of R6 objects means each node can tell its child nodes to define themselves on the TensorFlow graph first, before the parent node creates its own Tensor. That recursion ensures the TensorFlow graph is built in the right order.

Nodes representing random variables also have a method to create a Tensor that calculates their log-density, given their value. Those log-densities are summed on the TensorFlow graph to create a Tensor for the joint log-density of the model. TensorFlow's automatic gradient capabilities are then used to define a Tensor for the gradient of the log-density with respect to each parameter in the model. The `dag` R6 object contained within the model then exposes methods to send parameters to the TensorFlow graph and return the joint density and gradient. These methods are used by the Hamiltonian Monte Carlo algorithm to sample from the model parameters.

Crucially, all nodes ever created in an R session are registered (recorded in a hidden list), whether or not the greta arrays to which they correspond were assigned as objects. That enables us to nest functions and string together operations without losing track of dependency between nodes. It also enables us to define a likelihood via the syntax in the example above: `distribution()` creates a distribution node, sets it as having a fixed value, and registers it, but doesn't assign it to a greta\_array object.

![](README_files/figure-markdown_github/bottom_banner-1.png)
