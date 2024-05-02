pkgname <- "greta.gp"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('greta.gp')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("gp")
### * gp

flush(stderr()); flush(stdout())

### Name: gp
### Title: Define a Gaussian process
### Aliases: gp project

### ** Examples

## Not run: 
##D # build a kernel function on two dimensions
##D k1 <- rbf(lengthscales = c(0.1, 0.2), variance = 0.6)
##D k2 <- bias(variance = lognormal(0, 1))
##D K <- k1 + k2
##D 
##D # use this kernel in a full-rank Gaussian process
##D f <- gp(1:10, K)
##D 
##D # or in sparse Gaussian process
##D f_sparse <- gp(1:10, K, inducing = c(2, 5, 8))
##D 
##D # project the values of the GP to new coordinates
##D f_new <- project(f, 11:15)
##D 
##D # or project with a different kernel (e.g. a sub-kernel)
##D f_new_bias <- project(f, 11:15, k2)
## End(Not run)



cleanEx()
nameEx("kernels")
### * kernels

flush(stderr()); flush(stdout())

### Name: kernels
### Title: Gaussian process kernels
### Aliases: kernels bias constant white iid rbf rational_quadratic linear
###   polynomial expo mat12 mat32 mat52 cosine periodic

### ** Examples

## Not run: 
##D # create a radial basis function kernel on two dimensions
##D k1 <- rbf(lengthscales = c(0.1, 0.2), variance = 0.6)
##D 
##D # evaluate it on a greta array to get the variance-covariance matrix
##D x <- greta_array(rnorm(8), dim = c(4, 2))
##D k1(x)
##D 
##D # non-symmetric covariance between two sets of points
##D x2 <- greta_array(rnorm(10), dim = c(5, 2))
##D k1(x, x2)
##D 
##D # create a bias kernel, with the variance as a variable
##D k2 <- bias(variance = lognormal(0, 1))
##D 
##D # combine two kernels and evaluate
##D K <- k1 + k2
##D K(x, x2)
##D 
##D # other kernels
##D constant(variance = lognormal(0, 1))
##D white(variance = lognormal(0, 1))
##D iid(variance = lognormal(0,1))
##D rational_quadratic(lengthscales = c(0.1, 0.2), alpha = 0.5, variance = 0.6)
##D linear(variances = 0.1)
##D polynomial(variances = 0.6, offset = 0.8, degree = 2)
##D expo(lengthscales = 0.6 ,variance = 0.9)
##D mat12(lengthscales = 0.5, variance = 0.7)
##D mat32(lengthscales = 0.4, variance = 0.8)
##D mat52(lengthscales = 0.3, variance = 0.9)
##D cosine(lengthscales = 0.68, variance = 0.8)
##D periodic(period = 0.71, lengthscale = 0.59, variance = 0.2)
## End(Not run)



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
