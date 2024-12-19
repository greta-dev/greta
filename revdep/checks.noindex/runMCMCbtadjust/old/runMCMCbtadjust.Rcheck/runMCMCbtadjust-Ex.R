pkgname <- "runMCMCbtadjust"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('runMCMCbtadjust')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("findMCMC_strong_corrs")
### * findMCMC_strong_corrs

flush(stderr()); flush(stdout())

### Name: findMCMC_strong_corrs
### Title: findMCMC_strong_corrs
### Aliases: findMCMC_strong_corrs

### ** Examples

## Not run: 
##D #generating data
##D set.seed(1)
##D y1000<-rnorm(n=1000,mean=600,sd=30)
##D ModelData <-list(mass = y1000,nobs = length(y1000))
##D 
##D #writing the Jags code as a character chain in R
##D modeltotransfer<-"model {
##D 
##D # Priors
##D population.mean ~ dunif(0,5000)
##D population.sd ~ dunif(0,100)
##D 
##D # Precision = 1/variance: Normal distribution parameterized by precision in Jags
##D population.variance <- population.sd * population.sd
##D precision <- 1 / population.variance
##D 
##D # Likelihood
##D for(i in 1:nobs){
##D   mass[i] ~ dnorm(population.mean, precision)
##D  }
##D  }"
##D 
##D #specifying the initial values
##D ModelInits <- function()
##D {list (population.mean = rnorm(1,600,90), population.sd = runif(1, 1, 30))}
##D params <- c("population.mean", "population.sd", "population.variance")
##D K<-3
##D set.seed(1)
##D Inits<-lapply(1:K,function(x){ModelInits()})
##D 
##D # running runMCMC_btadjust with MCMC_language="Jags":
##D set.seed(1)
##D out.mcmc.Coda<-runMCMC_btadjust(MCMC_language="Jags", code=modeltotransfer,
##D data=ModelData,
##D Nchains=K, params=params, inits=Inits,
##D niter.min=1000, niter.max=300000,
##D nburnin.min=100, nburnin.max=200000,
##D thin.min=1, thin.max=1000,
##D neff.min=1000, conv.max=1.05,
##D control=list(print.diagnostics=TRUE, neff.method="Coda"))
##D 
##D findMCMC_strong_corrs(out.mcmc.Coda)
## End(Not run)



cleanEx()
nameEx("runMCMC_btadjust")
### * runMCMC_btadjust

flush(stderr()); flush(stdout())

### Name: runMCMC_btadjust
### Title: runMCMC_btadjust
### Aliases: runMCMC_btadjust

### ** Examples

# for examples with Nimble or Greta, see the Presentation Vignette.
## Not run: 
##D #generating data
##D set.seed(1)
##D y1000<-rnorm(n=1000,mean=600,sd=30)
##D ModelData <-list(mass = y1000,nobs = length(y1000))
##D 
##D #writing the Jags code as a character chain in R
##D modeltotransfer<-"model {
##D 
##D # Priors
##D population.mean ~ dunif(0,5000)
##D population.sd ~ dunif(0,100)
##D 
##D # Precision = 1/variance: Normal distribution parameterized by precision in Jags
##D population.variance <- population.sd * population.sd
##D precision <- 1 / population.variance
##D 
##D # Likelihood
##D for(i in 1:nobs){
##D   mass[i] ~ dnorm(population.mean, precision)
##D  }
##D  }"
##D 
##D #specifying the initial values
##D ModelInits <- function()
##D {list (population.mean = rnorm(1,600,90), population.sd = runif(1, 1, 30))}
##D params <- c("population.mean", "population.sd", "population.variance")
##D K<-3
##D set.seed(1)
##D Inits<-lapply(1:K,function(x){ModelInits()})
##D 
##D # running runMCMC_btadjust with MCMC_language="Jags":
##D set.seed(1)
##D out.mcmc.Coda<-runMCMC_btadjust(MCMC_language="Jags", code=modeltotransfer,
##D data=ModelData,
##D Nchains=K, params=params, inits=Inits,
##D niter.min=1000, niter.max=300000,
##D nburnin.min=100, nburnin.max=200000,
##D thin.min=1, thin.max=1000,
##D neff.min=1000, conv.max=1.05,
##D control=list(print.diagnostics=TRUE, neff.method="Coda"))
##D 
##D summary(out.mcmc.Coda)
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
