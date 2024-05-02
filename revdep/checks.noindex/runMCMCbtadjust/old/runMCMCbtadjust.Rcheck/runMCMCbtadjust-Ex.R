pkgname <- "runMCMCbtadjust"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('runMCMCbtadjust')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("runMCMC_btadjust")
### * runMCMC_btadjust

flush(stderr()); flush(stdout())

### Name: runMCMC_btadjust
### Title: runMCMC_btadjust
### Aliases: runMCMC_btadjust

### ** Examples

 #\code{
# for examples with Nimble or Greta, see the Vignette.
# condition variable of whether installation is OK with Jags to avoid error durong package check
condition_jags<-TRUE
if (nchar(system.file(package='rjags'))==0) {condition_jags<-FALSE}
if (nchar(system.file(package='runjags'))==0) {condition_jags<-FALSE}
if (condition_jags)
{suppressWarnings(temp<-runjags::testjags(silent=TRUE))
 if(!(temp$JAGS.available&temp$JAGS.found&temp$JAGS.major==4)) {condition_jags<-FALSE}}

if (condition_jags) {
#generating data
set.seed(1)
y1000<-rnorm(n=1000,mean=600,sd=30)
ModelData <-list(mass = y1000,nobs = length(y1000))

#writing the Jags code as a character chain in R
modeltotransfer<-"model {

# Priors
population.mean ~ dunif(0,5000)
population.sd ~ dunif(0,100)

# Precision = 1/variance: Normal distribution parameterized by precision in Jags
population.variance <- population.sd * population.sd
precision <- 1 / population.variance

# Likelihood
for(i in 1:nobs){
  mass[i] ~ dnorm(population.mean, precision)
 }
 }"

#specifying the initial values
ModelInits <- function()
{list (population.mean = rnorm(1,600,90), population.sd = runif(1, 1, 30))}
params <- c("population.mean", "population.sd", "population.variance")
K<-3
set.seed(1)
Inits<-lapply(1:K,function(x){ModelInits()})

# running runMCMC_btadjust with MCMC_language="Jags":
set.seed(1)
out.mcmc.Coda<-runMCMC_btadjust(MCMC_language="Jags", code=modeltotransfer,
data=ModelData,
Nchains=K, params=params, inits=Inits,
niter.min=1000, niter.max=300000,
nburnin.min=100, nburnin.max=200000,
thin.min=1, thin.max=1000,
neff.min=1000, conv.max=1.05,
control=list(print.diagnostics=TRUE, neff.method="Coda"))

summary(out.mcmc.Coda)
}
#}



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
