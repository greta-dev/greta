context("tunMCMC_btadjust")

### minimal setup for running runMCMC_btadjust: see vignette
set.seed(1)
y1000<-rnorm(n=1000,mean=600,sd=30)

if (length(find.package("nimble", quiet=TRUE, verbose=FALSE))>0) {library(nimble); nimble_installed<-TRUE} else {nimble_installed<-FALSE}

#first condition added to prevent these tests being done on CRAN and outisde devtools: see: https://stackoverflow.com/questions/36166288/skip-tests-on-cran-but-run-locally
if(identical(Sys.getenv("NOT_CRAN"), "true")) {if (length(find.package("rjags", quiet=TRUE, verbose=FALSE))>0) {library(rjags); if (length(jags.version())==0) {rjags_installed<-FALSE} else {if (substring(jags.version(),first=1,last=1)!="4") {rjags_installed<-FALSE} else {rjags_installed<-TRUE}}} else {rjags_installed<-FALSE}} else {rjags_installed<-FALSE}

################
#not done with greta as loading greta gives error outside testthat.
################

if (nimble_installed)
  {ModelData <-list(mass = y1000)
ModelConsts <- list(nobs = length(y1000))
ModelCode<-nimbleCode(
  {
    # Priors
    population.mean ~ dunif(0,5000)
    population.sd ~ dunif(0,100)

    # Precision = 1/variance
    population.variance <- population.sd * population.sd

    # Normal parameterized by precision
    precision <- 1 / population.variance

    # Likelihood
    for(i in 1:nobs){
      mass[i] ~ dnorm(population.mean, precision)
    }
  })
}

if (rjags_installed)
{
ModelData.Jags <-list(mass = y1000, nobs = length(y1000))

modeltotransfer<-"model {

		# Priors
			population.mean ~ dunif(0,5000)
			population.sd ~ dunif(0,100)

			# Precision = 1/variance
			population.variance <- population.sd * population.sd

			# Normal parameterized by precision
			precision <- 1 / population.variance

			# Likelihood
			for(i in 1:nobs){
			  mass[i] ~ dnorm(population.mean, precision)
			}
		}"

}

ModelInits <- function()
{list (population.mean = rnorm(1,600,90), population.sd = runif(1, 1, 30))}

Nchains <- 3

set.seed(1)
Inits<-lapply(1:Nchains,function(x){ModelInits()})


ModelInitsbis <- function()
{list (population.mean = rnorm(1,600,90), population.sd = runif(1, -1, 0))}

Nchains <- 3

set.seed(1)
Initsbis<-lapply(1:Nchains,function(x){ModelInitsbis()})

#specifying the names of parameters to analyse and save:
params <- c("population.mean", "population.sd")



if (nimble_installed){
testthat::test_that("errors", {
##

  #should give  an error because time.max & niter.max are not specified at the same time
  testthat::expect_error((runMCMC_btadjust(data=ModelData,constants=ModelConsts,code=ModelCode,MCMC_language="Nimble",inits=Inits,params=params,neff.min=150,conv.max=1.05,Nchains=2)))

   #should give  an error because Nchains=1 & control$convtype="Gelman"
  testthat::expect_error((runMCMC_btadjust(data=ModelData,constants=ModelConsts,code=ModelCode,MCMC_language="Nimble",
                                          inits=Inits,params=params,neff.min=150,conv.max=1.05,Nchains=1,niter.max=2000,control=list(convtype="Gelman"))))

  #should give  an error because Nchains=1 & control$convtype="Gelman_new"
  testthat::expect_error((runMCMC_btadjust(data=ModelData,constants=ModelConsts,code=ModelCode,MCMC_language="Nimble",
                                          inits=Inits,params=params,neff.min=150,conv.max=1.05,Nchains=1,niter.max=2000,control=list(convtype="Gelman_new"))))

  #should give  an error because control has an unidentified component
  testthat::expect_error((runMCMC_btadjust(data=ModelData,constants=ModelConsts,code=ModelCode,MCMC_language="Nimble",
                                          inits=Inits,params=params,neff.min=150,conv.max=1.05,Nchains=2,niter.max=2000,control=list(toto=0))))

  #should give  an error because props.conv in control has negative values
  testthat::expect_error((runMCMC_btadjust(data=ModelData,constants=ModelConsts,code=ModelCode,MCMC_language="Nimble",
                                          inits=Inits,params=params,neff.min=150,conv.max=1.05,Nchains=2,niter.max=2000,control=list(props.conv=-1))))

  #should give  an error because props.conv in control has values >1
  testthat::expect_error((runMCMC_btadjust(data=ModelData,constants=ModelConsts,code=ModelCode,MCMC_language="Nimble",
                                          inits=Inits,params=params,neff.min=150,conv.max=1.05,Nchains=2,niter.max=2000,control=list(props.conv=c(0.5,2)))))

  #should give an error because safemultiplier.Nvals is not numeric
  testthat::expect_error((runMCMC_btadjust(data=ModelData,constants=ModelConsts,code=ModelCode,MCMC_language="Nimble",
                                          inits=Inits,params=params,neff.min=150,conv.max=1.05,Nchains=2,niter.max=2000,control=list(safemultiplier.Nvals="toto"))))

  #should give an error because time.max is NULL & niter.max is not finite
  testthat::expect_error((runMCMC_btadjust(data=ModelData,constants=ModelConsts,code=ModelCode,MCMC_language="Nimble",
                                           inits=Inits,params=params,neff.min=150,conv.max=1.05,Nchains=2,niter.max=Inf, time.max=NULL)))

  #should give an error because if Nchains>1, convtype!= "Gelman" or convtype!= "Gelman_new" or if Nchains==1, convtype!= "Geweke" or convType!="Heidleberger"
  testthat::expect_error((runMCMC_btadjust(data=ModelData,constants=ModelConsts,code=ModelCode,MCMC_language="Nimble",
                                           inits=Inits,params=params,neff.min=150,conv.max=1.05,Nchains=2,niter.max=2000,control=list(convType="Geweke"))))
 testthat::expect_error((runMCMC_btadjust(data=ModelData,constants=ModelConsts,code=ModelCode,MCMC_language="Nimble",
                                           inits=Inits,params=params,neff.min=150,conv.max=1.05,Nchains=2,niter.max=2000,control=list(convType="Heidleberger"))))
  testthat::expect_error((runMCMC_btadjust(data=ModelData,constants=ModelConsts,code=ModelCode,MCMC_language="Nimble",
                                           inits=Inits,params=params,neff.min=150,conv.max=1.05,Nchains=1,niter.max=2000,control=list(convType="Gelman"))))
  testthat::expect_error((runMCMC_btadjust(data=ModelData,constants=ModelConsts,code=ModelCode,MCMC_language="Nimble",
                                           inits=Inits,params=params,neff.min=150,conv.max=1.05,Nchains=1,niter.max=2000,control=list(convType="Gelman_new"))))
  testthat::expect_error((runMCMC_btadjust(data=ModelData,constants=ModelConsts,code=ModelCode,MCMC_language="Nimble",
                                           inits=Inits,params=params,neff.min=150,conv.max=1.05,Nchains=2,niter.max=2000,control=list(convType="toto"))))

#should give an error because control$convtype.alpha is inadequate
  testthat::expect_error((runMCMC_btadjust(data=ModelData,constants=ModelConsts,code=ModelCode,MCMC_language="Nimble",
                                           inits=Inits,params=params,neff.min=150,conv.max=1.05,Nchains=2,niter.max=2000,control=list(convType="Gelman",convtype.alpha=c(0.1,0.3)))))
  testthat::expect_error((runMCMC_btadjust(data=ModelData,constants=ModelConsts,code=ModelCode,MCMC_language="Nimble",
                                           inits=Inits,params=params,neff.min=150,conv.max=1.05,Nchains=2,niter.max=2000,control=list(convType="Gelman",convtype.alpha=NULL))))
  testthat::expect_error((runMCMC_btadjust(data=ModelData,constants=ModelConsts,code=ModelCode,MCMC_language="Nimble",
                                           inits=Inits,params=params,neff.min=150,conv.max=1.05,Nchains=2,niter.max=2000,control=list(convType="Gelman",convtype.alpha=1.1))))
 

  #should give an error because neff.method is not "Stan" or "Coda"
  testthat::expect_error((runMCMC_btadjust(data=ModelData,constants=ModelConsts,code=ModelCode,MCMC_language="Nimble",
                                           inits=Inits,params=params,neff.min=150,conv.max=1.05,Nchains=2,niter.max=2000,control=list(neff.method="toto"))))

  #should give an error because MCMC_language is not among "Nimble","Jags" or"Greta"
  testthat::expect_error((runMCMC_btadjust(data=ModelData,constants=ModelConsts,code=ModelCode,MCMC_language="toto",
                                           inits=Inits,params=params,neff.min=150,conv.max=1.05,Nchains=2,niter.max=2000)))

  #should give an error because constants is not provided or is null with MCMC_language="Nimble"
  testthat::expect_error((runMCMC_btadjust(data=ModelData,constants=NULL,code=ModelCode,MCMC_language="Nimble",
                                           inits=Inits,params=params,neff.min=150,conv.max=1.05,Nchains=2,niter.max=2000)))

  #should give an error because code is not provided or is null with MCMC_language="Nimble"
  testthat::expect_error((runMCMC_btadjust(data=ModelData,constants=ModelConsts,code=NULL,MCMC_language="Nimble",
                                           inits=Inits,params=params,neff.min=150,conv.max=1.05,Nchains=2,niter.max=2000)))

  #should give an error because data is not provided or is null with MCMC_language="Nimble"
  testthat::expect_error((runMCMC_btadjust(data=NULL,constants=ModelConsts,code=ModelCode,MCMC_language="Nimble",
                                           inits=Inits,params=params,neff.min=150,conv.max=1.05,Nchains=2,niter.max=2000)))


  #should give an error because data is not provided or is null with MCMC_language="Jags"
  testthat::expect_error((runMCMC_btadjust(data=NULL,code=ModelCode,MCMC_language="Nimble",
                                           inits=Inits,params=params,neff.min=150,conv.max=1.05,Nchains=2,niter.max=2000)))

  #should give an error because params is not provided
  testthat::expect_error((runMCMC_btadjust(data=ModelData,constants=ModelConsts,code=ModelCode,MCMC_language="Nimble",
                                           inits=Inits,neff.min=150,conv.max=1.05,Nchains=2,niter.max=2000)))


  })}




if (rjags_installed){
testthat::test_that("errors", {
##
  #should give an error because code is not provided or is null with MCMC_language="Jags"
  testthat::expect_error((runMCMC_btadjust(data=ModelData.Jags,code=NULL,MCMC_language="Jags",
                                           inits=Inits,params=params,neff.min=150,conv.max=1.05,Nchains=2,niter.max=2000)))

  #should give an error because code is not provided or is null with MCMC_language="Jags"
  testthat::expect_error((runMCMC_btadjust(data=ModelData.Jags,MCMC_language="Jags",
                                           inits=Inits,params=params,neff.min=150,conv.max=1.05,Nchains=2,niter.max=2000)))

 
  #should give an error because code is not of type character with MCMC_language="Jags"
  testthat::expect_error((runMCMC_btadjust(data=ModelData.Jags,code=1,MCMC_language="Jags",
                                           inits=Inits,params=params,neff.min=150,conv.max=1.05,Nchains=2,niter.max=2000)))

  #should give an error because Inits should give bad values for all chains is not provided
  testthat::expect_error((runMCMC_btadjust(data=ModelData.Jags,code=modeltotransfer,MCMC_language="Jags",
                                           inits=Initsbis,params=params,neff.min=150,conv.max=1.05,Nchains=2,niter.max=2000)))



  })}
