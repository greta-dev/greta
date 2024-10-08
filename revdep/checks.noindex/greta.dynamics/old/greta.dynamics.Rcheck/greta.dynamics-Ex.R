pkgname <- "greta.dynamics"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('greta.dynamics')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("iterate_matrix")
### * iterate_matrix

flush(stderr()); flush(stdout())

### Name: iterate_matrix
### Title: iterate transition matrices
### Aliases: iterate_matrix

### ** Examples

## Not run: 
##D # simulate from a probabilistic 4-stage transition matrix model
##D k <- 4
##D 
##D # component variables
##D # survival probability for all stages
##D survival <- uniform(0, 1, dim = k)
##D # conditional (on survival) probability of staying in a stage
##D stasis <- c(uniform(0, 1, dim = k - 1), 1)
##D # marginal probability of staying/progressing
##D stay <- survival * stasis
##D progress <- (survival * (1 - stay))[1:(k - 1)]
##D # recruitment rate for the largest two stages
##D recruit <- exponential(c(3, 5))
##D 
##D # combine into a matrix:
##D tmat <- zeros(k, k)
##D diag(tmat) <- stay
##D progress_idx <- row(tmat) - col(tmat) == 1
##D tmat[progress_idx] <- progress
##D tmat[1, k - (1:0)] <- recruit
##D 
##D # analyse this to get the intrinsic growth rate and stable state
##D iterations <- iterate_matrix(tmat)
##D iterations$lambda
##D iterations$stable_distribution
##D iterations$all_states
##D 
##D # Can also do this simultaneously for a collection of transition matrices
##D k <- 2
##D n <- 10
##D survival <- uniform(0, 1, dim = c(n, k))
##D stasis <- cbind(uniform(0, 1, dim = n), rep(1, n))
##D stay <- survival * stasis
##D progress <- (survival * (1 - stasis))[, 1]
##D recruit_rate <- 1 / seq(0.1, 5, length.out = n)
##D recruit <- exponential(recruit_rate, dim = n)
##D tmats <- zeros(10, 2, 2)
##D tmats[, 1, 1] <- stasis[, 1]
##D tmats[, 2, 2] <- stasis[, 2]
##D tmats[, 2, 1] <- progress
##D tmats[, 1, 2] <- recruit
##D 
##D iterations <- iterate_matrix(tmats)
##D iterations$lambda
##D iterations$stable_distribution
##D iterations$all_states
## End(Not run)



cleanEx()
nameEx("ode_solve")
### * ode_solve

flush(stderr()); flush(stdout())

### Name: ode_solve
### Title: solve ODEs
### Aliases: ode_solve

### ** Examples

## Not run: 
##D # replicate the Lotka-Volterra example from deSolve
##D library(deSolve)
##D LVmod <- function(Time, State, Pars) {
##D   with(as.list(c(State, Pars)), {
##D     Ingestion <- rIng * Prey * Predator
##D     GrowthPrey <- rGrow * Prey * (1 - Prey / K)
##D     MortPredator <- rMort * Predator
##D 
##D     dPrey <- GrowthPrey - Ingestion
##D     dPredator <- Ingestion * assEff - MortPredator
##D 
##D     return(list(c(dPrey, dPredator)))
##D   })
##D }
##D 
##D pars <- c(
##D   rIng = 0.2, # /day, rate of ingestion
##D   rGrow = 1.0, # /day, growth rate of prey
##D   rMort = 0.2, # /day, mortality rate of predator
##D   assEff = 0.5, # -, assimilation efficiency
##D   K = 10
##D ) # mmol/m3, carrying capacity
##D 
##D yini <- c(Prey = 1, Predator = 2)
##D times <- seq(0, 30, by = 1)
##D out <- ode(yini, times, LVmod, pars)
##D 
##D # simulate observations
##D jitter <- rnorm(2 * length(times), 0, 0.1)
##D y_obs <- out[, -1] + matrix(jitter, ncol = 2)
##D 
##D # ~~~~~~~~~
##D # fit a greta model to infer the parameters from this simulated data
##D 
##D # greta version of the function
##D lotka_volterra <- function(y, t, rIng, rGrow, rMort, assEff, K) {
##D   Prey <- y[1, 1]
##D   Predator <- y[1, 2]
##D 
##D   Ingestion <- rIng * Prey * Predator
##D   GrowthPrey <- rGrow * Prey * (1 - Prey / K)
##D   MortPredator <- rMort * Predator
##D 
##D   dPrey <- GrowthPrey - Ingestion
##D   dPredator <- Ingestion * assEff - MortPredator
##D 
##D   cbind(dPrey, dPredator)
##D }
##D 
##D # priors for the parameters
##D rIng <- uniform(0, 2) # /day, rate of ingestion
##D rGrow <- uniform(0, 3) # /day, growth rate of prey
##D rMort <- uniform(0, 1) # /day, mortality rate of predator
##D assEff <- uniform(0, 1) # -, assimilation efficiency
##D K <- uniform(0, 30) # mmol/m3, carrying capacity
##D 
##D # initial values and observation error
##D y0 <- uniform(0, 5, dim = c(1, 2))
##D obs_sd <- uniform(0, 1)
##D 
##D # solution to the ODE
##D y <- ode_solve(lotka_volterra, y0, times, rIng, rGrow, rMort, assEff, K)
##D 
##D # sampling statement/observation model
##D distribution(y_obs) <- normal(y, obs_sd)
##D 
##D # we can use greta to solve directly, for a fixed set of parameters (the true
##D # ones in this case)
##D values <- c(
##D   list(y0 = t(1:2)),
##D   as.list(pars)
##D )
##D vals <- calculate(y, values = values)[[1]]
##D plot(vals[, 1] ~ times, type = "l", ylim = range(vals))
##D lines(vals[, 2] ~ times, lty = 2)
##D points(y_obs[, 1] ~ times)
##D points(y_obs[, 2] ~ times, pch = 2)
##D 
##D # or we can do inference on the parameters:
##D 
##D # build the model (takes a few seconds to define the tensorflow graph)
##D m <- model(rIng, rGrow, rMort, assEff, K, obs_sd)
##D 
##D # compute MAP estimate
##D o <- opt(m)
##D o
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
