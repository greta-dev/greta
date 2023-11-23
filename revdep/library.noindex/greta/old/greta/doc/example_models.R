## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(comment = NA,
                      eval = greta:::check_tf_version("message"),
                      cache = TRUE)
library (greta)

## ----linear_greta-------------------------------------------------------------
# variables & priors
int <- normal(0, 10)
coef <- normal(0, 10)
sd <- cauchy(0, 3, truncation = c(0, Inf))

# linear predictor
mu <- int + coef * attitude$complaints

# observation model
distribution(attitude$rating) <- normal(mu, sd)

## ----multiple_linear_data-----------------------------------------------------
data(attitude)
design <- as.matrix(attitude[, 2:7])

## ----multiple_linear_greta----------------------------------------------------
int <- normal(0, 10)
coefs <- normal(0, 10, dim = ncol(design))
sd <- cauchy(0, 3, truncation = c(0, Inf))

# matrix multiplication is more efficient than multiplying the coefficients
# separately
mu <- int + design %*% coefs

distribution(attitude$rating) <- normal(mu, sd)

## ----multiple_linear_warpbreaks_data------------------------------------------
data("warpbreaks")
X <- as_data(model.matrix(breaks ~ wool + tension, warpbreaks))
y <- as_data(warpbreaks$breaks)

## ----multiple_linear_warpbreaks_greta-----------------------------------------
int <- variable()
coefs <- normal(0, 5, dim = ncol(X) - 1)
beta <- c(int, coefs)

eta <- X %*% beta

distribution(y) <- poisson(exp(eta))

## ----multiple_linear_multilogit_data------------------------------------------
data(iris)
X <- as_data(cbind(1, iris[, 1:4]))
y <- model.matrix(~ Species - 1, iris)
P <- ncol(X)
K <- ncol(y)

## ----multiple_linear_multilogit_greta-----------------------------------------
beta <- normal(0, 5, dim = c(P, K - 1))
eta <- X %*% beta
prob <- imultilogit(eta)
distribution(y) <- categorical(prob)

## ----multiple_linear_lasso_data-----------------------------------------------
data(attitude)
design <- as.matrix(attitude[, 2:7])

## ----multiple_linear_lasso_greta----------------------------------------------
int <- normal(0, 10)
sd <- cauchy(0, 3, truncation = c(0, Inf))

tau <- exponential(0.5, dim = ncol(design)) 
coefs <- normal(0, tau)
mu <- int + design %*% coefs

distribution(attitude$rating) <- normal(mu, sd)

## ----hierarchical_linear_greta------------------------------------------------
# linear model parameters
int <- normal(0, 10)
coef <- normal(0, 10)
sd <- cauchy(0, 3, truncation = c(0, Inf))

# hierarchical model for species effect; use the first species as the baseline
# like in lm()
species_sd <- lognormal(0, 1)
species_offset <- normal(0, species_sd, dim = 2)
species_effect <- rbind(0, species_offset)
species_id <- as.numeric(iris$Species)

# model
mu <- int + coef * iris$Sepal.Width + species_effect[species_id]
distribution(iris$Sepal.Length) <- normal(mu, sd)

## ----hierarchical_linear_slopes_greta-----------------------------------------
# linear model parameters
int <- normal(0, 10)
coef <- normal(0, 10)
sd <- cauchy(0, 3, truncation = c(0, Inf))

species_id <- as.numeric(iris$Species)

# random intercepts
species_int_sd <- lognormal(0, 1)
species_int <- normal(0, species_int_sd, dim = 2)
species_int_eff <- rbind(0, species_int)

# random slopes
species_slope_sd <- lognormal(0, 1)
species_slope <- normal(0, species_slope_sd, dim = 2)
species_slope_eff <- rbind(0, species_slope)

# model
mu <- int + coef * iris$Sepal.Width + species_int_eff[species_id] + iris$Sepal.Width * species_slope_eff[species_id]
distribution(iris$Sepal.Length) <- normal(mu, sd)

## ----hierarchical_linear_slopes_corr_greta------------------------------------
# model matrix
modmat <- model.matrix(~ Sepal.Width, iris) 
# index of species
jj <- as.numeric(iris$Species)

M <- ncol(modmat) # number of varying coefficients
N <- max(jj) # number of species

# prior on the standard deviation of the varying coefficient
tau <- exponential(0.5, dim = M)

# prior on the correlation between the varying coefficient
Omega <- lkj_correlation(3, M)

# optimization of the varying coefficient sampling through
# cholesky factorization and whitening
Omega_U <- chol(Omega)
Sigma_U <- sweep(Omega_U, 2, tau, "*")
z <- normal(0, 1, dim = c(N, M)) 
ab <- z %*% Sigma_U # equivalent to: ab ~ multi_normal(0, Sigma_U)

# the linear predictor
mu <- rowSums(ab[jj,] * modmat)

# the residual variance
sigma_e <- cauchy(0, 3, truncation = c(0, Inf))

#model
y <- iris$Sepal.Length
distribution(y) <- normal(mu, sigma_e)

## ----linear_uninformative_greta-----------------------------------------------
# variables & priors
int  <- variable()
coef <- variable()
sd   <- cauchy(0, 3, truncation = c(0, Inf))

# linear predictor
mu <- int + coef * attitude$complaints

# observation model
distribution(attitude$rating) <- normal(mu, sd)

## ----linear_ridge_greta-------------------------------------------------------
# variables & priors
int <- variable()
sd <- cauchy(0, 3, truncation = c(0, Inf))

tau <- inverse_gamma(1, 1)
coef <- normal(0, tau)

# linear predictor
mu <- int + coef * attitude$complaints

# observation model
distribution(attitude$rating) <- normal(mu, sd)

## ----linear_lasso_greta-------------------------------------------------------
# variables & priors
int <- variable()
sd <- inverse_gamma(1, 1)

lambda <- gamma(1, 1)
tau <- exponential(0.5 * lambda**2)
coef <- normal(0, tau)

# linear predictor
mu <- int + coef * attitude$complaints

# observation model
distribution(attitude$rating) <- normal(mu, sd)

## ----linear_horseshoe_greta---------------------------------------------------
horseshoe <- function (tau = 1, dim = NULL) {
  lambda <- cauchy(0, 1, truncation = c(0, Inf), dim = dim)
  sd <- tau ^ 2 * lambda ^ 2
  normal(0, sd, dim = dim)
}

# variables & priors
int <- variable()
sd <- inverse_gamma(1, 1)
coef <- horseshoe()

# linear predictor
mu <- int + coef * attitude$complaints

# observation model
distribution(attitude$rating) <- normal(mu, sd)

## ----linear_finnish_horseshoe_greta-------------------------------------------
regularized_horseshoe <- function (tau = 1,  c = 1, dim = NULL) {
  stopifnot(c > 0)
  lambda <- cauchy(0, 1, truncation = c(0, Inf), dim = dim)
  lambda_tilde <- (c^2 * lambda^2) / (c^2 + tau^2 * lambda^2)
  sd <- tau ^ 2 * lambda_tilde ^ 2
  normal(0, sd, dim = dim)
}

# variables & priors
int <- variable()
sd <- inverse_gamma(1, 1)
coef <- regularized_horseshoe()

# linear predictor
mu <- int + coef * attitude$complaints

# observation model
distribution(attitude$rating) <- normal(mu, sd)

## ----hierarchical_linear_general_greta----------------------------------------
int  <- normal(0, 10)
coef <- normal(0, 10)
sd   <- cauchy(0, 3, truncation = c(0, Inf))

n_species  <- length(unique(iris$Species))
species_id <- as.numeric(iris$Species)

Z <- model.matrix(~ Species + Sepal.Length * Species - 1, data = iris)

gamma_matrix <- multivariate_normal(matrix(0, 1, 2),
                                    diag(2),
                                    n_realisations = 3) 
gamma <- c(gamma_matrix)

wi <- as_data(iris$Sepal.Width)
Z  <- as_data(Z)
mu <- int + coef * wi + Z %*% gamma

distribution(iris$Sepal.Length) <- normal(mu, sd)

## ----hierarchical_linear_marginal_greta---------------------------------------
int  <- variable()
coef <- normal(0, 5)
sd   <- cauchy(0, 3, truncation = c(0, Inf))

n_species  <- length(unique(iris$Species))
species_id <- as.numeric(iris$Species)

Z <- model.matrix(~ Species + Sepal.Length * Species - 1, data = iris)
G  <- zeros(n_species * 2, n_species * 2)

for (s in unique(species_id)) {
  G[c(s, s + n_species), c(s, s + n_species)] <- diag(2)
}

mu <- int + coef * iris$Sepal.Width
V <- zeros(nrow(iris), nrow(iris))
diag(V) <- sd

Z <- as_data(Z)
V <- V + Z %*% G %*% t(Z)

sep <- t(iris$Sepal.Width)
distribution(sep) <- multivariate_normal(t(mu), V)

## ----bayesian_neural_network_data, highlight = FALSE--------------------------
N <- 100
p <- 10

set.seed(23)  
X <- matrix(rnorm(N * p), N)
beta <- rnorm(10)
y <- X %*% beta + rnorm(N, sd = 0.1)

## ----bayesian_neural_network_greta--------------------------------------------
neural_network <- function(x)
{
  # this can be arbitrarily complex, e.g. multiple hidden layers
  x %*% weights
}
  
weights <- normal(0, 1, dim = c(p, 1))
sd <- inverse_gamma(1, 1)

distribution(y) <- normal(neural_network(X), sd)

## ----factor_analysis_data, highlight = FALSE----------------------------------
generate.data <- function(n = 100, p = 5, q = 2, psi = diag(rgamma(p, 1, 1)))
{
  W  <- matrix(rnorm(p * q, 1), p, q)
  Z  <- matrix(rnorm(q * n, 2), q, n)
  WZ <- W %*% Z
  
  X  <- matrix(0, n, p)
  for (i in seq_len(n)) {
    X[i, ] <- MASS::mvrnorm(1, WZ[, i], psi)
  }
  
  list(X = X, W = W, Z = Z, psi = psi)
}

n <- 100
p <- 5
q <- 2
data <- generate.data(n = n, p = p, q = q)
X <- data$X

## ----factor_analysis----------------------------------------------------------
W <- normal(0, 1, dim = c(p, q))
Z <- normal(0, 1, dim = c(q, n))
psi <- zeros(p, p)
diag(psi) <- inverse_gamma(1, 1, dim = p)

distribution(X) <- multivariate_normal(t(W %*% Z), psi)

## ----air_data, highlight = FALSE----------------------------------------------
y <- c(21, 20, 15)
n <- c(48, 34, 21)
Z <- c(10, 30, 50)
alpha <- 4.48
beta <- 0.76
sigma2 <- 81.14
sigma <- sqrt(sigma2)
tau <- 1 / sigma2
J <- 3

## ----air_greta----------------------------------------------------------------
theta <- normal(0, 32, dim = 2)
mu <- alpha + beta * Z
X <- normal(mu, sigma)
p <- ilogit(theta[1] + theta[2] * X)
distribution(y) <- binomial(n, p)

## ----air_stan, echo = FALSE---------------------------------------------------
cat(readLines('https://raw.githubusercontent.com/stan-dev/example-models/master/bugs_examples/vol2/air/air.stan'), sep = '\n')

## ----beetles_data, highlight = FALSE------------------------------------------
x <- c(1.6907, 1.7242, 1.7552, 1.7842, 1.8113, 1.8369, 1.8610, 1.8839)
n <- c(59, 60, 62, 56, 63, 59, 62, 60)
r <- c(6, 13, 18, 28, 52, 53, 61, 60)
N <- 8

## ----beetles_greta------------------------------------------------------------
alpha_star <- normal(0, 32)
beta <- normal(0, 32)
p <- ilogit(alpha_star + beta * (x - mean(x)))
distribution(r) <- binomial(n, p)

alpha <- alpha_star - beta * mean(x)
rhat <- p * n

## ----beetles_stan, echo = FALSE-----------------------------------------------
cat(readLines('https://raw.githubusercontent.com/stan-dev/example-models/master/bugs_examples/vol2/beetles/beetles_logit.stan'), sep = '\n')

## ----lightspeed_data, highlight = FALSE---------------------------------------
y <- c(28, 26, 33, 24, 34, -44, 27, 16, 40, -2, 29, 22, 24, 21, 25, 
       30, 23, 29, 31, 19, 24, 20, 36, 32, 36, 28, 25, 21, 28, 29, 
       37, 25, 28, 26, 30, 32, 36, 26, 30, 22, 36, 23, 27, 27, 28, 
       27, 31, 27, 26, 33, 26, 32, 32, 24, 39, 28, 24, 25, 32, 25, 
       29, 27, 28, 29, 16, 23)
n <- length(y)

## ----lightspeed_greta---------------------------------------------------------
beta  <- variable()
sigma <- variable(lower = 0)

distribution(y) <- normal(beta, sigma)

## ----lightspeed_stan, echo = FALSE--------------------------------------------
cat(readLines('https://raw.githubusercontent.com/stan-dev/example-models/master/ARM/Ch.8/lightspeed.stan'), sep = '\n')

## ----schools_data, highlight = FALSE------------------------------------------
y <- c(28,  8, -3,  7, -1,  1, 18, 12)
sigma_y <- c(15, 10, 16, 11,  9, 11, 10, 18)
N  <- length(y)

## ----schools_greta------------------------------------------------------------
sigma_eta <- inverse_gamma(1, 1)
eta <- normal(0, sigma_eta, dim=N)

mu_theta <- normal(0, 100)
xi <- normal(0, 5)
theta <- mu_theta + xi * eta

distribution(y) <- normal(theta, sigma_y)

## ----schools_stan, echo = FALSE-----------------------------------------------
cat(readLines('https://raw.githubusercontent.com/stan-dev/example-models/master/ARM/Ch.19/schools.stan'), sep = '\n')

## ----data_logistic, highlight = FALSE-----------------------------------------
# make fake data
n_env <- 3
n_sites <- 20

# n_sites x n_env matrix of environmental variables
env <- matrix(rnorm(n_sites * n_env), nrow = n_sites) 
# n_sites observations of species presence or absence
occupancy <- rbinom(n_sites, 1, 0.5) 

## ----logistic_greta-----------------------------------------------------------
alpha <- normal(0, 10)
beta <- normal(0, 10, dim = n_env)

# logit-linear model
linear_predictor <- alpha + env %*% beta
p <- ilogit(linear_predictor)

# distribution (likelihood) over observed values
distribution(occupancy) <- bernoulli(p)

## ----data_poisson, highlight = FALSE------------------------------------------
# make fake data
n_env <- 3
n_sites <- 20

# n_sites x n_env matrix of environmental variables
env <- matrix(rnorm(n_sites * n_env), nrow = n_sites) 
# n_sites observations of species abundance
occupancy <- rpois(n_sites, 5) 

## ----poisson_greta------------------------------------------------------------
alpha <- normal(0, 10)
beta <- normal(0, 10, dim = n_env)
linear_predictor <- alpha + env %*% beta
lambda <- exp(linear_predictor)
distribution(occupancy) <- poisson(lambda)

## ----data_logistic_error_term_greta, highlight = FALSE------------------------
# make fake data
n_env <- 3
n_sites <- 20
n_obs <- 5

# n_sites x n_env matrix of environmental variables
env <- matrix(rnorm(n_sites * n_env), nrow = n_sites) 
# n_sites observations of species presence or absence over n_obs visits
occupancy <- rbinom(n_sites, n_obs, 0.5)

## ----logistic_error_term_greta------------------------------------------------
alpha <- normal(0, 10)
beta <- normal(0, 10, dim = n_env)
error <- normal(0, 10, dim = n_sites)

# logit-linear model with extra variation
linear_predictor <- alpha + env %*% beta + error
p <- ilogit(linear_predictor)

# distribution (likelihood) over observed values
distribution(occupancy) <- binomial(n_obs, p)

## ----data_multispecies_bernoulli, highlight = FALSE---------------------------
# make fake data
n_species <- 5
n_env <- 3
n_sites <- 20

env <- matrix(rnorm(n_sites * n_env), nrow = n_sites)
occupancy <- matrix(rbinom(n_species * n_sites, 1, 0.5), nrow = n_sites)

## ----multispecies_bernoulli_greta---------------------------------------------
alpha <- normal(0, 10, dim = n_species)
beta <- normal(0, 10, dim = c(n_env, n_species))

env_effect <- env %*% beta

# add intercepts for all species
linear_predictor <- sweep(env_effect, 2, alpha, FUN = '+')

# ilogit of linear predictor
p <- ilogit(linear_predictor)

# a single observation means our data are bernoulli distributed
distribution(occupancy) <- bernoulli(p)

## ----data_multispecies_partially_pool, highlight = FALSE----------------------
# make fake data
n_species <- 5
n_env <- 1
n_sites <- 50

env <- matrix(rnorm(n_sites * n_env), nrow = n_sites)
occupancy <- matrix(rbinom(n_sites * n_species, 1, 0.5), nrow = n_sites)

## ----multispecies_partially_pool_greta----------------------------------------
global_alpha <- normal(0, 10, dim = 1)
global_alpha_sd <- uniform(0, 10, dim = 1) 
alpha <- normal(global_alpha, global_alpha_sd, dim = n_species)

global_betas <- normal(0, 10, dim = n_env)
global_betas_sd <- uniform(0, 10, dim = n_env)
beta <- normal(global_betas, global_betas_sd, dim = c(n_env, n_species))

env_effect <- env %*% beta

# add intercepts for all species
linear_predictor <- sweep(env_effect, 2, alpha, FUN = '+')

# ilogit of linear predictor
p <- ilogit(linear_predictor)

distribution(occupancy) <- bernoulli(p)

## ----data_multilevel, highlight = FALSE---------------------------------------
# make fake data
n_species <- 3
n_env <- 1
n_sites <- 5
n_traits <- 1

# n_sites x n_env matrix of environmental variables
env <- matrix(rnorm(n_sites * n_env), nrow = n_sites)
# n_species * n_traits matix of trait variables
traits <- matrix(rnorm(n_species * n_traits), nrow = n_species)
# n_sites * n_species matrix of observed occupancy
occupancy <- matrix(rbinom(n_sites * n_species, 1, 0.5), nrow = n_sites)

## ----multilevel_greta---------------------------------------------------------
# include a column of 1's for intercept estimation in the sub-model (traits) and base model
traits <- cbind(rep(1, n_species), traits)
env <- cbind(rep(1, n_sites), env)

# redefine n_env and n_traits after adding columns for intercepts
n_env <- ncol(env)
n_traits <- ncol(traits)

# sub-model parameters have normal prior distributions
g <- normal(0, 10, dim = c(n_env, n_traits))
# parameters of the base model are a function of the parameters of the sub-model
beta <-  g %*% t(traits) 

# use the coefficients to get the model linear predictor
linear_predictor <- env %*% beta 

# use the logit link to get probabilities of occupancy
p <- ilogit(linear_predictor)

# data are bernoulli distributed
distribution(occupancy) <- bernoulli(p)

## ----cjs_data, highlight = FALSE----------------------------------------------
n_obs <- 100
n_time <- 20
y <- matrix(sample(c(0, 1), size = (n_obs * n_time), replace = TRUE),
            ncol = n_time)

## ----cjs_greta----------------------------------------------------------------
# data summaries
first_obs <- apply(y, 1, function(x) min(which(x > 0)))
final_obs <- apply(y, 1, function(x) max(which(x > 0)))
obs_id <- apply(y, 1, function(x) seq(min(which(x > 0)), max(which(x > 0)), by = 1)[-1])
obs_id <- unlist(obs_id)
capture_vec <- apply(y, 1, function(x) x[min(which(x > 0)):max(which(x > 0))][-1])
capture_vec <- unlist(capture_vec)

# priors
phi <- beta(1, 1, dim = n_time)
p <- beta(1, 1, dim = n_time)

# derived parameter
chi <- ones(n_time)
for (i in seq_len(n_time - 1)) {
  tn <- n_time - i
  chi[tn] <- (1 - phi[tn]) + phi[tn] * (1 - p[tn + 1]) * chi[tn + 1]
}

# dummy variables
alive_data <- ones(length(obs_id))            # definitely alive
not_seen_last <- final_obs != 20              # ignore observations in last timestep
final_observation <- ones(sum(not_seen_last)) # final observation

# set likelihoods
distribution(alive_data) <- bernoulli(phi[obs_id - 1])
distribution(capture_vec) <- bernoulli(p[obs_id])
distribution(final_observation) <- bernoulli(chi[final_obs[not_seen_last]])

## ----cjs_stan, echo = FALSE---------------------------------------------------
cat(readLines('https://raw.githubusercontent.com/stan-dev/example-models/master/misc/ecology/mark-recapture/cjs-K.stan'), sep = '\n')

