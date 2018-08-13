## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(comment = NA,
                      eval = greta:::check_tf_version("message"),
                      cache = TRUE)
library (greta)

## ----multiple_linear_warpbreaks_data-------------------------------------
data("warpbreaks")
head(warpbreaks)

X <- as_data(model.matrix(breaks ~ wool + tension, warpbreaks))
y <- as_data(warpbreaks$breaks)

## ----multiple_linear_warpbreaks_greta------------------------------------
int <- variable()
coefs <- normal(0, 5, dim = ncol(X) - 1)
beta <- c(int, coefs)

eta <- X %*% beta

distribution(y) <- poisson(exp(eta))

## ----multiple_linear_multilogit_data-------------------------------------
data(iris)

# Only choose a small subset until categorical takes matrix arguments
idxs <- c(1:10, 51:60, 101:111)
n <- length(idxs)

X <- as_data(cbind(1, iris[idxs,1:4]))
y <- model.matrix(~Species, iris[idxs,])
P <- ncol(X)
K <- ncol(y)

## ----multiple_linear_multilogit_greta------------------------------------
beta <- normal(0, 5, dim = c(P, K - 1))
eta <- X %*% beta
prob <- imultilogit(eta)

for (i in seq_len(n)) {
  yi <- t(y[i, ])
  distribution(yi) <- categorical(t(prob[i, ]))
}

## ----multiple_linear_lasso_greta-----------------------------------------
# the predictors as a matrix
design <- as.matrix(attitude[, 2:7])

int   <- normal(0, 10)
sd <- cauchy(0, 3, truncation = c(0, Inf))

tau <- exponential(0.5, dim = ncol(design)) 
coefs <- normal(0, tau)
mu <- int + design %*% coefs

distribution(attitude$rating) <- normal(mu, sd)

## ----hierarchical_linear_slopes_greta------------------------------------
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

## ----linear_ridge_greta--------------------------------------------------
# variables & priors
int  <- variable()
sd   <- cauchy(0, 3, truncation = c(0, Inf))

tau  <- inverse_gamma(1, 1)
coef <- normal(0, tau)

# linear predictor
mu <- int + coef * attitude$complaints

# observation model
distribution(attitude$rating) <- normal(mu, sd)

## ----linear_horseshoe_greta----------------------------------------------
horseshoe <- function (tau = 1, dim = NULL) {
  lambda <- cauchy(0, 1, truncation = c(0, Inf), dim = dim)
  sd <- tau ^ 2 * lambda ^ 2
  normal(0, sd, dim = dim)
}

# variables & priors
int  <- variable()
sd   <- inverse_gamma(1, 1)
coef <- horseshoe()

# linear predictor
mu <- int + coef * attitude$complaints

# observation model
distribution(attitude$rating) <- normal(mu, sd)

## ----linear_finnish_horseshoe_greta--------------------------------------
regularized_horseshoe <- function (tau = 1,  c = 1, dim = NULL) {
  stopifnot(c > 0)
  lambda <- cauchy(0, 1, truncation = c(0, Inf), dim = dim)
  lambda_tilde <- (c^2 * lambda^2) / (c^2 + tau^2 * lambda^2)
  sd <- tau ^ 2 * lambda_tilde ^ 2
  normal(0, sd, dim = dim)
}

# variables & priors
int  <- variable()
sd   <- inverse_gamma(1, 1)
coef <- regularized_horseshoe()

# linear predictor
mu <- int + coef * attitude$complaints

# observation model
distribution(attitude$rating) <- normal(mu, sd)

## ----hierarchical_linear_general_greta-----------------------------------
int  <- normal(0, 10)
coef <- normal(0, 10)
sd   <- cauchy(0, 3, truncation = c(0, Inf))

n_species  <- length(unique(iris$Species))
species_id <- as.numeric(iris$Species)

Z <- model.matrix(~ Species + Sepal.Length * Species - 1, data = iris)
gamma <- zeros(n_species * 2)

for (s in unique(species_id)) {
  gamma_sd <- diag(2)
  gamma[c(s, s + n_species)] <- multivariate_normal(rep(0, 2), gamma_sd) 
}

wi <- as_data(iris$Sepal.Width)
Z  <- as_data(Z)
mu <- int + coef * wi + Z %*% gamma

distribution(iris$Sepal.Length) <- normal(mu, sd)

## ----hierarchical_linear_marginal_greta----------------------------------
int  <- variable()
coef <- normal(0, 5)
sd   <- cauchy(0, 3, truncation = c(0, Inf))

n_species  <- length(unique(iris$Species))
species_id <- as.numeric(iris$Species)

Z <- model.matrix(~ Species + Sepal.Length * Species - 1, data = iris)
G  <- zeros(n_species * 2, n_species * 2)

for (s in unique(species_id)) {
  G_sd <- diag(2)
  G[c(s, s + n_species), c(s, s + n_species)] <- G_sd
}

mu <- int + coef * iris$Sepal.Width
V <- zeros(nrow(iris), nrow(iris))
diag(V) <- sd

Z <- as_data(Z)
V <- V + Z %*% G %*% t(Z)

sep <- t(iris$Sepal.Width)
distribution(sep) <- multivariate_normal(mu, V)

## ----bayesian_neural_network_data, highlight = FALSE---------------------
N <- 100
p <- 10

set.seed(23)  
X <- matrix(rnorm(N * p), N)
beta <- rnorm(10)
y <- X %*% beta + rnorm(N, sd = 0.1)

## ----bayesian_neural_network_greta---------------------------------------
neural_network <- function(x)
{
  # this can be arbitrarily complex, e.g. multiple hidden layers
  x %*% weights
}
  
weights <- normal(0, 1, dim = c(p, 1))
sd <- inverse_gamma(1, 1)

distribution(y) <- normal(neural_network(X), sd)

## ------------------------------------------------------------------------
m <- model(weights)
d <- opt(m)

print(beta)
print(d$par)

## ----lightspeed_greta----------------------------------------------------
beta  <- variable()
sigma <- variable(lower = 0)

distribution(y) <- normal(beta, sigma)

## ----schools_greta-------------------------------------------------------
sigma_eta <- inverse_gamma(1, 1)
eta       <- normal(0, sigma_eta, dim=N)

mu_theta <- normal(0, 100)
xi       <- normal(0, 5)
theta    <- mu_theta + xi * eta

distribution(y) <- normal(theta, sigma_y)

## ----poisson_greta-------------------------------------------------------
# load greta
library(greta)

# create matrices to greta arrays
X <- as_data(env)
Y <- as_data(occupancy)

# create greta arrays for random variables
alpha <- normal(0, 10)
beta <- normal(0, 10, dim = n_env)

# model
linear_predictor <- alpha + X %*% beta
lambda <- exp(linear_predictor)
distribution(Y) <- poisson(lambda)

