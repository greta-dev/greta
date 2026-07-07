# Example models

## Common models

Below are a few examples of common statistical models implemented in
greta.

------------------------------------------------------------------------

### Linear regression

A simple, one-variable Bayesian linear regression model using the
attitude data

``` r

# variables & priors
int <- normal(0, 10)
coef <- normal(0, 10)
sd <- cauchy(0, 3, truncation = c(0, Inf))

# linear predictor
mu <- int + coef * attitude$complaints

# observation model
distribution(attitude$rating) <- normal(mu, sd)
```

------------------------------------------------------------------------

### Multiple linear regression

A multi-variable Bayesian linear regression model using the attitude
data

``` r

data(attitude)
design <- as.matrix(attitude[, 2:7])
```

``` r

int <- normal(0, 10)
coefs <- normal(0, 10, dim = ncol(design))
sd <- cauchy(0, 3, truncation = c(0, Inf))

# matrix multiplication is more efficient than multiplying the coefficients
# separately
mu <- int + design %*% coefs

distribution(attitude$rating) <- normal(mu, sd)
```

------------------------------------------------------------------------

### Multiple Poisson regression

A multiple Bayesian linear regression model using the `warpbreaks` data.

``` r

data("warpbreaks")
X <- as_data(model.matrix(breaks ~ wool + tension, warpbreaks))
y <- as_data(warpbreaks$breaks)
```

``` r

int <- variable()
coefs <- normal(0, 5, dim = ncol(X) - 1)
beta <- c(int, coefs)

eta <- X %*% beta

distribution(y) <- poisson(exp(eta))
```

------------------------------------------------------------------------

### Multiple categorical regression

A multi-variable Bayesian categorical regression model using the iris
data.

``` r

data(iris)
X <- as_data(cbind(1, iris[, 1:4]))
y <- model.matrix(~ Species - 1, iris)
P <- ncol(X)
K <- ncol(y)
```

``` r

beta <- normal(0, 5, dim = c(P, K - 1))
eta <- X %*% beta
prob <- imultilogit(eta)
distribution(y) <- categorical(prob)
```

------------------------------------------------------------------------

### Multiple linear regression with LASSO prior

A multi-variable Bayesian linear regression model using an
exponential-normal prior for the coefficients.

``` r

data(attitude)
design <- as.matrix(attitude[, 2:7])
```

``` r

int <- normal(0, 10)
sd <- cauchy(0, 3, truncation = c(0, Inf))

tau <- exponential(0.5, dim = ncol(design)) 
coefs <- normal(0, tau)
mu <- int + design %*% coefs

distribution(attitude$rating) <- normal(mu, sd)
```

------------------------------------------------------------------------

### Hierarchical linear regression

A hierarchical, Bayesian linear regression model using the iris data,
with random intercepts for each of the three species.

``` r

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
```

------------------------------------------------------------------------

### Random intercept-slope model

A hierarchical, Bayesian linear regression model using the iris data,
with random intercepts and slopes for each of the three species. The
slopes and intercepts for each species are *uncorrelated* in this
example.

``` r

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
```

------------------------------------------------------------------------

### Random intercept-slope model (with correlated effects)

A hierarchical, Bayesian linear regression model using the iris data,
with random intercepts and slopes for each of the three species. The
slopes and intercepts for each species are *correlated* in this example.

``` r

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
```

## Common Bayesian priors

The following examples show some common Bayesian priors of which some
induce sparsity.

------------------------------------------------------------------------

### Improper flat prior

A simple, one-variable Bayesian linear regression model that uses flat
priors for the coefficients. A flat prior using `variable` puts an
unbounded uniform distribution on the parameter. With unconstrained flat
priors, the posterior will be proportional to the likelihood and the MAP
will correspond to the MLE. Flat priors are usually chosen when there is
little knowledge about the parameters available.

``` r

# variables & priors
int  <- variable()
coef <- variable()
sd   <- cauchy(0, 3, truncation = c(0, Inf))

# linear predictor
mu <- int + coef * attitude$complaints

# observation model
distribution(attitude$rating) <- normal(mu, sd)
```

------------------------------------------------------------------------

### Ridge prior

Here we estimate a simple, one-variable Bayesian linear regression model
that uses a *ridge* prior. The ridge prior has a frequentist
interpretation where it is used as a penalty for regression
coefficients. Among other effects, the penalty shrinks the coefficients
towards zero to reduce variance without setting them to zero. The
Bayesian version uses a normal distribution for the slopes and a inverse
gamma prior for the strength of the penalty. Note that since the prior
in our intercept is still improper, the joint prior is also improper.

``` r

# variables & priors
int <- variable()
sd <- cauchy(0, 3, truncation = c(0, Inf))

tau <- inverse_gamma(1, 1)
coef <- normal(0, tau)

# linear predictor
mu <- int + coef * attitude$complaints

# observation model
distribution(attitude$rating) <- normal(mu, sd)
```

------------------------------------------------------------------------

### Exponential-normal prior

In this example we infer the parameters of one-variable Bayesian linear
regression model using an exponential-normal prior. A compound
exponential-normal prior can be interpreted like an equivalent to the
frequentist LASSO. The exponential-normal prior yields a posterior that
is pooled towards zero. An exponential-normal prior, or equivalently a
Laplace prior, is consequently often chosen when a sparse solution is
assumed, which, for instance, is a natural scenario in many biological
settings.

``` r

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
```

------------------------------------------------------------------------

### Horseshoe prior

A simple, one-variable Bayesian linear regression model using a
horseshoe prior. The horseshoe, just as the LASSO, can be used when the
slopes are assumed to be sparse. According to the original
[publication](http://proceedings.mlr.press/v5/carvalho09a/carvalho09a.pdf):
\> its flat, Cauchy-like tails allow strong signals to remain large
\[…\] \> a posteriori. Yet its infinitely tall spike at the origin
provides \> severe shrinkage for the zero elements

``` r

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
```

------------------------------------------------------------------------

### Regularized horseshoe prior

The regularized (‘Finnish’) horseshoe (doi.org/10.1214/17-EJS1337SI)
remedies a problem of the original horseshoe: large, unregularized
values for the coefficients. This is especially problematic in scenarios
where the parameters are only weakly identified by the data, as in
logistic regression with perfectly seperable data.

``` r

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
```

## Advanced Bayesian models

Below are some more advanced examples implemented in greta.

------------------------------------------------------------------------

### Hierarchical linear regression in general conditional formulation

A hierarchical, Bayesian linear regression model using the iris data,
with random intercepts and slopes for each of the three species. The
slopes and intercepts for each species are *correlated* in this example.
We allow every species to have a species specific slope for
`Sepal.Length`.

``` r

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
```

------------------------------------------------------------------------

### Hierarchical linear regression in general marginal formulation

A hierarchical, Bayesian linear regression model using the iris data,
with random intercepts and slopes for each of the three species. This
time we try to set up the *marginal* model, i.e. when we integrate the
conditional density.

``` r

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
```

------------------------------------------------------------------------

### Bayesian neural network

*Bayesian neural network* estimates an easy neural network with a normal
prior on the edge weights. For clarity we use an architecture without a
hidden layer, such that the weights actually correspond to coefficients
in a linear regression model.

``` text
N <- 100
p <- 10

set.seed(23)  
X <- matrix(rnorm(N * p), N)
beta <- rnorm(10)
y <- X %*% beta + rnorm(N, sd = 0.1)
```

``` r

neural_network <- function(x)
{
  # this can be arbitrarily complex, e.g. multiple hidden layers
  x %*% weights
}
  
weights <- normal(0, 1, dim = c(p, 1))
sd <- inverse_gamma(1, 1)

distribution(y) <- normal(neural_network(X), sd)
```

------------------------------------------------------------------------

### Factor analysis

Factor analysis is a linear latent model used for finding a
lower-dimensional probabilistic description of a data set with
observations $`\mathbf{x}_i \in \mathbb{R}^p`$. We assume the data are
generated according to
``` math
\mathbf{x}_i = \mathbf{W} \mathbf{z}_i + \boldsymbol \mu + \epsilon_i
```
where the noise $`\epsilon`$ is normally distributed with zero mean and
diagonal covariance matrix
$`\Psi = \mathrm{diag}(\psi_1, \dots, \psi_p)`$. The goal of factor
analysis is to estimate the latent variables
$`\mathbf{z}_i \mathbb{R}^q`$.

In this example we take the mean vector $`\boldsymbol \mu`$ to be zero.

``` text
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
```

``` r

W <- normal(0, 1, dim = c(p, q))
Z <- normal(0, 1, dim = c(q, n))
psi <- zeros(p, p)
diag(psi) <- inverse_gamma(1, 1, dim = p)

distribution(X) <- multivariate_normal(t(W %*% Z), psi)
```

## BUGS models

The BUGS project provide a number of example models written in the BUGS
modelling language. These models will run in WinBUGS and OpenBUGS, and
likely also in JAGS. The [Stan
wiki](https://github.com/stan-dev/example-models/wiki/BUGS-Examples-Sorted-Alphabetically)
provides Stan implementations of these models.

The following sections provide greta implementations of some of these
example models, alongside the BUGS code from [WinBUGS examples volume
2](https://legacy.voteview.com/pdf/WINBUGSmanual_2.pdf) (PDF) and Stan
code and an R version of the data from the [Stan example models
wiki](https://github.com/stan-dev/example-models/wiki).

------------------------------------------------------------------------

### Air

*Air* analyses reported respiratory illness versus exposure to nitrogen
dioxide in 103 children. The parameters `alpha`, `beta` and `sigma2` are
known in advance, and the data are grouped into three categories.

See [WinBUGS examples volume
2](https://legacy.voteview.com/pdf/WINBUGSmanual_2.pdf) (pdf) for
details.

#### data

``` text
y <- c(21, 20, 15)
n <- c(48, 34, 21)
Z <- c(10, 30, 50)
alpha <- 4.48
beta <- 0.76
sigma2 <- 81.14
sigma <- sqrt(sigma2)
tau <- 1 / sigma2
J <- 3
```

#### greta code

``` r

theta <- normal(0, 32, dim = 2)
mu <- alpha + beta * Z
X <- normal(mu, sigma)
p <- ilogit(theta[1] + theta[2] * X)
distribution(y) <- binomial(n, p)
```

#### BUGS/JAGS code

    for(j in 1 : J) {
       y[j] ~ dbin(p[j], n[j])
       logit(p[j]) <- theta[1] + theta[2] * X[j]
       X[j] ~ dnorm(mu[j], tau)
       mu[j] <- alpha + beta * Z[j]
    }
    theta[1] ~ dnorm(0.0, 0.001)
    theta[2] ~ dnorm(0.0, 0.001)

#### Stan code

    data {
      real alpha;
      real beta;
      real<lower=0> sigma2;
      int<lower=0> J;
      array[J] int y;
      vector[J] Z;
      array[J] int n;
    }
    transformed data {
      real<lower=0> sigma;
      sigma = sqrt(sigma2);
    }
    parameters {
      real theta1;
      real theta2;
      vector[J] X;
    }
    model {
      array[J] real p;
      theta1 ~ normal(0, 32); // 32^2 = 1024 
      theta2 ~ normal(0, 32);
      X ~ normal(alpha + beta * Z, sigma);
      y ~ binomial_logit(n, theta1 + theta2 * X);
    }

------------------------------------------------------------------------

### Beetles

*Beetles* considers dose-response data from an experiment applying
carbon disulphide to 8 beetles. The original example compares three
different link functions; the logit, probit and complementary log-log.
Here, only the code for the logit link is shown. You can implement the
other two link functions in greta by changing `ilogit` to `iprobit` or
`icloglog`.

See [WinBUGS examples volume
2](https://legacy.voteview.com/pdf/WINBUGSmanual_2.pdf) (pdf) for
details.

#### data

``` text
x <- c(1.6907, 1.7242, 1.7552, 1.7842, 1.8113, 1.8369, 1.8610, 1.8839)
n <- c(59, 60, 62, 56, 63, 59, 62, 60)
r <- c(6, 13, 18, 28, 52, 53, 61, 60)
N <- 8
```

#### greta code

``` r

alpha_star <- normal(0, 32)
beta <- normal(0, 32)
p <- ilogit(alpha_star + beta * (x - mean(x)))
distribution(r) <- binomial(n, p)

alpha <- alpha_star - beta * mean(x)
rhat <- p * n
```

#### BUGS/JAGS code

    for( i in 1 : N ) {
      r[i] ~ dbin(p[i],n[i])
      logit(p[i]) <- alpha.star + beta * (x[i] - mean(x[]))
      rhat[i] <- n[i] * p[i]
      culmative.r[i] <- culmative(r[i], r[i])
    }
    alpha <- alpha.star - beta * mean(x[])
    beta ~ dnorm(0.0,0.001)
    alpha.star ~ dnorm(0.0,0.001)

#### Stan code

    data {
      int<lower=0> N;
      array[N] int<lower=0> n;
      array[N] int<lower=0> r;
      vector[N] x;
    }
    transformed data {
      vector[N] centered_x;
      real mean_x;
      mean_x = mean(x);
      centered_x = x - mean_x;
    }
    parameters {
      real alpha_star;
      real beta;
    }
    transformed parameters {
      vector[N] m;
      m = alpha_star + beta * centered_x;
    }
    model {
      alpha_star ~ normal(0.0, 1.0E4);
      beta ~ normal(0.0, 1.0E4);
      r ~ binomial_logit(n, m);
    }
    generated quantities {
      real alpha;
      array[N] real p;
      array[N] real llike;
      array[N] real rhat;
      for (i in 1 : N) {
        p[i] = inv_logit(m[i]);
        llike[i] = r[i] * log(p[i]) + (n[i] - r[i]) * log(1 - p[i]);
        rhat[i] = p[i] * n[i]; // fitted values
      }
      alpha = alpha_star - beta * mean_x;
    }

## Stan models

The following few code examples show how Stan code can be translated in
equivalent greta models.

------------------------------------------------------------------------

### Lightspeed

*Lightspeed* estimates a linear normal model without predictors. The
data are 66 measurements from Simon Newcomb and represent the time
required for light to travel roughly 7500 meters.

See also the [Stan
examples](https://github.com/stan-dev/example-models/wiki/ARM-Models-Sorted-by-Type#no-predictors)
for details.

#### data

``` text
y <- c(28, 26, 33, 24, 34, -44, 27, 16, 40, -2, 29, 22, 24, 21, 25, 
       30, 23, 29, 31, 19, 24, 20, 36, 32, 36, 28, 25, 21, 28, 29, 
       37, 25, 28, 26, 30, 32, 36, 26, 30, 22, 36, 23, 27, 27, 28, 
       27, 31, 27, 26, 33, 26, 32, 32, 24, 39, 28, 24, 25, 32, 25, 
       29, 27, 28, 29, 16, 23)
n <- length(y)
```

#### greta code

``` r

beta  <- variable()
sigma <- variable(lower = 0)

distribution(y) <- normal(beta, sigma)
```

#### Stan code

    data {
      int<lower=0> N;
      vector[N] y;
    }
    parameters {
      vector[1] beta;
      real<lower=0> sigma;
    }
    model {
      y ~ normal(beta[1], sigma);
    }

------------------------------------------------------------------------

### Eight schools

*Eight schools* estimates the effect of coaching programs in eight
schools. The data are 8 measurements of coaching effects along with
their standard errors.

See also the [Stan
example](https://github.com/stan-dev/example-models/wiki/ARM-Models-Sorted-by-Type#varying-intercept)
for details.

#### data

``` text
y <- c(28,  8, -3,  7, -1,  1, 18, 12)
sigma_y <- c(15, 10, 16, 11,  9, 11, 10, 18)
N  <- length(y)
```

#### greta code

``` r

sigma_eta <- inverse_gamma(1, 1)
eta <- normal(0, sigma_eta, dim=N)

mu_theta <- normal(0, 100)
xi <- normal(0, 5)
theta <- mu_theta + xi * eta

distribution(y) <- normal(theta, sigma_y)
```

#### Stan code

    data {
      int<lower=0> N;
      vector[N] y;
      vector[N] sigma_y;
    }
    parameters {
      vector[N] eta;
      real mu_theta;
      real<lower=0, upper=100> sigma_eta;
      real xi;
    }
    transformed parameters {
      real<lower=0> sigma_theta;
      vector[N] theta;
      
      theta = mu_theta + xi * eta;
      sigma_theta = abs(xi) / sigma_eta;
    }
    model {
      mu_theta ~ normal(0, 100);
      sigma_eta ~ inv_gamma(1, 1); //prior distribution can be changed to uniform
      
      eta ~ normal(0, sigma_eta);
      xi ~ normal(0, 5);
      y ~ normal(theta, sigma_y);
    }

## Ecological models

Here we provide some examples of common ecological models. We begin with
a basic logistic regression often used in species distribution modelling
to estimate species probability of presence. We then provide
increasingly complex species distribution models, beginning with
modelling observation error directly, and moving on to models for
multiple species: independently but concurrently modelled species,
partially pooled coefficients, repeated measures, and sub-models.

------------------------------------------------------------------------

### Logistic regression

A simple logistic regression being to estimate the probability of
species presence along a number of environmental gradients.

#### data

``` text
# make fake data
n_env <- 3
n_sites <- 20

# n_sites x n_env matrix of environmental variables
env <- matrix(rnorm(n_sites * n_env), nrow = n_sites) 
# n_sites observations of species presence or absence
occupancy <- rbinom(n_sites, 1, 0.5) 
```

#### greta code

``` r

alpha <- normal(0, 10)
beta <- normal(0, 10, dim = n_env)

# logit-linear model
linear_predictor <- alpha + env %*% beta
p <- ilogit(linear_predictor)

# distribution (likelihood) over observed values
distribution(occupancy) <- bernoulli(p)
```

------------------------------------------------------------------------

### Poisson regression

An example of a simple poisson regression being used to estimate the
abundance of a species along a number of environmental gradients.

#### data

``` text
# make fake data
n_env <- 3
n_sites <- 20

# n_sites x n_env matrix of environmental variables
env <- matrix(rnorm(n_sites * n_env), nrow = n_sites) 
# n_sites observations of species abundance
occupancy <- rpois(n_sites, 5) 
```

#### greta code

``` r

alpha <- normal(0, 10)
beta <- normal(0, 10, dim = n_env)
linear_predictor <- alpha + env %*% beta
lambda <- exp(linear_predictor)
distribution(occupancy) <- poisson(lambda)
```

------------------------------------------------------------------------

### Logistic regression with error term

This is an example of a simple logistic regression with an extra
observation-level error term, to model over-dispersion or clustering in
occupancy data from multiple visits.

#### data

``` text
# make fake data
n_env <- 3
n_sites <- 20
n_obs <- 5

# n_sites x n_env matrix of environmental variables
env <- matrix(rnorm(n_sites * n_env), nrow = n_sites) 
# n_sites observations of species presence or absence over n_obs visits
occupancy <- rbinom(n_sites, n_obs, 0.5)
```

#### greta code

``` r

alpha <- normal(0, 10)
beta <- normal(0, 10, dim = n_env)
error <- normal(0, 10, dim = n_sites)

# logit-linear model with extra variation
linear_predictor <- alpha + env %*% beta + error
p <- ilogit(linear_predictor)

# distribution (likelihood) over observed values
distribution(occupancy) <- binomial(n_obs, p)
```

------------------------------------------------------------------------

### Multiple species modelling independently and concurrently

An example of a logistic regression being used to estimate the
probability of multiple species’ presences along a number of
environmental gradients. Although modelled concurrently, the random
variables for each species are independent. We first simulate some data
to model followed by the `greta` code.

Where a single observation per species and location would have a
bernoulli error distribution, multiple observations for each species and
location have a binomial distribution.

When modelling multiple species (or other grouping factor), we need an
extra step in constructing the linear predictor. In order to add
multiple `greta` arrays together *for each species* we can use the
[`sweep()`](https://greta-dev.github.io/greta/reference/overloaded.md)
function.

#### data

``` text
# make fake data
n_species <- 5
n_env <- 3
n_sites <- 20

env <- matrix(rnorm(n_sites * n_env), nrow = n_sites)
occupancy <- matrix(rbinom(n_species * n_sites, 1, 0.5), nrow = n_sites)
```

#### greta code

``` r

alpha <- normal(0, 10, dim = n_species)
beta <- normal(0, 10, dim = c(n_env, n_species))

env_effect <- env %*% beta

# add intercepts for all species
linear_predictor <- sweep(env_effect, 2, alpha, FUN = '+')

# ilogit of linear predictor
p <- ilogit(linear_predictor)

# a single observation means our data are bernoulli distributed
distribution(occupancy) <- bernoulli(p)
```

------------------------------------------------------------------------

### Multiple species with partial pooling of regression coefficients

An example of a logistic regression being used to estimate the
probability of multiple species’ presences along a number of
environmental gradients. Instead of assuming independence of species
regression coefficients, we assume they are drawn from a shared
distribution. We partially pool species responses. This gives us not ony
the regression coefficients for each species but also a global average
coefficient and a measure of variation between species responses to
environmental gradients.

#### data

``` text
# make fake data
n_species <- 5
n_env <- 1
n_sites <- 50

env <- matrix(rnorm(n_sites * n_env), nrow = n_sites)
occupancy <- matrix(rbinom(n_sites * n_species, 1, 0.5), nrow = n_sites)
```

#### greta code

``` r

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
```

------------------------------------------------------------------------

### Multiple species with sub-model for regression coefficients

An example of a logistic regression being used to estimate the
probability of multiple species’ presences along a number of
environmental gradients. Instead of assuming independence of species
regression coefficients, or partial pooling in shared distributions, we
use a sub-model to estimate species regression coefficients. In this
case, we’re using species traits to estimate their response to different
environmental gradients.

Because we’re building a sub-model, it’s more efficient to simply add a
column of ones to dataframes for the base model and sub-model. This is
simply to prevent our code from becoming too cumbersome. If we didn’t
want to use our sub-model to estimate the intercept, we would not need
to include the column of ones in the environmental dataframe.

#### data

``` text
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
```

#### greta code

``` r

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
```

------------------------------------------------------------------------

### Cormack-Jolly-Seber model

*Cormack-Jolly-Seber* (CJS) models estimate probabilities of survival
and recapture from mark-recapture data. These models assume that we can
only ever see individuals that have been initially marked and released
or recaptured following release (i.e. individuals do not exist until
first observed). The two key parameters are survival, $`\phi`$, and
probability of recapture, $`p`$. There is an additional derived
parameter, $`\chi`$, which is the probability that an individual is not
recaptured following its final capture. $`\chi`$ marginalises over
multiple scenarios in which the individual is not observed either
because it has died or because it is alive but not detected.

The [introductory book](http://www.phidot.org/software/mark/docs/book/)
to the program MARK has a lot of information on mark-recapture models,
including CJS models (starting in Ch. 1) and the broader class of
Jolly-Seber models (Ch. 12). There is also a section on mark-recapture
models in the [Stan language
manual](https://mc-stan.org/users/documentation/), which goes through
the derivation of the parameter $`\chi`$.

#### data

``` text
n_obs <- 100
n_time <- 20
y <- matrix(sample(c(0, 1), size = (n_obs * n_time), replace = TRUE),
            ncol = n_time)
```

#### greta code

``` r

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
```

#### BUGS/JAGS code

    model {
      # priors
      for (t in 1:(n_time - 1)) {
        phi[t] ~ dunif(0, 1)
        p[t] ~ dunif(0, 1)
      }
      # likelihood
      for (i in 1:n_obs) {
        z[i, first_obs[i]] <- 1   # state at first capture must be 1!
        for (t in (first_obs[i] + 1):n_time) {
          mu1[i, t] <- phi[t - 1] * z[i, t - 1] 
          z[i, t] ~ dbern(mu1[i, t])   # true state
          mu2[i, t] <- p[t - 1] * z[i, t]
          y[i, t] ~ dbern(mu2[i, t])      # observed state
        }
      }
    }

#### Stan code

    /**
     * Cormack-Jolly-Seber Model
     * 
     * following section 1.2.1 of:
     * http://www.maths.otago.ac.nz/home/resources/theses/PhD_Matthew_Schofield.pdf
     *
     */
    data {
      int<lower=2> K; // capture events
      int<lower=0> I; // number of individuals
      array[I, K] int<lower=0, upper=1> X; // X[i,k]: individual i captured at k
    }
    transformed data {
      array[I] int<lower=0, upper=K + 1> first; // first[i]: ind i first capture
      array[I] int<lower=0, upper=K + 1> last; // last[i]:  ind i last capture
      array[K] int<lower=0, upper=I> n_captured; // n_capt[k]: num aptured at k
      
      first = rep_array(K + 1, I);
      last = rep_array(0, I);
      for (i in 1 : I) {
        for (k in 1 : K) {
          if (X[i, k] == 1) {
            if (k < first[i]) {
              first[i] = k;
            }
            if (k > last[i]) {
              last[i] = k;
            }
          }
        }
      }
      
      n_captured = rep_array(0, K);
      for (i in 1 : I) {
        for (k in 1 : K) {
          n_captured[k] = n_captured[k] + X[i, k];
        }
      }
    }
    parameters {
      vector<lower=0, upper=1>[K - 1] phi; // phi[k]: Pr[alive at k + 1 | alive at k]
      vector<lower=0, upper=1>[K] p; // p[k]: Pr[capture at k]
      
      // note:  p[1] not used in model and hence not identified
    }
    transformed parameters {
      vector<lower=0, upper=1>[K] chi; // chi[k]: Pr[no capture >  k | alive at k]
      {
        int k;
        chi[K] = 1.0;
        k = K - 1;
        while (k > 0) {
          chi[k] = (1 - phi[k]) + phi[k] * (1 - p[k + 1]) * chi[k + 1];
          k = k - 1;
        }
      }
    }
    model {
      for (i in 1 : I) {
        if (last[i] > 0) {
          for (k in (first[i] + 1) : last[i]) {
            target += log(phi[k - 1]); // i survived from k-1 to k
            if (X[i, k] == 1) {
              target += log(p[k]);
            } // i captured at k
            else {
              target += log1m(p[k]);
            } // i not captured at k
          }
          target += log(chi[last[i]]); // i not seen after last[i]
        }
      }
    }
    generated quantities {
      // phi[K-1] and p(K) not identified, but product is
      real beta;
      vector<lower=0>[K] pop_hat; // population
      
      beta = phi[K - 1] * p[K];
      
      for (k in 1 : K) {
        pop_hat[k] = n_captured[k] / p[k];
      }
    }

------------------------------------------------------------------------
