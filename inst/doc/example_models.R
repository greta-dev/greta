## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(comment = NA)
library (greta)

## ----air_data------------------------------------------------------------
y <- c(21, 20, 15)
n <- c(48, 34, 21)
Z <- c(10, 30, 50)
alpha <- 4.48
beta <- 0.76
sigma2 <- 81.14
sigma <- sqrt(sigma2)
tau <- 1 / sigma2
J <- 3

## ----air_greta-----------------------------------------------------------
theta = normal(0, 32)
mu <- alpha + beta * Z
X = normal(mu, sigma)
p <- ilogit(theta[1] + theta[2] * X)
distribution(y) = binomial(n, p)

## ----air_stan, echo = FALSE----------------------------------------------
cat(readLines('https://raw.githubusercontent.com/stan-dev/example-models/master/bugs_examples/vol2/air/air.stan'), sep = '\n')

## ----beetles_data--------------------------------------------------------
x <- c(1.6907, 1.7242, 1.7552, 1.7842, 1.8113, 1.8369, 1.8610, 1.8839)
n <- c(59, 60, 62, 56, 63, 59, 62, 60)
r <- c(6, 13, 18, 28, 52, 53, 61, 60)
N <- 8

## ----beetles_greta-------------------------------------------------------
alpha_star = normal(0, 32)
beta = normal(0, 32)
p <- ilogit(alpha_star + beta * (x - mean(x)))
likelihood(r) = binomial(n, p)

alpha <- alpha_star - beta * mean(x)
rhat <- p * n

## ----beetles_stan, echo = FALSE------------------------------------------
cat(readLines('https://raw.githubusercontent.com/stan-dev/example-models/master/bugs_examples/vol2/beetles/beetles_logit.stan'), sep = '\n')

