context("representations")

test_that("log and exp function representations work", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  # get log and exp pairs as greta data
  n <- 100
  x <- rnorm(n)
  y <- exp(x)

  x <- as_data(x)
  y <- as_data(y)

  # get versions with representations
  y2 <- exp(x)
  x2 <- log(y)

  # compare versions with/without representations
  compare_op(calculate(log(y)),
             calculate(log(y2)))
  compare_op(calculate(exp(x)),
             calculate(exp(x2)))

})

test_that("chol & chol2inv function representation works", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  # get symmetric matrix
  m <- 10
  W <- rWishart(1, m + 1, diag(m))[, , 1]
  U <- chol(W)

  # convert to greta arrays
  W <- as_data(W)
  U <- as_data(U)

  # get representation version of W
  W2 <- chol_to_symmetric(U)

  compare_op(calculate(chol(W)),
             calculate(chol(W2)))

  compare_op(calculate(chol2inv(W)),
             calculate(chol2inv(W2)))

})

test_that("bernoulli prob representations have correct density", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  n <- 100
  x <- rbinom(n, 1, 0.5)
  probs <- runif(n)
  logits <- as_data(qlogis(probs))
  probits <- as_data(qnorm(probs))

  prob_dens <- greta_density(greta::bernoulli,
                             list(prob = probs),
                             x)

  probit_dens <- greta_density(greta::bernoulli,
                               list(prob = iprobit(probits)),
                               x)

  logit_dens <- greta_density(greta::bernoulli,
                              list(prob = ilogit(logits)),
                              x)

  compare_op(prob_dens, probit_dens)
  compare_op(prob_dens, logit_dens)

})

test_that("binomial prob representations have correct density", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  n <- 100
  size <- rpois(n, 50)
  x <- rbinom(n, size, 0.5)

  probs <- runif(n)
  logits <- as_data(qlogis(probs))
  probits <- as_data(qnorm(probs))

  prob_dens <- greta_density(greta::binomial,
                             list(size = size,
                                  prob = probs),
                             x)

  probit_dens <- greta_density(greta::binomial,
                               list(size = size,
                                    prob = iprobit(probits)),
                               x)

  logit_dens <- greta_density(greta::binomial,
                              list(size = size,
                                   prob = ilogit(logits)),
                              x)

  compare_op(prob_dens, probit_dens)
  compare_op(prob_dens, logit_dens)

})

test_that("poisson lambda representation has correct density", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  n <- 100
  x <- rpois(n, 10)

  lambdas <- rlnorm(n, 0, 1)
  logs <- as_data(log(lambdas))

  lambda_dens <- greta_density(greta::poisson,
                               list(lambda = lambdas),
                               x)

  log_dens <- greta_density(greta::poisson,
                            list(lambda = exp(logs)),
                            x)

  compare_op(lambda_dens, log_dens)

})

test_that("mvn Sigma representation has correct density", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  n <- 100
  m <- 5
  mn <- t(rnorm(m))
  sig <- rWishart(1, m + 1, diag(m))[, , 1]
  x <- mvtnorm::rmvnorm(n, mn, sig)

  # greta arrays with and without representation
  sigs <- as_data(sig)
  U <- as_data(chol(sig))
  chol_sigs <- chol_to_symmetric(U)

  sigs_dens <- greta_density(greta::multivariate_normal,
                             list(mean = mn,
                                  Sigma = sigs),
                             x,
                             multivariate = TRUE)

  chol_sigs_dens <- greta_density(greta::multivariate_normal,
                                  list(mean = mn,
                                       Sigma = chol_sigs),
                                  x,
                                  multivariate = TRUE)

  compare_op(sigs_dens, chol_sigs_dens)

})

test_that("wishart target and Sigma representations have correct density", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  m <- 10
  x <- rWishart(1, m + 1, diag(m))[, , 1]
  sig <- rWishart(1, m + 1, diag(m))[, , 1]

  # greta arrays for Sigma with and without representation
  sigs <- as_data(sig)
  U <- as_data(chol(sig))
  chol_sigs <- chol_to_symmetric(U)

  sigs_dens <- greta_density(greta::wishart,
                             list(df = m + 1,
                                  Sigma = sigs),
                             x,
                             multivariate = TRUE)

  chol_sigs_dens <- greta_density(greta::wishart,
                                  list(df = m + 1,
                                       Sigma = chol_sigs),
                                  x,
                                  multivariate = TRUE)

  compare_op(sigs_dens, chol_sigs_dens, 1e-2)

  # greta arrays for x with and without representation
  xs <- as_data(x)
  Ux <- as_data(chol(x))
  chol_xs <- chol_to_symmetric(Ux)

  xs_dens <- greta_density(greta::wishart,
                           list(df = m + 1,
                                Sigma = sig),
                           xs,
                           multivariate = TRUE)

  chol_xs_dens <- greta_density(greta::wishart,
                                list(df = m + 1,
                                     Sigma = sig),
                                chol_xs,
                                multivariate = TRUE)

  compare_op(xs_dens, chol_xs_dens)

})

test_that("lkj target representation has correct density", {

  skip_if_not(check_tf_version())
  source("helpers.R")

  m <- 10
  eta <- 3
  x <- rWishart(1, m + 1, diag(m))[, , 1]
  x <- cov2cor(x)

  # greta arrays for x with and without representation
  xs <- as_data(x)
  Ux <- as_data(chol(x))
  chol_xs <- chol_to_symmetric(Ux)

  xs_dens <- greta_density(greta::lkj_correlation,
                           list(eta = eta),
                           xs)

  chol_xs_dens <- greta_density(greta::lkj_correlation,
                                list(eta = eta),
                                chol_xs)

  compare_op(xs_dens, chol_xs_dens)

})
