context('representations')

test_that('bernoulli prob representations have correct density', {

  skip_if_not(check_tf_version())
  source('helpers.R')

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

test_that('binomial prob representations have correct density', {

  skip_if_not(check_tf_version())
  source('helpers.R')

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

test_that('poisson lambda representation has correct density', {

  skip_if_not(check_tf_version())
  source('helpers.R')

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
