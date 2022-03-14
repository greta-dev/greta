test_that("log and exp function representations work", {
  skip_if_not(check_tf_version())


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
  compare_op(
    calculate(log(y))[[1]],
    calculate(log(y2))[[1]]
  )
  compare_op(
    calculate(exp(x))[[1]],
    calculate(exp(x2))[[1]]
  )
})

test_that("chol & chol2inv function representation works", {
  skip_if_not(check_tf_version())


  # get symmetric matrix
  m <- 10
  w <- rWishart(1, m + 1, diag(m))[, , 1]
  u <- chol(w)

  # convert to greta arrays
  w <- as_data(w)
  u <- as_data(u)

  # get representation version of W
  w2 <- chol2symm(u)

  compare_op(
    calculate(chol(w))[[1]],
    calculate(chol(w2))[[1]]
  )

  compare_op(
    calculate(chol2inv(w))[[1]],
    calculate(chol2inv(w2))[[1]]
  )
})

test_that("bernoulli prob representations have correct density", {
  skip_if_not(check_tf_version())


  n <- 100
  x <- rbinom(n, 1, 0.5)
  probs <- runif(n)
  logits <- as_data(qlogis(probs))
  probits <- as_data(qnorm(probs))

  prob_dens <- greta_density(
    greta::bernoulli,
    list(prob = probs),
    x
  )

  probit_dens <- greta_density(
    greta::bernoulli,
    list(prob = iprobit(probits)),
    x
  )

  logit_dens <- greta_density(
    greta::bernoulli,
    list(prob = ilogit(logits)),
    x
  )

  compare_op(prob_dens, probit_dens)
  compare_op(prob_dens, logit_dens)
})

test_that("binomial prob representations have correct density", {
  skip_if_not(check_tf_version())


  n <- 100
  size <- rpois(n, 50)
  x <- rbinom(n, size, 0.5)

  probs <- runif(n)
  logits <- as_data(qlogis(probs))
  probits <- as_data(qnorm(probs))

  prob_dens <- greta_density(
    greta::binomial,
    list(
      size = size,
      prob = probs
    ),
    x
  )

  probit_dens <- greta_density(
    greta::binomial,
    list(
      size = size,
      prob = iprobit(probits)
    ),
    x
  )

  logit_dens <- greta_density(
    greta::binomial,
    list(
      size = size,
      prob = ilogit(logits)
    ),
    x
  )

  compare_op(prob_dens, probit_dens)
  compare_op(prob_dens, logit_dens, tolerance = 1e-3)
})

test_that("poisson lambda representation has correct density", {
  skip_if_not(check_tf_version())


  n <- 100
  x <- rpois(n, 10)

  lambdas <- rlnorm(n, 0, 1)
  logs <- as_data(log(lambdas))

  lambda_dens <- greta_density(
    greta::poisson,
    list(lambda = lambdas),
    x
  )

  log_dens <- greta_density(
    greta::poisson,
    list(lambda = exp(logs)),
    x
  )

  compare_op(lambda_dens, log_dens)
})

test_that("mvn Sigma representation has correct density", {
  skip_if_not(check_tf_version())


  n <- 100
  m <- 5
  mn <- t(rnorm(m))
  sig <- rWishart(1, m + 1, diag(m))[, , 1]
  x <- mvtnorm::rmvnorm(n, mn, sig)

  # greta arrays with and without representation
  sigs <- as_data(sig)
  u <- as_data(chol(sig))
  chol_sigs <- chol2symm(u)

  sigs_dens <- greta_density(greta::multivariate_normal,
    list(
      mean = mn,
      Sigma = sigs
    ),
    x,
    multivariate = TRUE
  )

  chol_sigs_dens <- greta_density(greta::multivariate_normal,
    list(
      mean = mn,
      Sigma = chol_sigs
    ),
    x,
    multivariate = TRUE
  )

  compare_op(sigs_dens, chol_sigs_dens)
})

test_that("wishart target and Sigma representations have correct density", {
  skip_if_not(check_tf_version())


  m <- 10
  x <- rWishart(1, m + 1, diag(m))[, , 1]
  sig <- rWishart(1, m + 1, diag(m))[, , 1]

  # greta arrays for Sigma with and without representation
  sigs <- as_data(sig)
  u <- as_data(chol(sig))
  chol_sigs <- chol2symm(u)

  sigs_dens <- greta_density(greta::wishart,
    list(
      df = m + 1,
      Sigma = sigs
    ),
    x,
    multivariate = TRUE
  )

  chol_sigs_dens <- greta_density(greta::wishart,
    list(
      df = m + 1,
      Sigma = chol_sigs
    ),
    x,
    multivariate = TRUE
  )

  compare_op(sigs_dens, chol_sigs_dens, 1e-2)

  # greta arrays for x with and without representation
  xs <- as_data(x)
  ux <- as_data(chol(x))
  chol_xs <- chol2symm(ux)

  xs_dens <- greta_density(greta::wishart,
    list(
      df = m + 1,
      Sigma = sig
    ),
    xs,
    multivariate = TRUE
  )

  chol_xs_dens <- greta_density(greta::wishart,
    list(
      df = m + 1,
      Sigma = sig
    ),
    chol_xs,
    multivariate = TRUE
  )

  compare_op(xs_dens, chol_xs_dens)
})

test_that("lkj target representation has correct density", {
  skip_if_not(check_tf_version())


  m <- 10
  eta <- 3
  x <- rWishart(1, m + 1, diag(m))[, , 1]
  x <- cov2cor(x)

  # greta arrays for x with and without representation
  xs <- as_data(x)
  ux <- as_data(chol(x))
  chol_xs <- chol2symm(ux)

  xs_dens <- greta_density(
    greta::lkj_correlation,
    list(eta = eta),
    xs
  )

  chol_xs_dens <- greta_density(
    greta::lkj_correlation,
    list(eta = eta),
    chol_xs
  )

  compare_op(xs_dens, chol_xs_dens)
})
