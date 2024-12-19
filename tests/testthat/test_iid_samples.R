set.seed(2020 - 02 - 11)

test_that("univariate samples are correct", {
  skip_if_not(check_tf_version())

  compare_iid_samples(uniform,
    runif,
    parameters = list(min = -2, max = 3)
  )

  compare_iid_samples(normal,
    rnorm,
    parameters = list(mean = -2, sd = 3)
  )

  compare_iid_samples(lognormal,
    rlnorm,
    parameters = list(mean = -2, sd = 3)
  )

  compare_iid_samples(bernoulli,
    extraDistr::rbern,
    parameters = list(prob = 0.3)
  )

  compare_iid_samples(binomial,
    rbinom,
    parameters = list(size = 5, prob = 0.3),
    nsim = 1000
  )

  compare_iid_samples(beta_binomial,
    extraDistr::rbbinom,
    parameters = list(size = 12, alpha = 4, beta = 2)
  )

  compare_iid_samples(negative_binomial,
    rnbinom,
    parameters = list(size = 12, prob = 0.3),
    nsim = 1000
  )

  compare_iid_samples(poisson,
    rpois,
    parameters = list(lambda = 3.14)
  )

  compare_iid_samples(gamma,
    rgamma,
    parameters = list(shape = 3, rate = 1.2)
  )

  compare_iid_samples(inverse_gamma,
    extraDistr::rinvgamma,
    parameters = list(alpha = 3, beta = 1.2)
  )

  compare_iid_samples(weibull,
    rweibull,
    parameters = list(shape = 1.2, scale = 3.2)
  )

  compare_iid_samples(exponential,
    rexp,
    parameters = list(rate = 0.54)
  )

  compare_iid_samples(pareto,
    extraDistr::rpareto,
    parameters = list(a = 1, b = 2)
  )

  compare_iid_samples(student,
    extraDistr::rlst,
    parameters = list(df = 3, mu = -2, sigma = 3)
  )

  compare_iid_samples(laplace,
    extraDistr::rlaplace,
    parameters = list(mu = -2, sigma = 1.2)
  )

  compare_iid_samples(beta,
    rbeta,
    parameters = list(shape1 = 3, shape2 = 2)
  )

  compare_iid_samples(cauchy,
    rcauchy,
    parameters = list(location = -1, scale = 1.2)
  )

  compare_iid_samples(chi_squared,
    rchisq,
    parameters = list(df = 4)
  )

  compare_iid_samples(logistic,
    rlogis,
    parameters = list(location = -2, scale = 1.3)
  )

  compare_iid_samples(f,
    rf,
    parameters = list(df1 = 4, df2 = 1)
  )
})

test_that("truncated univariate samples are correct", {
  skip_if_not(check_tf_version())

  # an originally unconstrained distribution

  # positive
  compare_iid_samples(normal,
    rtnorm,
    parameters = list(
      mean = -2,
      sd = 3,
      truncation = c(-3, Inf)
    )
  )

  # negative
  compare_iid_samples(normal,
    rtnorm,
    parameters = list(
      mean = -2,
      sd = 3,
      truncation = c(-Inf, -3)
    )
  )

  # both
  compare_iid_samples(normal,
    rtnorm,
    parameters = list(
      mean = -2,
      sd = 3,
      truncation = c(-3, 10)
    )
  )

  # originally constrained distribution

  compare_iid_samples(lognormal,
    rtlnorm,
    parameters = list(
      mean = -2,
      sd = 3,
      truncation = c(2, 3)
    )
  )

  compare_iid_samples(weibull,
    rtweibull,
    parameters = list(
      shape = 1.2,
      scale = 3.2,
      truncation = c(2, 3)
    )
  )
})

test_that("multivariate samples are correct", {
  skip_if_not(check_tf_version())

  sigma <- rwish(1, 5, diag(4))[1, , ]
  prob <- t(runif(4))
  prob <- prob / sum(prob)

  compare_iid_samples(multivariate_normal,
    rmvnorm,
    parameters = list(mean = t(rnorm(4)), Sigma = sigma)
  )

  compare_iid_samples(multinomial,
    rmulti,
    parameters = list(size = 12, prob = prob)
  )

  compare_iid_samples(categorical,
    rcat,
    parameters = list(prob = prob)
  )

  compare_iid_samples(dirichlet,
    extraDistr::rdirichlet,
    parameters = list(alpha = t(runif(4)))
  )

  compare_iid_samples(dirichlet_multinomial,
    extraDistr::rdirmnom,
    parameters = list(size = 3, alpha = t(runif(4)))
  )

  compare_iid_samples(wishart,
    rwish,
    parameters = list(df = 7, Sigma = sigma)
  )

  compare_iid_samples(lkj_correlation,
    rlkjcorr,
    parameters = list(eta = 6.5, dimension = 4)
  )
})

test_that("joint samples are correct", {
  skip_if_not(check_tf_version())

  # joint of normal distributions
  params <- list(
    list(mean = 0, sd = 1),
    list(mean = 0, sd = 2)
  )
  compare_iid_samples(joint_normals,
    rjnorm,
    parameters = params
  )


  # joint of truncated normal distributions
  params <- list(
    list(mean = 0, sd = 1, truncation = c(-1, Inf)),
    list(mean = 0, sd = 2, truncation = c(-Inf, 2)),
    list(mean = 0, sd = 3, truncation = c(-2, 1))
  )
  compare_iid_samples(joint_normals,
    rjtnorm,
    parameters = params
  )
})


test_that("mixture samples are correct", {
  skip_if_not(check_tf_version())

  # mixture of normal distributions
  weights <- runif(3)
  weights <- weights / sum(weights)

  params <- list(
    list(mean = 0, sd = 1),
    list(mean = 0, sd = 2),
    list(mean = 10, sd = 1),
    weights = weights
  )

  compare_iid_samples(mixture_normals,
    rmixnorm,
    parameters = params
  )

  # mixture of truncated normal distributions
  params <- list(
    list(mean = 0, sd = 1, truncation = c(-1, Inf)),
    list(mean = 0, sd = 2, truncation = c(-1, Inf)),
    list(mean = 0, sd = 3, truncation = c(-1, Inf)),
    weights = weights
  )

  compare_iid_samples(mixture_normals,
    rmixtnorm,
    parameters = params
  )

  # mixture of multivariate normal distributions
  sigma <- diag(4) * 0.1
  params <- list(
    list(mean = matrix(10, 1, 4), Sigma = sigma),
    list(mean = matrix(0, 1, 4), Sigma = sigma),
    list(mean = matrix(-10, 1, 4), Sigma = sigma),
    weights = weights
  )

  compare_iid_samples(mixture_multivariate_normals,
    rmixmvnorm,
    parameters = params
  )
})

test_that("distributions without RNG error nicely", {
  # (move these into other tests as they get implemented)
  skip_if_not(check_tf_version())

  # univariate
  expect_snapshot(error = TRUE,
    compare_iid_samples(hypergeometric,
                        rhyper,
                        parameters = list(m = 11, n = 8, k = 5)
    )
  )

  # truncated RNG not implemented
  expect_snapshot(error = TRUE,
    compare_iid_samples(f,
      rtf,
      parameters = list(
        df1 = 4,
        df2 = 1,
        truncation = c(2, 3)
      )
    )
  )
})
