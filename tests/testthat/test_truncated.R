set.seed(2020 - 02 - 11)

test_that("truncated normal has correct densities", {
  skip_if_not(check_tf_version())

  # non truncated normal
  compare_truncated_distribution(
    normal,
    "norm",
    parameters = list(
      mean = -1,
      sd = 2.4
    ),
    truncation = c(-Inf, Inf)
  )

  # positive truncated
  compare_truncated_distribution(
    normal,
    "norm",
    parameters = list(
      mean = -1,
      sd = 2.4
    ),
    truncation = c(1, Inf)
  )

  # negative truncated
  compare_truncated_distribution(
    normal,
    "norm",
    parameters = list(
      mean = -1,
      sd = 2.4
    ),
    truncation = c(-Inf, 0)
  )

  # fully truncated
  compare_truncated_distribution(
    normal,
    "norm",
    parameters = list(
      mean = -1,
      sd = 2.4
    ),
    truncation = c(-2, -1)
  )
})

test_that("truncated lognormal has correct densities", {
  skip_if_not(check_tf_version())

  # non truncated
  compare_truncated_distribution(
    lognormal,
    "lnorm",
    parameters = list(
      meanlog = -1,
      sdlog = 2.4
    ),
    truncation = c(0, Inf)
  )

  # positive truncated
  compare_truncated_distribution(
    lognormal,
    "lnorm",
    parameters = list(
      meanlog = -1,
      sdlog = 2.4
    ),
    truncation = c(1, Inf)
  )

  # negative truncated
  compare_truncated_distribution(
    lognormal,
    "lnorm",
    parameters = list(
      meanlog = -1,
      sdlog = 2.4
    ),
    truncation = c(0, 2)
  )

  # fully truncated
  compare_truncated_distribution(
    lognormal,
    "lnorm",
    parameters = list(
      meanlog = -1,
      sdlog = 2.4
    ),
    truncation = c(2, 4)
  )
})

test_that("truncated gamma has correct densities", {
  skip_if_not(check_tf_version())

  # non truncated
  compare_truncated_distribution(
    gamma,
    "gamma",
    parameters = list(
      shape = 2,
      rate = 2
    ),
    truncation = c(0, Inf)
  )

  # positive truncated
  compare_truncated_distribution(
    gamma,
    "gamma",
    parameters = list(
      shape = 2,
      rate = 2
    ),
    truncation = c(1, Inf)
  )

  # negative truncated
  compare_truncated_distribution(
    gamma,
    "gamma",
    parameters = list(
      shape = 2,
      rate = 2
    ),
    truncation = c(0, 2)
  )

  # fully truncated
  compare_truncated_distribution(
    gamma,
    "gamma",
    parameters = list(
      shape = 2,
      rate = 2
    ),
    truncation = c(1, 2)
  )
})

test_that("truncated inverse gamma has correct densities", {
  skip_if_not(check_tf_version())

  # apparently testthat can't see these, trying out global assign to see if that
  # makes them visible
  dinvgamma <<- extraDistr::dinvgamma
  qinvgamma <<- extraDistr::qinvgamma
  pinvgamma <<- extraDistr::pinvgamma

  # non truncated
  compare_truncated_distribution(
    inverse_gamma,
    "invgamma",
    parameters = list(
      alpha = 2,
      beta = 1.2
    ),
    truncation = c(0, Inf)
  )

  # positive truncated
  compare_truncated_distribution(
    inverse_gamma,
    "invgamma",
    parameters = list(
      alpha = 2,
      beta = 1.2
    ),
    truncation = c(1, Inf)
  )

  # negative truncated
  compare_truncated_distribution(
    inverse_gamma,
    "invgamma",
    parameters = list(
      alpha = 2,
      beta = 1.2
    ),
    truncation = c(0, 2)
  )

  # fully truncated
  compare_truncated_distribution(
    inverse_gamma,
    "invgamma",
    parameters = list(
      alpha = 2,
      beta = 1.2
    ),
    truncation = c(1, 2)
  )
})

test_that("truncated weibull has correct densities", {
  skip_if_not(check_tf_version())

  # non truncated
  compare_truncated_distribution(
    weibull,
    "weibull",
    parameters = list(
      shape = 2,
      scale = 1.2
    ),
    truncation = c(0, Inf)
  )

  # positive truncated
  compare_truncated_distribution(
    weibull,
    "weibull",
    parameters = list(
      shape = 2,
      scale = 1.2
    ),
    truncation = c(1, Inf)
  )

  # negative truncated
  compare_truncated_distribution(
    weibull,
    "weibull",
    parameters = list(
      shape = 2,
      scale = 1.2
    ),
    truncation = c(0, 2)
  )

  # fully truncated
  compare_truncated_distribution(
    weibull,
    "weibull",
    parameters = list(
      shape = 2,
      scale = 1.2
    ),
    truncation = c(1, 2)
  )
})

test_that("truncated exponential has correct densities", {
  skip_if_not(check_tf_version())

  # non truncated
  compare_truncated_distribution(
    exponential,
    "exp",
    parameters = list(rate = 2),
    truncation = c(0, Inf)
  )

  # positive truncated
  compare_truncated_distribution(
    exponential,
    "exp",
    parameters = list(rate = 2),
    truncation = c(1, Inf)
  )

  # negative truncated
  compare_truncated_distribution(
    exponential,
    "exp",
    parameters = list(rate = 2),
    truncation = c(0, 2)
  )

  # fully truncated
  compare_truncated_distribution(
    exponential,
    "exp",
    parameters = list(rate = 2),
    truncation = c(1, 2)
  )
})

test_that("truncated pareto has correct densities", {
  skip_if_not(check_tf_version())
  # mock up pareto to have differently named parameters (a and b are use for the
  # truncation)
  #
  # # mock up pareto to have differently named parameters (a and b are use for the
  # # truncation)
  preto <<- function(a_, b_, dim, truncation) pareto(a_, b_, dim, truncation)
  dpreto <<- function(x, a_, b_) extraDistr::dpareto(x, a_, b_)
  ppreto <<- function(q, a_, b_) extraDistr::ppareto(q, a_, b_)
  qpreto <<- function(p, a_, b_) extraDistr::qpareto(p, a_, b_)

  # non truncated
  compare_truncated_distribution(
    preto,
    "preto",
    parameters = list(
      a_ = 1.9,
      b_ = 4.3
    ),
    truncation = c(0, Inf)
  )

  # positive truncated
  compare_truncated_distribution(
    preto,
    "preto",
    parameters = list(
      a_ = 1.9,
      b_ = 4.3
    ),
    truncation = c(7.2, Inf)
  )

  # negative truncated
  compare_truncated_distribution(
    preto,
    "preto",
    parameters = list(
      a_ = 1.9,
      b_ = 4.3
    ),
    truncation = c(0, 21.3)
  )

  # fully truncated
  compare_truncated_distribution(
    preto,
    "preto",
    parameters = list(
      a_ = 1.9,
      b_ = 4.3
    ),
    truncation = c(7.2, 21.3)
  )
})

test_that("truncated student has correct densities", {
  skip_if_not(check_tf_version())

  dstudent <<- extraDistr::dlst
  qstudent <<- extraDistr::qlst
  pstudent <<- extraDistr::plst

  # non truncated
  compare_truncated_distribution(
    student,
    "student",
    parameters = list(
      df = 5,
      mu = 2,
      sigma = 3.4
    ),
    truncation = c(-Inf, Inf)
  )

  # positive truncated
  compare_truncated_distribution(
    student,
    "student",
    parameters = list(
      df = 5,
      mu = 2,
      sigma = 3.4
    ),
    truncation = c(1, Inf)
  )

  # negative truncated
  compare_truncated_distribution(
    student,
    "student",
    parameters = list(
      df = 5,
      mu = 2,
      sigma = 3.4
    ),
    truncation = c(-Inf, 2)
  )

  # fully truncated
  compare_truncated_distribution(
    student,
    "student",
    parameters = list(
      df = 5,
      mu = 2,
      sigma = 3.4
    ),
    truncation = c(1, 2)
  )
})


test_that("truncated laplace has correct densities", {
  skip_if_not(check_tf_version())

  dlaplace <<- extraDistr::dlaplace
  qlaplace <<- extraDistr::qlaplace
  plaplace <<- extraDistr::plaplace

  # non truncated
  compare_truncated_distribution(
    laplace,
    "laplace",
    parameters = list(
      mu = 2,
      sigma = 3.4
    ),
    truncation = c(-Inf, Inf)
  )

  # positive truncated
  compare_truncated_distribution(
    laplace,
    "laplace",
    parameters = list(
      mu = 2,
      sigma = 3.4
    ),
    truncation = c(1, Inf)
  )

  # negative truncated
  compare_truncated_distribution(
    laplace,
    "laplace",
    parameters = list(
      mu = 2,
      sigma = 3.4
    ),
    truncation = c(-Inf, 2)
  )

  # fully truncated
  compare_truncated_distribution(
    laplace,
    "laplace",
    parameters = list(
      mu = 2,
      sigma = 3.4
    ),
    truncation = c(1, 2)
  )
})

test_that("truncated beta has correct densities", {
  skip_if_not(check_tf_version())

  # non truncated
  compare_truncated_distribution(
    beta,
    "beta",
    parameters = list(
      shape1 = 2.1,
      shape2 = 2.3
    ),
    truncation = c(0, 1)
  )

  # positive truncated
  compare_truncated_distribution(
    beta,
    "beta",
    parameters = list(
      shape1 = 2.1,
      shape2 = 2.3
    ),
    truncation = c(0.1, 1)
  )

  # negative truncated
  compare_truncated_distribution(
    beta,
    "beta",
    parameters = list(
      shape1 = 2.1,
      shape2 = 2.3
    ),
    truncation = c(0, 0.2)
  )

  # fully truncated
  compare_truncated_distribution(
    beta,
    "beta",
    parameters = list(
      shape1 = 2.1,
      shape2 = 2.3
    ),
    truncation = c(0.1, 0.2)
  )
})

test_that("truncated cauchy has correct densities", {
  skip_if_not(check_tf_version())

  # non truncated
  compare_truncated_distribution(
    cauchy,
    "cauchy",
    parameters = list(
      location = -1.3,
      scale = 2.3
    ),
    truncation = c(-Inf, Inf)
  )

  # positive truncated
  compare_truncated_distribution(
    cauchy,
    "cauchy",
    parameters = list(
      location = -1.3,
      scale = 2.3
    ),
    truncation = c(0.1, Inf)
  )

  # negative truncated
  compare_truncated_distribution(
    cauchy,
    "cauchy",
    parameters = list(
      location = -1.3,
      scale = 2.3
    ),
    truncation = c(-Inf, 0.2)
  )

  # fully truncated
  compare_truncated_distribution(
    cauchy,
    "cauchy",
    parameters = list(
      location = -1.3,
      scale = 2.3
    ),
    truncation = c(0.1, 0.2)
  )
})

test_that("truncated logistic has correct densities", {
  skip_if_not(check_tf_version())

  # non truncated
  compare_truncated_distribution(
    logistic,
    "logis",
    parameters = list(
      location = -1.3,
      scale = 2.3
    ),
    truncation = c(-Inf, Inf)
  )

  # positive truncated
  compare_truncated_distribution(
    logistic,
    "logis",
    parameters = list(
      location = -1.3,
      scale = 2.3
    ),
    truncation = c(0.1, Inf)
  )

  # negative truncated
  compare_truncated_distribution(
    logistic,
    "logis",
    parameters = list(
      location = -1.3,
      scale = 2.3
    ),
    truncation = c(-Inf, 0.2)
  )

  # fully truncated
  compare_truncated_distribution(
    logistic,
    "logis",
    parameters = list(
      location = -1.3,
      scale = 2.3
    ),
    truncation = c(0.1, 0.2)
  )
})

test_that("truncated f has correct densities", {
  skip_if_not(check_tf_version())

  # non truncated
  compare_truncated_distribution(
    f,
    "f",
    parameters = list(
      df1 = 1.3,
      df2 = 4.7
    ),
    truncation = c(0, Inf)
  )

  # positive truncated
  compare_truncated_distribution(
    f,
    "f",
    parameters = list(
      df1 = 1.3,
      df2 = 4.7
    ),
    truncation = c(0.1, Inf)
  )

  # negative truncated
  compare_truncated_distribution(
    f,
    "f",
    parameters = list(
      df1 = 1.3,
      df2 = 4.7
    ),
    truncation = c(0, 0.2)
  )

  # fully truncated
  compare_truncated_distribution(
    f,
    "f",
    parameters = list(
      df1 = 1.3,
      df2 = 4.7
    ),
    truncation = c(0.1, 0.2)
  )
})

test_that("truncated chi squared has correct densities", {
  skip_if_not(check_tf_version())

  # non truncated
  compare_truncated_distribution(
    chi_squared,
    "chisq",
    parameters = list(df = 9.3),
    truncation = c(0, Inf)
  )

  # positive truncated
  compare_truncated_distribution(
    chi_squared,
    "chisq",
    parameters = list(df = 9.3),
    truncation = c(0.1, Inf)
  )

  # negative truncated
  compare_truncated_distribution(
    chi_squared,
    "chisq",
    parameters = list(df = 9.3),
    truncation = c(0, 0.2)
  )

  # fully truncated
  compare_truncated_distribution(
    chi_squared,
    "chisq",
    parameters = list(df = 9.3),
    truncation = c(0.1, 0.2)
  )
})

test_that("bad truncations error", {
  skip_if_not(check_tf_version())

  expect_snapshot(error = TRUE, lognormal(0, 1, truncation = c(-1, Inf)))

  expect_snapshot(error = TRUE, beta(1, 1, truncation = c(-1, 2)))
})
