context('truncated distributions')

test_that('truncated normal has correct densities', {

  source('helpers.R')

  # non-truncated normal
  difference <- compare_truncated_distribution(normal,
                                               'norm',
                                               parameters = list(mean = -1,
                                                                 sd = 2.4),
                                               truncation = c(-Inf, Inf))
  expect_true(all(difference < 1e-4))

  # positive-truncated
  difference <- compare_truncated_distribution(normal,
                                               'norm',
                                               parameters = list(mean = -1,
                                                                 sd = 2.4),
                                               truncation = c(1, Inf))
  expect_true(all(difference < 1e-4))

  # negative-truncated
  difference <- compare_truncated_distribution(normal,
                                               'norm',
                                               parameters = list(mean = -1,
                                                                 sd = 2.4),
                                               truncation = c(-Inf, 0))
  expect_true(all(difference < 1e-4))

  # fully truncated
  difference <- compare_truncated_distribution(normal,
                                               'norm',
                                               parameters = list(mean = -1,
                                                                 sd = 2.4),
                                               truncation = c(-2, -1))
  expect_true(all(difference < 1e-4))

})

test_that('truncated lognormal has correct densities', {

  source('helpers.R')

  # non-truncated
  difference <- compare_truncated_distribution(lognormal,
                                               'lnorm',
                                               parameters = list(meanlog = -1,
                                                                 sdlog = 2.4),
                                               truncation = c(-Inf, Inf))
  expect_true(all(difference < 1e-4))

  # positive-truncated
  difference <- compare_truncated_distribution(lognormal,
                                               'lnorm',
                                               parameters = list(meanlog = -1,
                                                                 sdlog = 2.4),
                                               truncation = c(1, Inf))
  expect_true(all(difference < 1e-4))

  # negative-truncated
  difference <- compare_truncated_distribution(lognormal,
                                               'lnorm',
                                               parameters = list(meanlog = -1,
                                                                 sdlog = 2.4),
                                               truncation = c(-Inf, 2))
  expect_true(all(difference < 1e-4))

  # fully-truncated
  difference <- compare_truncated_distribution(lognormal,
                                               'lnorm',
                                               parameters = list(meanlog = -1,
                                                                 sdlog = 2.4),
                                               truncation = c(2, 4))
  expect_true(all(difference < 1e-4))

})

test_that('truncated uniform has correct densities', {

  source('helpers.R')

  # non-truncated
  difference <- compare_truncated_distribution(uniform,
                                               'unif',
                                               parameters = list(min = -1,
                                                                 max = 5),
                                               truncation = c(-Inf, Inf))
  expect_true(all(difference < 1e-4))

  # positive-truncated
  difference <- compare_truncated_distribution(uniform,
                                               'unif',
                                               parameters = list(min = -1,
                                                                 max = 5),
                                               truncation = c(0, Inf))
  expect_true(all(difference < 1e-4))

  # negative-truncated
  difference <- compare_truncated_distribution(uniform,
                                               'unif',
                                               parameters = list(min = -1,
                                                                 max = 5),
                                               truncation = c(-Inf, 4))
  expect_true(all(difference < 1e-4))

  # fully-truncated
  difference <- compare_truncated_distribution(uniform,
                                               'unif',
                                               parameters = list(min = -1,
                                                                 max = 5),
                                               truncation = c(1, 2))
  expect_true(all(difference < 1e-4))

})

test_that('truncated uniform has correct densities', {

  source('helpers.R')

  # non-truncated
  difference <- compare_truncated_distribution(uniform,
                                               'unif',
                                               parameters = list(min = -1,
                                                                 max = 5),
                                               truncation = c(-Inf, Inf))
  expect_true(all(difference < 1e-4))

  # positive-truncated
  difference <- compare_truncated_distribution(uniform,
                                               'unif',
                                               parameters = list(min = -1,
                                                                 max = 5),
                                               truncation = c(0, Inf))
  expect_true(all(difference < 1e-4))

  # negative-truncated
  difference <- compare_truncated_distribution(uniform,
                                               'unif',
                                               parameters = list(min = -1,
                                                                 max = 5),
                                               truncation = c(-Inf, 4))
  expect_true(all(difference < 1e-4))

  # fully-truncated
  difference <- compare_truncated_distribution(uniform,
                                               'unif',
                                               parameters = list(min = -1,
                                                                 max = 5),
                                               truncation = c(1, 2))
  expect_true(all(difference < 1e-4))

})

test_that('truncated gamma has correct densities', {

  source('helpers.R')

  # non-truncated
  difference <- compare_truncated_distribution(gamma,
                                               'gamma',
                                               parameters = list(shape = 2,
                                                                 rate = 2),
                                               truncation = c(-Inf, Inf))
  expect_true(all(difference < 1e-4))

  # positive-truncated
  difference <- compare_truncated_distribution(gamma,
                                               'gamma',
                                               parameters = list(shape = 2,
                                                                 rate = 2),
                                               truncation = c(1, Inf))
  expect_true(all(difference < 1e-4))

  # negative-truncated
  difference <- compare_truncated_distribution(gamma,
                                               'gamma',
                                               parameters = list(shape = 2,
                                                                 rate = 2),
                                               truncation = c(-Inf, 2))
  expect_true(all(difference < 1e-4))

  # fully-truncated
  difference <- compare_truncated_distribution(gamma,
                                               'gamma',
                                               parameters = list(shape = 2,
                                                                 rate = 2),
                                               truncation = c(1, 2))
  expect_true(all(difference < 1e-4))

})

test_that('truncated inverse gamma has correct densities', {

  source('helpers.R')

  # non-truncated
  difference <- compare_truncated_distribution(inverse_gamma,
                                               'invgamma',
                                               parameters = list(shape = 2,
                                                                 scale = 1.2),
                                               truncation = c(-Inf, Inf))
  expect_true(all(difference < 1e-4))

  # positive-truncated
  difference <- compare_truncated_distribution(inverse_gamma,
                                               'invgamma',
                                               parameters = list(shape = 2,
                                                                 scale = 1.2),
                                               truncation = c(1, Inf))
  expect_true(all(difference < 1e-4))

  # negative-truncated
  difference <- compare_truncated_distribution(inverse_gamma,
                                               'invgamma',
                                               parameters = list(shape = 2,
                                                                 scale = 1.2),
                                               truncation = c(-Inf, 2))
  expect_true(all(difference < 1e-4))

  # fully-truncated
  difference <- compare_truncated_distribution(inverse_gamma,
                                               'invgamma',
                                               parameters = list(shape = 2,
                                                                 scale = 1.2),
                                               truncation = c(1, 2))
  expect_true(all(difference < 1e-4))

})

test_that('truncated weibull has correct densities', {

  source('helpers.R')

  # non-truncated
  difference <- compare_truncated_distribution(weibull,
                                               'weibull',
                                               parameters = list(shape = 2,
                                                                 scale = 1.2),
                                               truncation = c(-Inf, Inf))
  expect_true(all(difference < 1e-4))

  # positive-truncated
  difference <- compare_truncated_distribution(weibull,
                                               'weibull',
                                               parameters = list(shape = 2,
                                                                 scale = 1.2),
                                               truncation = c(1, Inf))
  expect_true(all(difference < 1e-4))

  # negative-truncated
  difference <- compare_truncated_distribution(weibull,
                                               'weibull',
                                               parameters = list(shape = 2,
                                                                 scale = 1.2),
                                               truncation = c(-Inf, 2))
  expect_true(all(difference < 1e-4))

  # fully-truncated
  difference <- compare_truncated_distribution(weibull,
                                               'weibull',
                                               parameters = list(shape = 2,
                                                                 scale = 1.2),
                                               truncation = c(1, 2))
  expect_true(all(difference < 1e-4))

})

test_that('truncated exponential has correct densities', {

  source('helpers.R')

  # non-truncated
  difference <- compare_truncated_distribution(exponential,
                                               'exp',
                                               parameters = list(rate = 2),
                                               truncation = c(-Inf, Inf))
  expect_true(all(difference < 1e-4))

  # positive-truncated
  difference <- compare_truncated_distribution(exponential,
                                               'exp',
                                               parameters = list(rate = 2),
                                               truncation = c(1, Inf))
  expect_true(all(difference < 1e-4))

  # negative-truncated
  difference <- compare_truncated_distribution(exponential,
                                               'exp',
                                               parameters = list(rate = 2),
                                               truncation = c(-Inf, 2))
  expect_true(all(difference < 1e-4))

  # fully-truncated
  difference <- compare_truncated_distribution(exponential,
                                               'exp',
                                               parameters = list(rate = 2),
                                               truncation = c(1, 2))
  expect_true(all(difference < 1e-4))

})

test_that('truncated pareto has correct densities', {

  source('helpers.R')

  # non-truncated
  difference <- compare_truncated_distribution(preto,
                                               'preto',
                                               parameters = list(a_ = 1.9, b_ = 4.3),
                                               truncation = c(-Inf, Inf))
  expect_true(all(difference < 1e-4))

  # positive-truncated
  difference <- compare_truncated_distribution(preto,
                                               'preto',
                                               parameters = list(a_ = 1.9, b_ = 4.3),
                                               truncation = c(7.2, Inf))
  expect_true(all(difference < 1e-4))

  # negative-truncated
  difference <- compare_truncated_distribution(preto,
                                               'preto',
                                               parameters = list(a_ = 1.9, b_ = 4.3),
                                               truncation = c(-Inf, 21.3))
  expect_true(all(difference < 1e-4))

  # fully-truncated
  difference <- compare_truncated_distribution(preto,
                                               'preto',
                                               parameters = list(a_ = 1.9, b_ = 4.3),
                                               truncation = c(7.2, 21.3))
  expect_true(all(difference < 1e-4))

})

test_that('truncated student has correct densities', {

  source('helpers.R')

  # non-truncated
  difference <- compare_truncated_distribution(student,
                                               't_ls',
                                               parameters = list(df = 5,
                                                                 location = 2,
                                                                 scale = 3.4),
                                               truncation = c(-Inf, Inf))
  expect_true(all(difference < 1e-4))

  # positive-truncated
  difference <- compare_truncated_distribution(student,
                                               't_ls',
                                               parameters = list(df = 5,
                                                                 location = 2,
                                                                 scale = 3.4),
                                               truncation = c(1, Inf))
  expect_true(all(difference < 1e-4))

  # negative-truncated
  difference <- compare_truncated_distribution(student,
                                               't_ls',
                                               parameters = list(df = 5,
                                                                 location = 2,
                                                                 scale = 3.4),
                                               truncation = c(-Inf, 2))
  expect_true(all(difference < 1e-4))

  # fully-truncated
  difference <- compare_truncated_distribution(student,
                                               't_ls',
                                               parameters = list(df = 5,
                                                                 location = 2,
                                                                 scale = 3.4),
                                               truncation = c(1, 2))
  expect_true(all(difference < 1e-4))

})


test_that('truncated laplace has correct densities', {

  source('helpers.R')

  # non-truncated
  difference <- compare_truncated_distribution(laplace,
                                               'laplace',
                                               parameters = list(mu = 2,
                                                                 sigma = 3.4),
                                               truncation = c(-Inf, Inf))
  expect_true(all(difference < 1e-4))

  # positive-truncated
  difference <- compare_truncated_distribution(laplace,
                                               'laplace',
                                               parameters = list(mu = 2,
                                                                 sigma = 3.4),
                                               truncation = c(1, Inf))
  expect_true(all(difference < 1e-4))

  # negative-truncated
  difference <- compare_truncated_distribution(laplace,
                                               'laplace',
                                               parameters = list(mu = 2,
                                                                 sigma = 3.4),
                                               truncation = c(-Inf, 2))
  expect_true(all(difference < 1e-4))

  # fully-truncated
  difference <- compare_truncated_distribution(laplace,
                                               'laplace',
                                               parameters = list(mu = 2,
                                                                 sigma = 3.4),
                                               truncation = c(1, 2))
  expect_true(all(difference < 1e-4))

})

test_that('truncated beta has correct densities', {

  source('helpers.R')

  # non-truncated
  difference <- compare_truncated_distribution(beta,
                                               'beta',
                                               parameters = list(shape1 = 2.1,
                                                                 shape2 = 2.3),
                                               truncation = c(-Inf, Inf))
  expect_true(all(difference < 1e-4))

  # positive-truncated
  difference <- compare_truncated_distribution(beta,
                                               'beta',
                                               parameters = list(shape1 = 2.1,
                                                                 shape2 = 2.3),
                                               truncation = c(0.1, Inf))
  expect_true(all(difference < 1e-4))

  # negative-truncated
  difference <- compare_truncated_distribution(beta,
                                               'beta',
                                               parameters = list(shape1 = 2.1,
                                                                 shape2 = 2.3),
                                               truncation = c(-Inf, 0.2))
  expect_true(all(difference < 1e-4))

  # fully-truncated
  difference <- compare_truncated_distribution(beta,
                                               'beta',
                                               parameters = list(shape1 = 2.1,
                                                                 shape2 = 2.3),
                                               truncation = c(0.1, 0.2))
  expect_true(all(difference < 1e-4))

})

test_that('truncated cauchy has correct densities', {

  source('helpers.R')

  # non-truncated
  difference <- compare_truncated_distribution(cauchy,
                                               'cauchy',
                                               parameters = list(location = -1.3,
                                                                 scale = 2.3),
                                               truncation = c(-Inf, Inf))
  expect_true(all(difference < 1e-4))

  # positive-truncated
  difference <- compare_truncated_distribution(cauchy,
                                               'cauchy',
                                               parameters = list(location = -1.3,
                                                                 scale = 2.3),
                                               truncation = c(0.1, Inf))
  expect_true(all(difference < 1e-4))

  # negative-truncated
  difference <- compare_truncated_distribution(cauchy,
                                               'cauchy',
                                               parameters = list(location = -1.3,
                                                                 scale = 2.3),
                                               truncation = c(-Inf, 0.2))
  expect_true(all(difference < 1e-4))

  # fully-truncated
  difference <- compare_truncated_distribution(cauchy,
                                               'cauchy',
                                               parameters = list(location = -1.3,
                                                                 scale = 2.3),
                                               truncation = c(0.1, 0.2))
  expect_true(all(difference < 1e-4))

})

test_that('truncated logistic has correct densities', {

  source('helpers.R')

  # non-truncated
  difference <- compare_truncated_distribution(logistic,
                                               'logis',
                                               parameters = list(location = -1.3,
                                                                 scale = 2.3),
                                               truncation = c(-Inf, Inf))
  expect_true(all(difference < 1e-4))

  # positive-truncated
  difference <- compare_truncated_distribution(logistic,
                                               'logis',
                                               parameters = list(location = -1.3,
                                                                 scale = 2.3),
                                               truncation = c(0.1, Inf))
  expect_true(all(difference < 1e-4))

  # negative-truncated
  difference <- compare_truncated_distribution(logistic,
                                               'logis',
                                               parameters = list(location = -1.3,
                                                                 scale = 2.3),
                                               truncation = c(-Inf, 0.2))
  expect_true(all(difference < 1e-4))

  # fully-truncated
  difference <- compare_truncated_distribution(logistic,
                                               'logis',
                                               parameters = list(location = -1.3,
                                                                 scale = 2.3),
                                               truncation = c(0.1, 0.2))
  expect_true(all(difference < 1e-4))

})

test_that('truncated f has correct densities', {

  source('helpers.R')

  # non-truncated
  difference <- compare_truncated_distribution(f,
                                               'f',
                                               parameters = list(df1 = 1.3,
                                                                 df2 = 4.7),
                                               truncation = c(-Inf, Inf))
  expect_true(all(difference < 1e-4))

  # positive-truncated
  difference <- compare_truncated_distribution(f,
                                               'f',
                                               parameters = list(df1 = 1.3,
                                                                 df2 = 4.7),
                                               truncation = c(0.1, Inf))
  expect_true(all(difference < 1e-4))

  # negative-truncated
  difference <- compare_truncated_distribution(f,
                                               'f',
                                               parameters = list(df1 = 1.3,
                                                                 df2 = 4.7),
                                               truncation = c(-Inf, 0.2))
  expect_true(all(difference < 1e-4))

  # fully-truncated
  difference <- compare_truncated_distribution(f,
                                               'f',
                                               parameters = list(df1 = 1.3,
                                                                 df2 = 4.7),
                                               truncation = c(0.1, 0.2))
  expect_true(all(difference < 1e-4))

})

test_that('truncated chi squared has correct densities', {

  source('helpers.R')

  # non-truncated
  difference <- compare_truncated_distribution(chi_squared,
                                               'chisq',
                                               parameters = list(df = 9.3),
                                               truncation = c(-Inf, Inf))
  expect_true(all(difference < 1e-4))

  # positive-truncated
  difference <- compare_truncated_distribution(chi_squared,
                                               'chisq',
                                               parameters = list(df = 9.3),
                                               truncation = c(0.1, Inf))
  expect_true(all(difference < 1e-4))

  # negative-truncated
  difference <- compare_truncated_distribution(chi_squared,
                                               'chisq',
                                               parameters = list(df = 9.3),
                                               truncation = c(-Inf, 0.2))
  expect_true(all(difference < 1e-4))

  # fully-truncated
  difference <- compare_truncated_distribution(chi_squared,
                                               'chisq',
                                               parameters = list(df = 9.3),
                                               truncation = c(0.1, 0.2))
  expect_true(all(difference < 1e-4))

})
