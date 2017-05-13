context('truncated distributions')

test_that('truncated distributions define correct densities', {

  source('helpers.R')

  # non-truncated normal (via truncation trick)
  difference <- compare_truncated_distribution(normal,
                                               'norm',
                                               parameters = list(mean = -1,
                                                                 sd = 2.4),
                                               truncation = c(-Inf, Inf))
  expect_true(all(difference < 1e-4))

  # positive-truncated normal
  difference <- compare_truncated_distribution(normal,
                                               'norm',
                                               parameters = list(mean = -1,
                                                                 sd = 2.4),
                                               truncation = c(1, Inf))
  expect_true(all(difference < 1e-4))

  # negative-truncated normal
  difference <- compare_truncated_distribution(normal,
                                               'norm',
                                               parameters = list(mean = -1,
                                                                 sd = 2.4),
                                               truncation = c(-Inf, 0))
  expect_true(all(difference < 1e-4))

  # non-truncated normal (via truncation trick)
  difference <- compare_truncated_distribution(normal,
                                               'norm',
                                               parameters = list(mean = -1,
                                                                 sd = 2.4),
                                               truncation = c(-2, -1))
  expect_true(all(difference < 1e-4))

})
