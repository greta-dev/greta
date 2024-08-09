# set.seed(2020 - 02 - 11)
#
# test_that("numerical issues are handled in mcmc", {
#   skip_if_not(check_tf_version())
#
#   # this should have a cholesky decomposition problem at some point
#   alpha <- normal(0, 1)
#   # x <- matrix(rnorm(6), 3, 2)
#   x <- matrix(rnorm(12), 4, 3)
#   # y <- t(rnorm(3))
#   y <- t(rnorm(4))
#   z <- alpha * x
#   sigma <- z %*% t(z)
#   # distribution(y) <- multivariate_normal(zeros(1, 3), sigma)
#   distribution(y) <- multivariate_normal(zeros(1, 4), sigma)
#   m <- model(alpha)
#
#   # running with bursts should error informatively
#   expect_snapshot(
#     error = TRUE,
#     draws <- mcmc(m, verbose = FALSE)
#   )
#
#   # setting one_by_one = TRUE should handle those errors as bad samples
#   expect_no_error(draws <- mcmc(m,
#                                 warmup = 100, n_samples = 10,
#                                 one_by_one = TRUE,
#                                 verbose = FALSE
#   ))
# })
