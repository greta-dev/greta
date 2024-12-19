# Currently takes about 30 seconds on an M1 mac
test_that("samplers are unbiased for bivariate normals", {
  skip_if_not(check_tf_version())

  hmc_mvn_samples <- check_mvn_samples(sampler = hmc())
  expect_lte(max(hmc_mvn_samples), stats::qnorm(0.99))

  rwmh_mvn_samples <- check_mvn_samples(sampler = rwmh())
  expect_lte(max(rwmh_mvn_samples), stats::qnorm(0.99))

  slice_mvn_samples <- check_mvn_samples(sampler = slice())
  expect_lte(max(rwmh_mvn_samples), stats::qnorm(0.99))
})
