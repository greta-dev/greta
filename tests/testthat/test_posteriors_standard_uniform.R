test_that("samplers are unbiased for standard uniform", {
  skip_if_not(check_tf_version())

  x <- uniform(0, 1)
  iid <- runif

  runif_checked <- check_samples(x, iid)

  qqplot_checked_samples(runif_checked)

  # do a formal hypothesis test
  stat <- ks_test_mcmc_vs_iid(runif_checked)

  expect_gte(stat$p.value, 0.01)
})
