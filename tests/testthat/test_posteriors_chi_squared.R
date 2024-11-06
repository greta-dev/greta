test_that("samplers are unbiased for chi-squared", {
  skip_if_not(check_tf_version())

  df <- 5
  x <- chi_squared(df)
  iid <- function(n) rchisq(n, df)

  chi_squared_checked <- check_samples(x = x,
                                       iid_function = iid,
                                       sampler = hmc())

  # do the plotting
  qqplot_checked_samples(chi_squared_checked)

  # do a formal hypothesis test
  stat <- ks_test_mcmc_vs_iid(chi_squared_checked)

  expect_gte(stat$p.value, 0.01)
})
