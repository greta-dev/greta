Sys.setenv("RELEASE_CANDIDATE" = "true")

test_that("samplers are unbiased for Wishart", {
  skip_if_not(check_tf_version())

  skip_if_not_release()

  sigma <- matrix(
    data = c(1.2, 0.7, 0.7, 2.3),
    nrow = 2,
    ncol = 2
  )

  df <- 4

  x <- wishart(df, sigma)[1, 2]

  iid <- function(n) {
    rWishart(n, df, sigma)[1, 2, ]
  }

  wishart_checked <- check_samples(
    x = x,
    iid_function = iid,
    one_by_one = TRUE,
    thin = 5
  )


  # do the plotting
  qqplot_checked_samples(wishart_checked)

  # do a formal hypothesis test
  stat <- ks_test_mcmc_vs_iid(wishart_checked)

  expect_gte(stat$p.value, 0.01)
})
