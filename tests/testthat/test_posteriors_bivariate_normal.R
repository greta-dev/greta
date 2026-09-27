# Currently takes about 30 seconds on an M1 mac

# Each sampler's posterior summaries are compared with the truth in Monte Carlo
# standard errors, estimated by batch means with batch size sqrt(n) (mcse() in
# helpers.R). The method, and why sqrt(n), is in Flegal and Jones (2010) Batch
# means and spectral variance estimators in Markov chain Monte Carlo. Annals of
# Statistics 38(2):1034-1070. https://doi.org/10.1214/09-AOS735
test_that("samplers are unbiased for bivariate normals", {
  skip_if_not(check_tf_version())
  skip_on_os("windows")

  # seeded here rather than inheriting whatever random state earlier test files
  # left. set.seed() alone does not reach the sampler until greta-dev/greta#285
  # is fixed, so TensorFlow is seeded too
  seed <- as.integer(2026 - 09 - 27)
  withr::local_seed(seed)
  tensorflow::tf$random$set_seed(seed)

  # each score is absolute, so each tail gets half of the error rate, which is
  # then split across the five scores
  false_positive_rate <- 0.01
  n_scores <- 5
  threshold <- stats::qnorm(1 - false_positive_rate / (2 * n_scores))

  hmc_errors <- check_mvn_samples(sampler = hmc())
  rwmh_errors <- check_mvn_samples(sampler = rwmh())
  slice_errors <- check_mvn_samples(sampler = slice())

  expect_lte(max(hmc_errors), threshold)
  expect_lte(max(rwmh_errors), threshold)
  expect_lte(max(slice_errors), threshold)
})
