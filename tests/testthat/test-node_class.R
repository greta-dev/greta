test_that("building greta arrays leaves R's random number stream alone", {
  skip_if_not(check_tf_version())

  withr::local_seed(1)
  before <- rng_seed()
  x <- normal(0, 1)
  model(x * 2)
  expect_identical(rng_seed(), before)
})

test_that("greta arrays built after the same seed stay distinct", {
  skip_if_not(check_tf_version())

  withr::local_preserve_seed()

  # identical priors, so naming nodes by their contents would merge them too -
  # greta-dev/greta#366
  a <- withr::with_seed(1, normal(0, 1))
  b <- withr::with_seed(1, normal(0, 1))

  draws <- calculate(a, b, nsim = 10)
  expect_false(identical(draws$a, draws$b))
})

test_that("a greta array read back from another session stays distinct", {
  skip_if_not(check_tf_version())
  skip_on_cran()

  # every session counts nodes from 1, which the session token keeps apart.
  # Both halves run in fresh sessions: this one has counted too far to clash,
  # so the test would pass without the token
  rds <- withr::local_tempfile(fileext = ".rds")
  in_fresh_greta(function(rds) saveRDS(normal(0, 1), rds), rds)

  merged <- in_fresh_greta(
    function(rds) {
      a <- readRDS(rds)
      b <- normal(0, 1)
      draws <- calculate(a, b, nsim = 10)
      identical(draws$a, draws$b)
    },
    rds
  )
  expect_false(merged)
})
