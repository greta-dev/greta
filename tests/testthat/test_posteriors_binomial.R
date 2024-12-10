test_that("posterior is correct (binomial)", {
  skip_if_not(check_tf_version())

  # analytic solution to the posterior of the paramter of a binomial
  # distribution, with uniform prior
  n <- 100
  pos <- rbinom(1, n, runif(1))
  theta <- uniform(0, 1)
  distribution(pos) <- binomial(n, theta)
  m <- model(theta)

  draws <- get_enough_draws(m, hmc(), 2000, verbose = FALSE)

  samples <- as.matrix(draws)

  # analytic solution to posterior is beta(1 + pos, 1 + N - pos)
  shape1 <- 1 + pos
  shape2 <- 1 + n - pos

  # qq plot against true quantiles
  quants <- (1:99) / 100
  q_target <- qbeta(quants, shape1, shape2)
  q_est <- quantile(samples, quants)
  plot(q_target ~ q_est, main = "binomial posterior")
  abline(0, 1)

  n_draws <- round(coda::effectiveSize(draws))
  comparison <- rbeta(n_draws, shape1, shape2)
  suppressWarnings(test <- ks.test(samples, comparison))
  expect_gte(test$p.value, 0.01)
})
