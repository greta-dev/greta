Sys.setenv("RELEASE_CANDIDATE" = "false")

## TF1/2 - method for this test needs to be updated for TF2
## See https://github.com/greta-dev/greta/issues/720
test_that("samplers pass geweke tests", {
  skip_if_not(check_tf_version())

  skip_if_not_release()

  # nolint start
  # run geweke tests on this model:
  # theta ~ normal(mu1, sd1)
  # x[i] ~ normal(theta, sd2)
  # for i in N
  # nolint end

  n <- 10
  mu1 <- rnorm(1, 0, 3)
  sd1 <- rlnorm(1)
  sd2 <- rlnorm(1)

  # prior (n draws)
  p_theta <- function(n) {
    rnorm(n, mu1, sd1)
  }

  # likelihood
  p_x_bar_theta <- function(theta) {
    rnorm(n, theta, sd2)
  }

  # define the greta model (single precision for slice sampler)
  x <- as_data(rep(0, n))
  greta_theta <- normal(mu1, sd1)
  distribution(x) <- normal(greta_theta, sd2)
  model <- model(greta_theta, precision = "single")

  # run tests on all available samplers
  check_geweke(
    sampler = hmc(),
    model = model,
    data = x,
    p_theta = p_theta,
    p_x_bar_theta = p_x_bar_theta,
    title = "HMC Geweke test"
  )

  check_geweke(
    sampler = rwmh(),
    model = model,
    data = x,
    p_theta = p_theta,
    p_x_bar_theta = p_x_bar_theta,
    warmup = 2000,
    title = "RWMH Geweke test"
  )

  check_geweke(
    sampler = slice(),
    model = model,
    data = x,
    p_theta = p_theta,
    p_x_bar_theta = p_x_bar_theta,
    title = "slice sampler Geweke test"
  )
})
