Sys.setenv("RELEASE_CANDIDATE" = "false")

# run geweke tests on this model:
# theta ~ normal(mu1, sd1)
# x[i] ~ normal(theta, sd2)
# for i in N

n <- 10

mu1 <- 0
sd1 <- 2
sd2 <- 1

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

test_that("hmc sampler passes geweke tests", {
  skip_if_not(check_tf_version())
  skip_if_not_release()

  # run tests on all available samplers
  geweke_hmc <- check_geweke(
    sampler = hmc(),
    model = model,
    data = x,
    p_theta = p_theta,
    p_x_bar_theta = p_x_bar_theta,
    thin = 5
  )

  geweke_qq(geweke_hmc, title = "HMC Geweke test")

  geweke_stat_hmc <- geweke_ks(geweke_hmc)

  testthat::expect_gte(geweke_stat_hmc$p.value, 0.005)
})

test_that("rwmh sampler passes geweke tests", {
  skip_if_not(check_tf_version())
  skip_if_not_release()

  geweke_rwmh <- check_geweke(
    sampler = rwmh(),
    model = model,
    data = x,
    p_theta = p_theta,
    p_x_bar_theta = p_x_bar_theta,
    warmup = 2000,
    thin = 5
  )

  geweke_qq(geweke_rwmh, title = "RWMH Geweke test")

  geweke_stat_rwmh <- geweke_ks(geweke_rwmh)

  testthat::expect_gte(geweke_stat_rwmh$p.value, 0.005)
})

test_that("slice sampler passes geweke tests", {
  skip_if_not(check_tf_version())

  skip_if_not_release()

  geweke_slice <- check_geweke(
    sampler = slice(),
    model = model,
    data = x,
    p_theta = p_theta,
    p_x_bar_theta = p_x_bar_theta
  )

  geweke_qq(geweke_slice, title = "slice sampler Geweke test")

  geweke_stat_slice <- geweke_ks(geweke_slice)

  testthat::expect_gte(geweke_stat_slice$p.value, 0.005)
})

test_that("adaptive hmc sampler passes geweke tests", {
  skip_if_not(check_tf_version())

  skip_if_not_release()

  geweke_adaptive_hmc <- check_geweke(
    sampler = adaptive_hmc(),
    model = model,
    data = x,
    p_theta = p_theta,
    p_x_bar_theta = p_x_bar_theta,
    chains = 10,
    niter = 200,
    warmup = 2000,
    thin = 5
  )

  geweke_qq(geweke_adaptive_hmc, title = "adaptive hmc sampler Geweke test")

  geweke_stat_adaptive_hmc <- geweke_ks(geweke_adaptive_hmc)

  testthat::expect_gte(geweke_stat_adaptive_hmc$p.value, 0.005)
})
