test_that("Log prob for lkj is correct", {
  # General process is:
  # 1. Simulate a lkj draw x with rWish()
  # 2. Transform x to the equivalent free_state, using the bijector but running
  # it in reverse
  # 3. Run the log_prob() function on free_state
  # 4. Run dWish(..., log = TRUE) on x, and compare with result of step 3

  # 1. Simulate a lkj draw x with rWish() ----------------------------------
  set.seed(2024-10-28-1100)

  # these are more likely to result in a mix of negative and positive free state
  # values
  eta <- 1
  dim <- 2

  x <- rlkjcorr(n = 1, eta = eta, dimension = dim)
  x
  chol_x <- chol(x)

  ## 2. Transform x to the equivalent free_state, using the bijector but
  ## running it in reverse -----------------------------------------------------
  ### we need to get a free state that we can plug into log prob. We know that this free state matches the chol_x, so we can compare them later.

  new_lkj_bijector <- function(){
    steps <- list(
      tfp$bijectors$Transpose(perm = 1:0),
      tfp$bijectors$CorrelationCholesky()
    )
    bijector <- tfp$bijectors$Chain(steps)
    bijector
  }

  a_bijector <- new_lkj_bijector()
  free_state <- a_bijector$inverse(fl(chol_x))
  # get the greta log prob function
  x_g <- lkj_correlation(eta = eta, dimension = dim)
  m_g <- model(x_g)
  greta_log_prob <- m_g$dag$generate_log_prob_function()

  free_state_mat <- t(as.matrix(free_state))
  log_probs <- greta_log_prob(free_state_mat)

  # compare this with a direct calculation of what it should be, using TF

  # wishart distribution for choleskies, matching what's in the greta code
  lkj_cholesky_dist <- tfp$distributions$CholeskyLKJ(
    dimension = as.integer(dim),
    concentration = fl(eta)
  )
  lkj_dist <- tfp$distributions$LKJ(dimension = as.integer(dim),
                                    concentration = fl(eta),
                                    input_output_cholesky = FALSE)

  # project forward from the free state
  chol_x_new <- a_bijector$forward(free_state)
  x_new <- tf_chol2symm(chol_x_new)
  # compute the log density of the transformed variable, and the adjustment for
  # change of support
  ### njt log_density_unadjusted?
  log_density_raw <- lkj_cholesky_dist$log_prob(chol_x_new)
  log_density_raw_new <- lkj_dist$log_prob(x_new)
  adjustment <- a_bijector$forward_log_det_jacobian(free_state)
  log_density <- log_density_raw + adjustment

  # greta log probs should match alternative way to calculate these
  expect_equal(as.numeric(log_density), as.numeric(log_probs$adjusted))
  # this is what the unadjusted version would look like, without accounting for
  # the density adjustment due to the bijector
  expect_equal(as.numeric(log_density_raw), as.numeric(log_probs$unadjusted))

  # the difference between adjusted and unadjusted in greta is the same as
  # the bijector adjustment calculated here, so the bijector adjustment
  # calculated in greta must be correct, and it must be a problem with the
  # distribution, not the bijector
  expect_equal(
    as.numeric(adjustment),
    as.numeric(log_probs$adjusted - log_probs$unadjusted)
  )

  # check the TF distribution we use above against the R version (the same)
  expect_equal(
    as.numeric(log_density_raw),
    dlkj_correlation(x, eta = eta, log = TRUE, dimension = dim)
  )

})
