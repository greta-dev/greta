# ## This code was initially written to compare and understand if the
# ## bijectors for lkj were accurate/needed to change
# ## Overall we found that we didn't actually need to use the custom
# ## forward log det jacobian that we had for lkj
# ## Which was initially written as
# test_that("Log prob for lkj is correct", {
#
#   set.seed(2024-10-31-1027)
#
#   eta <- pi
#   dim <- 2
#
#   x <- rlkjcorr(n = 1, eta = eta, dimension = dim)
#   chol_x <- chol(x)
#
#   new_lkj_bijector <- function(){
#     steps <- list(
#       tfp$bijectors$Transpose(perm = 1:0),
#       tfp$bijectors$CorrelationCholesky()
#     )
#     bijector <- tfp$bijectors$Chain(steps)
#     bijector
#   }
#
#   new_bijector <- new_lkj_bijector()
#
#   free_state <- new_bijector$inverse(fl(chol_x))
#   free_state
#
#   # check that we can rebuild x
#
#   new_chol_x <- new_bijector$forward(free_state)
#   class(new_chol_x)
#   chol2symm(new_chol_x)
#   # this is the same as chol2symm - slight rounding difference due to signif digits
#   new_x <- tf$matmul(new_chol_x, new_chol_x, adjoint_a = TRUE)
#
#   ## we want to check that the log prob is correct!
#
#   x_g <- lkj_correlation(eta = eta, dimension = dim)
#   m_g <- model(x_g)
#   greta_log_prob <- m_g$dag$generate_log_prob_function()
#
#   # make it 2d and tranpose to ensure it is 1xnumber of free state elements
#   free_state_mat <- t(as.matrix(free_state))
#   greta_log_probs <- greta_log_prob(free_state_mat)
#
#   greta_log_probs
#
#   log_prob_adjustment <- new_bijector$forward_log_det_jacobian(free_state)
#   dist_chol_lkj <- tfp$distributions$CholeskyLKJ(
#     dimension = as.integer(dim),
#     concentration = fl(eta)
#   )
#
#   log_prob_raw <- dist_chol_lkj$log_prob(new_chol_x)
#   log_prob_raw_unnormalised <- dist_chol_lkj$unnormalized_log_prob(new_chol_x)
#
#   # this is the normalisation contant, which depends on what we set for `eta`
#   dist_chol_lkj$`_log_normalization`()
#
#   # this is the unnormalised probability
#   dist_chol_lkj$`_log_unnorm_prob`(new_chol_x, fl(eta))
#
#   # our implementation for log normalisation is (most likely?) wrong
#   lkj_log_normalising(eta, n = 1)
#
#
#   log_prob_raw
#   log_prob_raw_unnormalised
#
#   log_prob_adjusted <- log_prob_raw + log_prob_adjustment
#
#   as.numeric(log_prob_adjusted)
#   as.numeric(greta_log_probs$adjusted)
#
#   as.numeric(log_prob_raw)
#   as.numeric(greta_log_probs$unadjusted)
#
#   as.numeric(greta_log_probs$adjusted) - as.numeric(greta_log_probs$unadjusted)
#   as.numeric(log_prob_adjustment)
#
#   # use the old (current one)
#   old_bijector <- tf_correlation_cholesky_bijector()
#
#   old_log_prob_adjustment <- old_bijector$forward_log_det_jacobian(t(as.matrix(free_state)))
#
#   as.numeric(old_log_prob_adjustment)
#   as.numeric(greta_log_probs$adjusted) - as.numeric(greta_log_probs$unadjusted)
#
# })
