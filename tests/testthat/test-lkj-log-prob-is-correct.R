# test_that("Log prob for lkj is correct", {
#   # General process is:
#   # 1. Simulate a lkj draw x with rlkj()
#   # 2. Transform x to the equivalent free_state, using the bijector but running
#   # it in reverse
#   # 3. Run the log_prob() function on free_state
#   # 4. Run dlkj(..., log = TRUE) on x, and compare with result of step 3
#
#   # 1. Simulate a lkj draw x with rlkj() ----------------------------------
#   set.seed(2024-10-31-1027)
#
#   eta <- 1
#   dim <- 2
#
#   x <- rlkjcorr(n = 1, eta = eta, dimension = dim)
#   chol_x <- chol(x)
#
#   ## 2. Transform x to the equivalent free_state, using the bijector but
#   ## running it in reverse -----------------------------------------------------
#   # we need to get a free state that we can plug into log prob. We know that
#   # this free state matches the chol_x, so we can compare them later.
#
#   # old bijector?
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
#   a_bijector <- new_lkj_bijector()
#   free_state_new_bijector <- a_bijector$inverse(fl(chol_x))
#
#   # TODO
#   # these are the same! Do we care which bijector we use?
#   # We probably want the old one as it has a different log det jeacobian fun?
#   old_lkj_bijector <- tf_correlation_cholesky_bijector()
#   free_state_old_bijector <- old_lkj_bijector$inverse(fl(chol_x))
#   expect_equal(
#     as.numeric(free_state_old_bijector),
#     as.numeric(free_state_new_bijector)
#   )
#
#   # comparing log_det_jacobian of old and new
#   free_state_new_bijector
#   free_state_old_bijector
#   t(as.matrix(free_state_old_bijector))
#   free_state_old_bijector
#   old_lkj_bijector$forward_log_det_jacobian(t(as.matrix(free_state_old_bijector)))
#   a_bijector$forward_log_det_jacobian(free_state_old_bijector)
#
#   # TODO
#   # get the greta log prob function
#   # x_g <- lkj_correlation(eta = eta, dimension = dim)
#   # m_g <- model(x_g)
#   # greta_log_prob <- m_g$dag$generate_log_prob_function()
#   #
#   # free_state_mat <- t(as.matrix(free_state))
#   # log_probs <- greta_log_prob(free_state_mat)
#
#   # compare this with a direct calculation of what it should be, using TF
#
#   # lkj distribution for choleskies, matching what's in the greta code
#   lkj_cholesky_dist <- tfp$distributions$CholeskyLKJ(
#     dimension = as.integer(dim),
#     concentration = fl(eta)
#   )
#
#   # we should expect this to have 1s on the diagonal...
#   new_draws <- lkj_cholesky_dist$sample()
#
#   new_draws
#
#   tf$matmul(new_draws, new_draws, adjoint_b = TRUE)
#
#   lkj_dist <- tfp$distributions$LKJ(dimension = as.integer(dim),
#                                     concentration = fl(eta),
#                                     input_output_cholesky = FALSE)
#
#   lkj_dist$sample()
#
#   # same value for both old and new bijector
#   free_state <- free_state_old_bijector
#   # project forward from the free state
#   chol_x_new <- a_bijector$forward(free_state)
#   chol_x_new
#   chol2symm(chol_x_new)
#   x_new <- tf$matmul(new_draws, new_draws, adjoint_b = TRUE)
#   tf$matmul(new_draws, new_draws, adjoint_a = TRUE)
#   tf$matmul(chol_x_new, chol_x_new,adjoint_a = TRUE)
#   x_new
#   x
#
#   # compute the log density of the transformed variable, and the adjustment for
#   # change of support
#   log_density_raw <- lkj_cholesky_dist$log_prob(chol_x_new)
#   log_density_raw_new <- lkj_dist$log_prob(x_new)
#   adjustment <- a_bijector$forward_log_det_jacobian(free_state)
#   log_density <- log_density_raw + adjustment
#   log_density_new <- log_density_raw_new + adjustment
#
#   log_density
#
#   # greta log probs should match alternative way to calculate these
#   expect_equal(as.numeric(log_density), as.numeric(log_probs$adjusted))
#   # this is what the unadjusted version would look like, without accounting for
#   # the density adjustment due to the bijector
#   expect_equal(as.numeric(log_density_raw), as.numeric(log_probs$unadjusted))
#
#   # the difference between adjusted and unadjusted in greta is the same as
#   # the bijector adjustment calculated here, so the bijector adjustment
#   # calculated in greta must be correct, and it must be a problem with the
#   # distribution, not the bijector
#   expect_equal(
#     as.numeric(adjustment),
#     as.numeric(log_probs$adjusted - log_probs$unadjusted)
#   )
#
#   # check the TF distribution we use above against the R version (the same)
#   expect_equal(
#     as.numeric(log_density_raw),
#     dlkj_correlation(x, eta = eta, log = TRUE, dimension = dim)
#   )
#
#   expect_equal(
#     as.numeric(log_density_raw_new),
#     dlkj_correlation(x, eta = eta, log = TRUE, dimension = dim)
#   )
#
# })
