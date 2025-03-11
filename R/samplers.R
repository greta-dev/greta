#' @name samplers
#'
#' @title MCMC samplers
#' @description Functions to set up MCMC samplers and change the starting values
#'   of their parameters, for use in [mcmc()].
#'
#' @details During the warmup iterations of `mcmc`, some of these
#'   sampler parameters will be tuned to improve the efficiency of the sampler,
#'   so the values provided here are used as starting values.
#'
#' @return a `sampler` object that can be passed to [mcmc()].

NULL

# nolint start
#' @rdname samplers
#' @export
#'
#' @param Lmin minimum number of leapfrog steps (positive integer, Lmin > Lmax)
#' @param Lmax maximum number of leapfrog steps (positive integer, Lmax > Lmin)
#' @param epsilon leapfrog stepsize hyperparameter (positive, will be tuned)
#' @param diag_sd estimate of the posterior marginal standard deviations
#'   (positive, will be tuned).
#'
#' @details For `hmc()`, the number of leapfrog steps at each iteration is
#'   selected uniformly at random from between `Lmin` and `Lmax`.
#'   `diag_sd` is used to rescale the parameter space to make it more
#'   uniform, and make sampling more efficient.
hmc <- function(Lmin = 5, Lmax = 10, epsilon = 0.1, diag_sd = 1) {
  # nolint end
  obj <- list(
    parameters = list(
      Lmin = Lmin,
      Lmax = Lmax,
      epsilon = epsilon,
      diag_sd = diag_sd
    ),
    name = "hmc",
    class = hmc_sampler
  )
  class(obj) <- c("hmc sampler", "sampler")
  obj
}

#' @rdname samplers
#'
#' @details `rwmh()` creates a random walk Metropolis-Hastings sampler;  a
#'   a gradient-free sampling algorithm. The algorithm involves a proposal
#'   generating step `proposal_state = current_state + perturb` by a random
#'   perturbation, followed by Metropolis-Hastings accept/reject step. The class
#'   is implemented for uniform and normal proposals.
#'
#' @param proposal the probability distribution used to generate proposal states
#'
#' @export
rwmh <- function(
  proposal = c("normal", "uniform"),
  epsilon = 0.1,
  diag_sd = 1
) {
  proposal <- match.arg(proposal)

  obj <- list(
    parameters = list(
      proposal = proposal,
      epsilon = epsilon,
      diag_sd = diag_sd
    ),
    name = "rwmh",
    class = rwmh_sampler
  )
  class(obj) <- c("rwmh sampler", "sampler")
  obj
}

#' @rdname samplers
#'
#' @details `slice()` implements a multivariate slice sampling algorithm.
#'   The parameter `max_doublings` is not tuned during warmup.
#'
#' @param max_doublings the maximum number of iterations of the 'doubling'
#'   algorithm used to adapt the size of the slice
#'
#' @export
slice <- function(max_doublings = 5) {
  obj <- list(
    parameters = list(
      max_doublings = as.integer(max_doublings)[1]
    ),
    name = "slice",
    class = slice_sampler
  )
  class(obj) <- c("slice sampler", "sampler")
  obj
}

#' @noRd
#' @export
print.sampler <- function(x, ...) {
  values_text <- paste(
    names(x$parameters),
    prettyNum(x$parameters),
    sep = " = ",
    collapse = ", "
  )

  if (!nzchar(values_text)) values_text <- "None"

  parameters_text <- glue::glue(
    "
                                parameters:
                                  {values_text}
                                "
  )

  msg <- glue::glue(
    "{class(x)[1]} object with {parameters_text}"
  )

  cat(msg)
}


hmc_sampler <- R6Class(
  "hmc_sampler",
  inherit = sampler,
  public = list(
    parameters = list(
      Lmin = 10,
      Lmax = 20,
      epsilon = 0.005,
      diag_sd = 1
    ),
    accept_target = 0.651,

    define_tf_kernel = function(sampler_param_vec) {
      dag <- self$model$dag
      tfe <- dag$tf_environment

      free_state_size <- length(sampler_param_vec) - 2

      # TF1/2 check
      # this will likely get replaced...

      hmc_l <- sampler_param_vec[0]
      hmc_epsilon <- sampler_param_vec[1]
      hmc_diag_sd <- sampler_param_vec[2:(1 + free_state_size)]

      hmc_step_sizes <- tf$cast(
        x = tf$reshape(
          hmc_epsilon * (hmc_diag_sd / tf$reduce_sum(hmc_diag_sd)),
          shape = shape(free_state_size)
        ),
        dtype = tf$float64
      )
      # TF1/2 check
      # where is "free_state" pulled from, given that it is the
      # argument to this function, "generate_log_prob_function" ?
      # log probability function

      # build the kernel
      # nolint start

      sampler_kernel <- tfp$mcmc$HamiltonianMonteCarlo(
        target_log_prob_fn = dag$tf_log_prob_function_adjusted,
        step_size = hmc_step_sizes,
        num_leapfrog_steps = hmc_l
      )
      return(
        sampler_kernel
      )
    },
    sampler_parameter_values = function() {
      # random number of integration steps
      l_min <- self$parameters$Lmin
      l_max <- self$parameters$Lmax
      l <- sample(seq(l_min, l_max), 1)

      epsilon <- self$parameters$epsilon
      diag_sd <- matrix(self$parameters$diag_sd)

      # return named list for replacing tensors
      list(
        hmc_l = l,
        hmc_epsilon = epsilon,
        hmc_diag_sd = diag_sd
      )
    }
  )
)

rwmh_sampler <- R6Class(
  "rwmh_sampler",
  inherit = sampler,
  public = list(
    parameters = list(
      proposal = "normal",
      epsilon = 0.1,
      diag_sd = 1
    ),
    accept_target = 0.44,

    define_tf_kernel = function(sampler_param_vec) {
      # wrap this up into a function to extract these out
      free_state_size <- length(sampler_param_vec) - 1 # get it from dag object
      # e.g., length(dag$free_state)
      rwmh_epsilon <- sampler_param_vec[0]
      rwmh_diag_sd <- sampler_param_vec[1:(1 + free_state_size)]

      dag <- self$model$dag
      tfe <- dag$tf_environment

      tfe$rwmh_proposal <- switch(
        self$parameters$proposal,
        normal = tfp$mcmc$random_walk_normal_fn,
        uniform = tfp$mcmc$random_walk_uniform_fn
      )

      # TF1/2 check
      # I think a good portion of this code could be abstracted away
      # Perhaps from `rwmh_epsilon` to `new_state_fn`
      # could be passed
      # tfe$log_prob_fun <- dag$generate_log_prob_function()

      # tensors for sampler parameters
      # rwmh_epsilon <- tf$compat$v1$placeholder(dtype = tf_float())

      # need to pass in the value for this placeholder as a matrix (shape(n, 1))
      # rwmh_diag_sd <- tf$compat$v1$placeholder(
      #   dtype = tf_float(),
      #   # TF1/2 check
      # again what do we with with `free_state`?
      #   shape = shape(dim(free_state)[[2]], 1)
      # )

      # but it step_sizes must be a vector (shape(n, )), so reshape it
      rwmh_step_sizes <- tf$reshape(
        rwmh_epsilon * (rwmh_diag_sd / tf$reduce_sum(rwmh_diag_sd)),
        # TF1/2 check
        # what are we to do about `free_state` here?
        shape = shape(free_state_size)
      )

      new_state_fn <- tfe$rwmh_proposal(scale = rwmh_step_sizes)

      # build the kernel
      # nolint start
      sampler_kernel <- tfp$mcmc$RandomWalkMetropolis(
        target_log_prob_fn = dag$tf_log_prob_function_adjusted,
        new_state_fn = new_state_fn
      )
      return(
        sampler_kernel
      )
    },
    sampler_parameter_values = function() {
      epsilon <- self$parameters$epsilon
      diag_sd <- matrix(self$parameters$diag_sd)

      # return named list for replacing tensors
      list(
        rwmh_epsilon = epsilon,
        rwmh_diag_sd = diag_sd
      )
    }
  )
)

slice_sampler <- R6Class(
  "slice_sampler",
  inherit = sampler,
  public = list(
    parameters = list(
      max_doublings = NA
    ),
    tuning_interval = Inf,
    uses_metropolis = FALSE,

    define_tf_kernel = function(sampler_param_vec) {
      slice_max_doublings <- tensorflow::as_tensor(
        x = sampler_param_vec[0],
        dtype = tf$int32
      )

      dag <- self$model$dag
      tfe <- dag$tf_environment

      # build the kernel
      # nolint start
      sampler_kernel <- tfp$mcmc$SliceSampler(
        target_log_prob_fn = dag$tf_log_prob_function_adjusted,
        step_size = fl(1),
        max_doublings = slice_max_doublings
      )

      return(
        sampler_kernel
      )
    },
    sampler_parameter_values = function() {
      max_doublings <- as.integer(self$parameters$max_doublings)

      # return named list for replacing tensors
      list(slice_max_doublings = max_doublings)
    },

    # no additional here tuning
    tune = function(iterations_completed, total_iterations) {
    }
  )
)

adaptive_hmc_sampler <- R6Class(
  "adaptive_hmc_sampler",
  inherit = sampler,
  public = list(
    parameters = list(
      # Lmin = 10,
      # Lmax = 20,
      max_leapfrog_steps = 1000,
      # TODO clean up these parameter usage else where
      # epsilon = 0.005,
      # diag_sd = 1,
      # TODO some kind of validity check of method? Currently this can only be
      # "SNAPER".
      method = "SNAPER"
    ),
    accept_target = 0.651,

    define_tf_kernel = function(sampler_param_vec) {
      dag <- self$model$dag
      tfe <- dag$tf_environment

      free_state_size <- length(sampler_param_vec) - 2

      adaptive_hmc_max_leapfrog_steps <- tf$cast(
        x = sampler_param_vec[0],
        dtype = tf$int32
      )
      # TODO pipe that in properly
      n_warmup <- sampler_param_vec[1]
      # adaptive_hmc_epsilon <- sampler_param_vec[1]
      # adaptive_hmc_diag_sd <- sampler_param_vec[2:(1+free_state_size)]

      kernel_base <- tfp$experimental$mcmc$SNAPERHamiltonianMonteCarlo(
        target_log_prob_fn = dag$tf_log_prob_function_adjusted,
        step_size = 1,
        num_adaptation_steps = as.integer(self$warmup),
        max_leapfrog_steps = adaptive_hmc_max_leapfrog_steps
      )

      sampler_kernel <- tfp$mcmc$DualAveragingStepSizeAdaptation(
        inner_kernel = kernel_base,
        num_adaptation_steps = as.integer(self$warmup)
      )

      return(
        sampler_kernel
      )
    },
    sampler_parameter_values = function() {
      # random number of integration steps
      max_leapfrog_steps <- self$parameters$max_leapfrog_steps
      epsilon <- self$parameters$epsilon
      diag_sd <- matrix(self$parameters$diag_sd)
      method <- self$parameters$method

      # return named list for replacing tensors
      list(
        adaptive_hmc_max_leapfrog_steps = max_leapfrog_steps,
        # adaptive_hmc_epsilon = epsilon,
        # adaptive_hmc_diag_sd = diag_sd,
        method = method
      )
    }
  )
)
