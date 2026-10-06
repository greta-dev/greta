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
#' @param Lmin minimum number of leapfrog steps (positive integer, no larger
#'   than Lmax)
#' @param Lmax maximum number of leapfrog steps (positive integer, no smaller
#'   than Lmin)
#' @param epsilon leapfrog stepsize hyperparameter (positive, will be tuned)
#' @param diag_sd estimate of the posterior marginal standard deviations
#'   (positive, will be tuned).
#'
#' @details For `hmc()`, the number of leapfrog steps is drawn uniformly at
#'   random from between `Lmin` and `Lmax`, afresh at every iteration.
#'   `diag_sd` is used to rescale the parameter space to make it more
#'   uniform, and make sampling more efficient.
hmc <- function(Lmin = 5, Lmax = 10, epsilon = 0.1, diag_sd = 1) {
  # nolint end
  check_leapfrog_range(Lmin, Lmax)
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

  if (!nzchar(values_text)) {
    values_text <- "None"
  }

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

tune_tf <- R6Class(
  "tune_tf",
  inherit = sampler
)

tune_r <- R6Class(
  "tune_r",
  inherit = sampler
)

hmc_sampler <- R6Class(
  "hmc_sampler",
  inherit = tune_r,
  public = list(
    parameters = list(
      Lmin = 10,
      Lmax = 20,
      epsilon = 0.005,
      diag_sd = 1
    ),
    accept_target = 0.651,

    define_tf_kernel = function(sampler_param_vec, seed) {
      dag <- self$model$dag
      tfe <- dag$tf_environment

      free_state_size <- length(sampler_param_vec) - 3

      # the sampler's parameters arrive as one flat vector because that is what
      # the traced function's TensorSpec takes; this unpacks it again
      hmc_l_min <- tf$cast(sampler_param_vec[0], tf$int32)
      hmc_l_max <- tf$cast(sampler_param_vec[1], tf$int32)
      hmc_epsilon <- sampler_param_vec[2]
      hmc_diag_sd <- sampler_param_vec[3:(2 + free_state_size)]

      # the sampler loop builds a kernel every iteration, each from its own
      # seed, so the leapfrog count is drawn afresh every iteration. A count
      # fixed for many iterations would let a chain whose step size and count
      # trace a closed orbit stop mixing
      hmc_l <- tf$random$stateless_uniform(
        shape = shape(),
        seed = seed,
        minval = hmc_l_min,
        maxval = hmc_l_max + 1L,
        dtype = tf$int32
      )

      hmc_step_sizes <- tf$cast(
        x = tf$reshape(
          hmc_epsilon * (hmc_diag_sd / tf$reduce_sum(hmc_diag_sd)),
          shape = shape(free_state_size)
        ),
        dtype = tf$float64
      )
      # the kernel never sees free_state: its size comes from the parameter
      # vector above, and TFP passes the current state to target_log_prob_fn
      # when it calls it

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
      epsilon <- self$parameters$epsilon
      diag_sd <- matrix(self$parameters$diag_sd)

      # return named list for replacing tensors
      list(
        hmc_l_min = self$parameters$Lmin,
        hmc_l_max = self$parameters$Lmax,
        hmc_epsilon = epsilon,
        hmc_diag_sd = diag_sd
      )
    },
    tuning_indices = function() {
      list(epsilon = 2L, diag_sd = 2L + seq_len(self$n_free))
    }
  )
)

rwmh_sampler <- R6Class(
  "rwmh_sampler",
  inherit = tune_r,
  public = list(
    parameters = list(
      proposal = "normal",
      epsilon = 0.1,
      diag_sd = 1
    ),
    accept_target = 0.44,

    define_tf_kernel = function(sampler_param_vec, seed) {
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

      # step_sizes must be a vector, shape(n, ), so reshape it. As in the HMC
      # kernel, free_state is not needed: the size comes from the parameter
      # vector, and TFP passes the state to target_log_prob_fn
      rwmh_step_sizes <- tf$reshape(
        rwmh_epsilon * (rwmh_diag_sd / tf$reduce_sum(rwmh_diag_sd)),
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
    },
    tuning_indices = function() {
      list(epsilon = 0L, diag_sd = seq_len(self$n_free))
    }
  )
)

slice_sampler <- R6Class(
  "slice_sampler",
  inherit = tune_r,
  public = list(
    parameters = list(
      max_doublings = NA
    ),
    tuning_interval = Inf,
    uses_metropolis = FALSE,

    # TFP's slice sampler loses a batch dimension of one inside its own while
    # loop, and then errors that the shape changed between iterations, so a
    # single chain is traced for any number of rows
    free_state_signature = function() {
      n_rows <- if (self$n_chains > 1) as.integer(self$n_chains) else NULL
      self$model$dag$free_state_signature(n_rows = n_rows)[[1]]
    },

    define_tf_kernel = function(sampler_param_vec, seed) {
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
    }
  )
)
