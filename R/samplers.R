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

#' @rdname samplers
#'
#' @param epsilon leapfrog stepsize hyperparameter (positive, will be tuned)
#' @param diag_sd estimate of the posterior marginal standard deviations
#'   (positive, will be tuned).
#' @param max_leapfrog_steps numeric. Default 1000. Maximum number of leapfrog
#'   steps used. The algorithm will determine the optimal number less than this.
#' @param method character length one. Currently can only be "SNAPER" but in
#'   the future this may expand to other adaptive samplers.
#' @details For `adaptive_hmc()`. The Lmin and Lmax parameters are learnt and so
#'   not provided in this. The number of chains cannot be less than 2, due to
#'   how adaptive HMC works. `diag_sd` is used to rescale the parameter space to
#'   make it more uniform, and make sampling more efficient.
#' @export
adaptive_hmc <- function(
  max_leapfrog_steps = 1000,
  epsilon = 0.1,
  diag_sd = 1,
  method = "SNAPER"
) {
  method <- rlang::arg_match(
    arg = method,
    values = "SNAPER"
  )

  # nolint end
  obj <- list(
    parameters = list(
      max_leapfrog_steps = max_leapfrog_steps,
      epsilon = epsilon,
      diag_sd = diag_sd
    ),
    class = adaptive_hmc_sampler
  )
  class(obj) <- c("adaptive_hmc_sampler", "sampler")
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

      # TODO we will wrap these up into adaptive sampling to learn better
      # similar to the adaptive_hmc - to put more things into TFP,
      # rather than going back and forth between R and TF

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
      # TODO HMC with momentum would get adaptation addition
      # one of the is dual averaging step size adapation (see adaptive_hmc)
      # the other is adapt_momentum
      # diagonal mass
      # https://www.tensorflow.org/probability/api_docs/python/tfp/experimental/mcmc/DiagonalMassMatrixAdaptation
      #

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

    # TODO
    # Eventually we will adapt the other samplers to have an interface
    # more in line with this - where we do the adaptation in tensorflow
    sampler_kernel = NULL,

    define_tf_kernel = function(sampler_param_vec) {
      dag <- self$model$dag
      tfe <- dag$tf_environment

      adaptive_hmc_max_leapfrog_steps <- tf$cast(
        x = self$parameters$max_leapfrog_steps,
        dtype = tf$int32
      )

      kernel_base <- tfp$experimental$mcmc$SNAPERHamiltonianMonteCarlo(
        target_log_prob_fn = dag$tf_log_prob_function_adjusted,
        step_size = 1,
        num_adaptation_steps = as.integer(self$warmup),
        max_leapfrog_steps = adaptive_hmc_max_leapfrog_steps
      )

      # learns the step size
      self$sampler_kernel <- tfp$mcmc$DualAveragingStepSizeAdaptation(
        inner_kernel = kernel_base,
        num_adaptation_steps = as.integer(self$warmup)
      )
    },

    current_state = NULL,

    # given MCMC kernel `sampler_kernel` and initial model parameter state
    # `free_state`, adapt the kernel tuning parameters whilst simultaneously
    # burning-in the model parameter state. Return both finalised kernel
    # tuning parameters and the burned-in model parameter state
    warm_up_sampler = function(sampler_kernel, free_state) {
      # get the predetermined adaptation period of the kernel

      # make the uncompiled function (with curried arguments)
      warmup_raw <- function() {
        tfp$mcmc$sample_chain(
          num_results = as.integer(sampler_kernel$num_adaptation_steps),
          current_state = free_state,
          kernel = sampler_kernel,
          return_final_kernel_results = TRUE,
          trace_fn = function(current_state, kernel_results) {
            kernel_results$step #kernel_results
          }
        )
      }

      # compile it into a concrete function
      warmup <- tensorflow::tf_function(warmup_raw)

      # execute it
      result <- warmup()

      # If no warmup, return the free state to use as initials in sampling,
      # Otherwise, use the last chunk of all_states, which has been tuned
      no_warmup <- as.integer(sampler_kernel$num_adaptation_steps) <= 0
      if (no_warmup) {
        self$current_state <- tensorflow::as_tensor(free_state)
      } else {
        self$current_state <- get_last_state(result$all_states)
      }

      # return the last (burned-in) state of the model parameters and the final
      # (tuned) kernel parameters
      self$warm_results <- list(
        kernel = sampler_kernel,
        kernel_results = result$final_kernel_results,
        current_state = self$current_state
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
    },

    # given a warmed up sampler object, return a compiled TF function
    # that generates a new burst of samples from samples from it
    sampler_function = NULL,
    make_sampler_function = function(sampler) {
      # if warmed up?
      warmed_up <- !is.null(self$warm_results)
      if (warmed_up) {
        sampler <- self$warm_results
      } else {
        sampler <- self$sampler_kernel
      }

      # make the uncompiled function (with curried arguments)
      sample_raw <- function(current_state, n_samples) {
        results <- tfp$mcmc$sample_chain(
          # how many iterations
          num_results = n_samples,
          # where to start from
          current_state = current_state,
          # kernel
          kernel = sampler$kernel,
          # tuned sampler settings
          previous_kernel_results = sampler$kernel_results,
          # what to trace (nothing)
          trace_fn = function(current_state, kernel_results) {
            # could compute badness here to save memory?
            # is.finite(kernel_results$inner_results$inner_results$inner_results$log_accept_ratio)
            kernel_results
          }
        )
        # return the parameter states and the kernel results
        list(
          all_states = results$all_states,
          kernel_results = results$trace
        )
      }

      # compile it into a concrete function and return
      self$sampler_function <- tensorflow::tf_function(
        sample_raw,
        list(
          as_tensorspec(self$current_state),
          tf$TensorSpec(shape = c(), dtype = tf$int32)
        )
      )
    },

    # prepare a slot to put the warmed up results into
    warm_results = NULL,

    run_warmup = function(
      n_samples,
      pb_update,
      ideal_burst_size,
      verbose
    ) {
      self$define_tf_kernel()
      self$warm_up_sampler(
        sampler_kernel = self$sampler_kernel,
        free_state = self$free_state
      )
    },

    run_sampling = function(
      n_samples,
      pb_update,
      ideal_burst_size,
      trace_batch_size,
      thin,
      verbose
    ) {
      # There's a couple of possibilities
      ## warmup already happened previously, so get already-warmup sampler
      ## warmup never happened, so get an unwarmed up sampler

      # warmup_status <- self$warmup_status()
      # self$warm_results
      # self$warmup
      # warmup_has_happened <- self$has_warmup_happened()
      # warmup

      perform_sampling <- n_samples > 0
      if (perform_sampling) {
        # on exiting during the main sampling period (even if killed by the
        # user) trace the free state values

        on.exit(self$trace_values(trace_batch_size), add = TRUE)

        # main sampling
        if (verbose) {
          pb_sampling <- create_progress_bar(
            phase = "sampling",
            iter = c(self$warmup, n_samples),
            pb_update = pb_update,
            width = self$pb_width
          )
          iterate_progress_bar(
            pb = pb_sampling,
            it = 0,
            rejects = 0,
            chains = self$n_chains,
            file = self$pb_file
          )
        } else {
          pb_sampling <- NULL
        }

        ### Adaptive start
        # split up warmup iterations into bursts of sampling
        burst_lengths <- as.integer(self$burst_lengths(
          n_samples,
          ideal_burst_size
        ))
        completed_iterations <- cumsum(burst_lengths)

        # maybe warm up a sampler
        if (is.null(self$warm_results)) {
          self$run_warmup()
        }
        # maybe compile a sampling function
        if (is.null(self$sampler_function)) {
          self$make_sampler_function()
        }
        # use this to compile the warmed version
        # sample <- self$make_sampler_function()

        current_state <- self$warm_results$current_state
        # trace <- array(NA, dim = c(n_samples, dim(current_state)))
        # track numerical rejections
        # n_bad <- 0

        for (burst in seq_along(burst_lengths)) {
          burst_size <- burst_lengths[burst]
          batch_results <- self$sampler_function(
            current_state = current_state,
            n_samples = burst_size
          )

          free_state_draws <- as.array(batch_results$all_states)

          # if there is one sample at a time, and it's rejected, conversion from
          # python back to R can drop a dimension, so handle that here. Ugh.
          if (n_dim(free_state_draws) != 3) {
            dim(free_state_draws) <- c(1, dim(free_state_draws))
          }

          self$last_burst_free_states <- split_chains(free_state_draws)

          n_draws <- nrow(free_state_draws)
          if (n_draws > 0) {
            free_state <- free_state_draws[n_draws, , , drop = FALSE]
            dim(free_state) <- dim(free_state)[-1]
            self$free_state <- free_state
          }

          self$update_rejection(batch_results)

          self$trace()

          # trace the MCMC results from this burst
          # burst_idx <- (burst - 1) * burst_size + seq_len(burst_size)
          # trace[burst_idx, , ] <- as.array(batch_results$all_states)

          # overwrite the current state
          current_state <- get_last_state(batch_results$all_states)

          if (verbose) {
            # update the progress bar/percentage log
            iterate_progress_bar(
              pb = pb_sampling,
              it = completed_iterations[burst],
              rejects = self$numerical_rejections,
              chains = self$n_chains,
              file = self$pb_file
            )

            self$write_percentage_log(
              total = n_samples,
              completed = completed_iterations[burst],
              stage = "sampling"
            )
          }
        }
      } # end sampling
      ### Adaptive end
    },

    update_rejection = function(results) {
      if (self$uses_metropolis) {
        # accumulate and report on the badness
        bad <- sum(bad_steps(results$kernel_results))
        self$numerical_rejections <- self$numerical_rejections + bad
      }
    }
  )
)
