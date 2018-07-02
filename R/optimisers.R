#' @name optimisers
#'
#' @title optimisation methods
#' @description Functions to set up optimisers (which find parameters that
#'   maximise the joint density of a model) and change their tuning parameters,
#'   for use in \code{\link{opt}()}.
#'
#' @return an \code{optimiser} object that can be passed to \code{\link{mcmc}}.
NULL

#' @rdname optimisers
#' @export
#'
#' @param learning_rate the size of steps (in parameter space) towards the
#'   optimal value
#' @param initial_accumulator_value initial value of the 'accumulator' used to
#'   tune the algorithm
#'
adagrad <- function (learning_rate = 0.8,
                     initial_accumulator_value = 0.1) {

  obj <- list(parameters = list(learning_rate = learning_rate,
                                initial_accumulator_value = initial_accumulator_value),
              tf_optimiser = "tf$train$AdagradOptimizer",
              type = "tensorflow")
  class(obj) <- c("adagrad optimiser", "optimiser")
  obj

}


#' @rdname optimisers
#' @export
#'
#'
bfgs <- function () {

  obj <- list(parameters = list(),
              method = "bfgs",
              type = "scipy")
  class(obj) <- c("bfgs optimiser", "optimiser")
  obj

}

#' @noRd
#' @export
print.optimiser <- print.sampler
