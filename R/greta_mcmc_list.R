# define a new S3 class of the output of greta::mcmc(), inheriting from coda's
# mcmc.list

as_greta_mcmc_list <- function(x, model_info) {

  # add the raw draws as an attribute
  attr(x, "model_info") <- model_info
  class(x) <- c("greta_mcmc_list", class(x))
  x

}

# nolint start
#' @export
#' @importFrom coda as.mcmc.list
#' @noRd
as.mcmc.list.greta_mcmc_list <- function(x, ...) {
  # nolint end
  attr(x, "model_info") <- NULL
  classes <- class(x)
  class(x) <- classes[classes != "greta_mcmc_list"]
  x
}


# for window (and any other function that modifies the object), apply the same
# to the free state draws

# nolint start
#' @export
#' @importFrom stats window
#' @noRd
window.greta_mcmc_list <- function(x, start, end, thin, ...) {
  # nolint end
  model_info <- attr(x, "model_info")
  model_info$raw_draws <- window(model_info$raw_draws, start, end, thin, ...)
  x <- NextMethod(x)
  as_greta_mcmc_list(x, model_info)
}

# add new methods for these generics, to make them look nicer and be more
# user-friendly:
# print, plot, summary
