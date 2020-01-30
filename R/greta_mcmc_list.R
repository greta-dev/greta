# define a new S3 class of the output of greta::mcmc(), inheriting from coda's
# mcmc.list

as_greta_mcmc_list <- function(x, model_info) {

  # add the raw draws as an attribute
  attr(x, "model_info") <- model_info
  class(x) <- c("greta_mcmc_list", class(x))
  x

}

# for window (and any other function that modifies the object), apply the same
# to the free state draws

# Begin Exclude Linting
#' @export
#' @importFrom stats window
#' @noRd
window.greta_mcmc_list <- function(x, start, end, thin, ...) {
# End Exclude Linting %>%
  model_info <- attr(x, "model_info")
  model_info$raw_draws <- window(model_info$raw_draws, start, end, thin, ...)
  x <- NextMethod(x)
  as_greta_mcmc_list(x, model_info)
}

# add new methods for these generics, to make them look nicer and be more
# user-friendly:
# print, plot, summary
