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

#' @title Is object a `greta_mcmc_list`?
#' @param x An object that may be a greta_mcmc_list
#'
#' @param ... extra args (not currently used)
#' @returns logical TRUE/FALSE
#'
#' @export
is.greta_mcmc_list <- function(x, ...){
  inherits(x, "greta_mcmc_list")
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

#' Print method for greta MCMC list
#'
#' @param x greta mcmc list
#' @param ... extra args (currently not used)
#' @param n number of lines to print
#'
#' @return printed MCMC output
#' @export
print.greta_mcmc_list <- function(x, ..., n = 5){

  n_chain <- coda::nchain(x)
  n_iter <- coda::niter(x)
  n_thin <- coda::thin(x)
  cli::cli_h1("MCMC draws from {.pkg greta}")
  cli::cli_bullets(
    c(
      "*" = "Iterations = {n_iter}",
      "*" = "Chains  = {n_chain}",
      "*" = "Thinning  = {n_thin}"
    )
  )

  n_print <- getOption("greta.print_max") %||% n
  remaining_draws <- n_iter - n_print
  more_draws_than_can_print <- remaining_draws > 0
  draws_can_be_printed <- remaining_draws <= 0

  if (more_draws_than_can_print) {
    cli::cli_h1("Chain 1 (iterations 1...{n_print})")
  }

  if (draws_can_be_printed) {
    cli::cli_h1("Chain 1 (iterations 1...{n_iter})")
  }

  flat_mat <- as.matrix(x[[1]])

  draws_head <- head(flat_mat, n = n_print)

  print(draws_head)

  if (more_draws_than_can_print){
  cli::cli_alert_info(
    text = c(
      "i" = "{remaining_draws} more draws\n",
      "i" = "Use {.code print(n = ...)} to see more draws"
    )
  )
  }

  cli::cli_rule()

  cli::cli_alert_info(
    c("View {.pkg greta} draw chain {.param i} with:\n",
      "{.code greta_draws_object[[i]]}. \n",
      "E.g., view chain {.param 1} with: \n",
      "{.code greta_draws_object[[1]]}."
    )
  )

  cli::cli_alert_info(
    c("To see a summary of draws, run:\n",
      "{.code summary(greta_draws_object)}")
  )

}
