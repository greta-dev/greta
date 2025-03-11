test_if_forked_cluster <- function() {
  is_forked <- value(future(parallelly::isForkedChild()))
  if (is_forked) {
    cli::cli_abort(
      "parallel mcmc samplers cannot be run with a fork cluster"
    )
  }
}
