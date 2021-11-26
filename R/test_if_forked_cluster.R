test_if_forked_cluster <- function(){
  is_forked <- value(future(parallelly::isForkedChild()))
  if (is_forked) {
    msg <- cli::format_error(
      "parallel mcmc samplers cannot be run with a fork cluster"
    )
    stop(
      msg,
      call. = FALSE
    )
  }
}
