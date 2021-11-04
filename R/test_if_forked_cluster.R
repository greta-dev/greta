test_if_forked_cluster <- function(){
  dummy <- parallelly::isForkedChild()

  f <- future({
    if (parallelly::isForkedChild()) {
      msg <- cli::format_error(
        "parallel mcmc samplers cannot be run with a fork cluster"
      )
      stop(
        msg,
        call. = FALSE
      )
    }

    42
  })

  value(f)
}
