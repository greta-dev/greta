new_install_process <- function(callr_process,
                                timeout,
                                stdout_file = NULL,
                                stderr_file = NULL,
                                cli_start_msg = NULL,
                                cli_end_msg = NULL){
  cli::cli_progress_step(msg = cli_start_msg,
                         spinner = TRUE)
  # convert max timeout from milliseconds into minutes
  timeout_minutes <- timeout * 1000 * 60
  r_callr_process <- callr::r_process$new(callr_process)
  cli::cli_progress_update()
  r_callr_process$wait(timeout = timeout_minutes)
  cli::cli_progress_update()

  status <- r_callr_process$get_exit_status()
  output_notes <- read_char(stdout_file)
  no_output <- nchar(output_notes) == 0
  output_error <- read_char(stderr_file)

  output_error <- output_error %||% "No output detected in stderr"

  if (is.null(status)) {
    cli::cli_progress_step(msg_failed = "Installation timed out")
    msg_timeout <- timeout_install_msg(timeout, output_error)
    cli::cli_abort(
      message = msg_timeout
    )
  } else if (no_output) {
    cli::cli_progress_step(msg_failed = "Installation failed")
    msg_other <- other_install_fail_msg(output_error)
    cli::cli_abort(
      msg_other
    )
  }

  cli::cli_progress_update()
  cli::cli_progress_step(msg_done = cli_end_msg)
  cli::cli_progress_done()

  return(
    list(output_notes = output_notes,
         status = status,
         no_output = no_output,
         output_error = output_error)
  )

}
