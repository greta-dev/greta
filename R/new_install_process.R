new_install_process <- function(callr_process,
                                timeout,
                                stdout_file = NULL,
                                stderr_file = NULL,
                                cli_start_msg = NULL,
                                cli_end_msg = NULL){
  options(warning.length = 2000)
  cli::cli_process_start(cli_start_msg)
  # convert max timeout from milliseconds into minutes
  timeout_minutes <- timeout * 1000 * 60
  r_callr_process <- callr::r_process$new(callr_process)
  r_callr_process$wait(timeout = timeout_minutes)

  status <- r_callr_process$get_exit_status()
  output_notes <- read_char(stdout_file)
  # output_notes <- r_callr_process$read_output()
  no_output <- nchar(output_notes) == 0
  output_error <- read_char(stderr_file)
  # output_error <- r_callr_process$read_all_error_lines()

  # clean up stdout_file and stderr_file
  unlink(stdout_file)
  unlink(stderr_file)
  if (is.null(status)) {
    cli::cli_process_failed()
    stop(
      timeout_install_msg(timeout, output_error),
      call. = FALSE
    )
  } else if (no_output) {
    cli::cli_process_failed()
    stop(
      other_install_fail_msg(output_error),
      call. = FALSE
    )
  }

  cli_process_done(msg_done = cli_end_msg)

  return(
    list(output_notes = output_notes,
         status = status,
         no_output = no_output,
         output_error = output_error)
  )

}
