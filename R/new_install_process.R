new_install_process <- function(callr_process,
                                timeout,
                                cli_start_msg = NULL,
                                cli_end_msg = NULL){
  cli::cli_process_start(cli_start_msg)
  # convert max timeout from milliseconds into minutes
  timeout_minutes <- timeout * 1000 * 60
  r_callr_process <- callr::r_process$new(callr_process)
  r_callr_process$wait(timeout = timeout_minutes)

  status <- r_callr_process$get_exit_status()
  output_notes <- r_callr_process$read_output()
  no_output <- nchar(output_notes) == 0
  output_error <- r_callr_process$read_all_error_lines()

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
