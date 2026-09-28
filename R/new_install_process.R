new_install_process <- function(
  callr_process,
  timeout,
  stdout_file,
  stderr_file,
  stash_as,
  cli_start_msg = NULL,
  cli_end_msg = NULL
) {
  cli::cli_process_start(cli_start_msg)
  # convert max timeout from milliseconds into minutes
  timeout_minutes <- timeout * 1000 * 60
  r_callr_process <- callr::r_process$new(callr_process)
  r_callr_process$wait(timeout = timeout_minutes)

  status <- r_callr_process$get_exit_status()
  output_notes <- read_char(stdout_file)
  output_error <- read_char(stderr_file)

  # stashed before checking for failure, so the logfile has the output of a
  # failed step too, which is when it is needed
  greta_stash[[paste0(stash_as, "_notes")]] <- output_notes
  greta_stash[[paste0(stash_as, "_error")]] <- output_error

  # a step can write output and still fail, so its exit status decides; a NULL
  # status means it was still running at the timeout
  timed_out <- is.null(status)
  failed <- timed_out || status != 0 || !nzchar(output_notes)

  if (failed) {
    cli::cli_process_failed()
    log_written <- write_install_log_quietly()
    msg <- if (timed_out) {
      timeout_install_msg(timeout, output_error, log_written = log_written)
    } else {
      other_install_fail_msg(
        output_error,
        output_notes = output_notes,
        log_written = log_written
      )
    }
    # already formatted, so not cli_abort(), which would read the install output
    # in it as cli markup a second time
    rlang::abort(msg)
  }

  cli_process_done(msg_done = cli_end_msg)
  invisible()
}

# Failing to write the log must not hide the install error, so the reason is
# returned rather than raised
write_install_log_quietly <- function() {
  tryCatch(
    {
      suppressMessages(write_greta_install_log())
      check_result(TRUE)
    },
    error = function(e) check_result(FALSE, conditionMessage(e))
  )
}

# The failure message's pointer to the logfile, or why there is none
install_log_pointer <- function(log_written) {
  if (isTRUE(log_written)) {
    return(c(
      "i" = "The full output is in the logfile, \\
      {.path {greta_install_logfile()}}. Open it with \\
      {.run greta::open_greta_install_log()}."
    ))
  }
  reason <- check_reason(log_written)
  if (is.null(reason)) {
    return(character())
  }
  c("i" = paste0("greta could not write its logfile: ", cli_escape(reason)))
}
