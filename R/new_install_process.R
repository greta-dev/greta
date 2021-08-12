new_install_process <- function(callr_process,
                                timeout,
                                cli_start_message){
  cli::cli_process_start(cli_start_message)
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

  return(
    list(output_notes = output_notes,
         status = status,
         no_output = no_output,
         output_error = output_error)
  )

}
#
# callr_install_miniconda <- callr::r_process_options(
#   func = function(){
#     reticulate::install_miniconda()
#   }
# )
#

# if this function doesn't fail, then this code here can be run?
  install_miniconda_process <- new_install_process(
    callr_process = callr_install_miniconda,
    timeout = 5,
    cli_start_message = "No {.pkg miniconda} detected, installing \\
                      {.pkg miniconda}, this may take a minute."

    )
  # because it should fail here
  # I'm just not sure if the `cli::cli_process_failed()` will work when that
  # is inside a function
  # in theory it should...just work?
  cli_process_done(msg_done = "{.pkg miniconda} installed!")
  greta_stash$miniconda_notes <- install_miniconda_process$output_notes
  cli_ul("To see full installation notes run:")
  cli_ul("{.code greta_notes_install_miniconda()}")
