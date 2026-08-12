# greta_stash holds state that outlives a single call -- python init flags,
# installation notes, samplers rescued from an aborted run. It is built here
# and attached to the namespace by .onLoad(), rather than at the top level of
# a file, so that no file has to be sourced before any other.
init_greta_stash <- function() {
  stash <- new.env()

  greta_note_msg <- cli::format_message(
    c(
      "If you are reading this, the {.pkg greta} installation or error notes \\
      have been wiped. This likely means that installation has not happened, \\
      or it has happened and you've restarted R. See `?install_greta_deps()` \\
      for more information."
    )
  )

  stash$python_has_been_initialised <- FALSE
  stash$deps_removed_this_session <- FALSE
  stash$numerical_messages <- c(
    "is not invertible",
    "Cholesky decomposition was not successful"
  )
  stash$callbacks <- list(parallel_progress = progress_bars)

  stash$install_miniconda_notes <- greta_note_msg
  stash$install_miniconda_error <- greta_note_msg
  stash$conda_create_notes <- greta_note_msg
  stash$conda_create_error <- greta_note_msg
  stash$conda_install_notes <- greta_note_msg
  stash$conda_install_error <- greta_note_msg
  stash$tf_num_error <- greta_note_msg

  stash
}

#' @title Retrieve python messages.
#'
#' @description
#'  These functions retrieve specific python error messages that might
#'   come up during greta use.
#'
#' @rdname stash-notes
#' @return Invisibly returns `NULL`; called for its side effect of printing
#'   the stored message.
#' @export
#' @examples
#' \dontrun{
#' greta_notes_tf_num_error()
#' }
greta_notes_tf_num_error <- function() {
  # wrap in paste0 to remove list properties
  message(paste0(greta_stash$tf_num_error))
}
