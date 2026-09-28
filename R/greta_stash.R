# greta_stash holds state that outlives a single call -- python init flags,
# installation notes, samplers rescued from an aborted run. It is built here
# and attached to the namespace by .onLoad(), rather than at the top level of
# a file, so that no file has to be sourced before any other.
init_greta_stash <- function() {
  stash <- new.env()

  stash$python_has_been_initialised <- FALSE
  stash$deps_removed_this_session <- FALSE
  stash$numerical_messages <- c(
    "is not invertible",
    "Cholesky decomposition was not successful"
  )
  stash$callbacks <- list(parallel_progress = progress_bars)

  # nodes named by count and token: unique across and within sessions
  stash$node_count <- 0L
  session_hash <- rlang::hash(list(Sys.getpid(), Sys.time()))
  stash$session_token <- substr(session_hash, 1, 8)

  stash$tf_num_error <- cli::format_message(
    "If you are reading this, {.pkg greta} has not recorded a TensorFlow \\
    numerical error in this R session. They are cleared when R restarts."
  )

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
