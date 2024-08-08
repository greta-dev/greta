greta_note_msg <- cli::format_message(
  c(
    "If you are reading this, the {.pkg greta} installation or error notes \\
    have been wiped."
    )
)

greta_stash$install_miniconda_notes <- greta_note_msg
greta_stash$conda_create_notes <- greta_note_msg
greta_stash$conda_install_notes <- greta_note_msg
greta_stash$tf_num_error <- greta_note_msg

#' @title Retrieve python messages.
#'
#' @description
#'  These functions retrieve specific python error messages that might
#'   come up during greta use.
#'
#' @rdname stash-notes
#' @export
#' @examples
#' \dontrun{
#' greta_notes_tf_error()
#' }
greta_notes_tf_num_error <- function() {
  cat(greta_stash$tf_num_error)
}
