greta_note_msg <- cli::format_message(
  c(
    "If you are reading this, the {.pkg greta} installation or error notes \\
    have been wiped. This likely means that installation has not happened, or \\
    it has happened and you've restarted R. See `?install_greta_deps()` for \\
    more information."
    )
)

greta_stash$install_miniconda_notes <- greta_note_msg
greta_stash$install_miniconda_error <- greta_note_msg
greta_stash$conda_create_notes <- greta_note_msg
greta_stash$conda_create_error <- greta_note_msg
greta_stash$conda_install_notes <- greta_note_msg
greta_stash$conda_install_error <- greta_note_msg
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
  # wrap in paste0 to remove list properties
  cat(paste0(greta_stash$tf_num_error))
}
