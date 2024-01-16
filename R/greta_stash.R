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

#' @title Retrieve python installation or error details.
#'
#' @description
#'  These functions retrieve installation or error information output by python
#'   when running `install_miniconda()`, `conda_create()`, `conda_install()`, or
#'   when encountering a TensorFlow numerical problem.
#'
#' @rdname stash-notes
#' @export
#' @examples
#' \dontrun{
#' greta_notes_install_miniconda()
#' greta_notes_conda_create()
#' greta_notes_conda_install()
#' greta_notes_tf_num_error()
#' greta_notes_tf_error()
#' }
greta_notes_install_miniconda_output <- function() {
  cat(greta_stash$miniconda_notes)
}

#' @name stash-notes
#' @export
greta_notes_install_miniconda_error <- function(){
  cat(greta_stash$miniconda_error)
}

#' @name stash-notes
#' @export
greta_notes_conda_create_output <- function() {
  cat(greta_stash$conda_create_notes)
}

#' @name stash-notes
#' @export
greta_notes_conda_create_error <- function() {
  cat(greta_stash$conda_create_error)
}

#' @name stash-notes
#' @export
greta_notes_conda_install_output <- function() {
  cat(greta_stash$conda_install_notes)
}

#' @name stash-notes
#' @export
greta_notes_conda_install_error <- function() {
  cat(greta_stash$conda_install_error)
}

#' @name stash-notes
#' @export
greta_notes_tf_num_error <- function() {
  cat(greta_stash$tf_num_error)
}
