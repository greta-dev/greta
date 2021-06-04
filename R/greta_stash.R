

install_msg <- "If you are reading this, the installation notes have been wiped."
greta_stash$install_miniconda_notes <- install_msg
greta_stash$conda_create_notes <- install_msg
greta_stash$conda_install_notes <- install_msg

#' @title Retrieve python installation details
#'
#' These functions retrieve installation information output by python
#'   when running `install_miniconda()`, `conda_create()` and `conda_install()`.
#'
#' @rdname installation-stash
#' @export
#' @examples
#' greta_notes_install_miniconda()
#' greta_notes_conda_create()
#' greta_notes_conda_install()
greta_notes_install_miniconda <- function() {
  cat(greta_stash$install_miniconda_notes)
}


#' @name installation-stash
#' @export
greta_notes_conda_create <- function() {
  cat(greta_stash$conda_create_notes)
}

#' @name installation-stash
#' @export
greta_notes_conda_install <- function() {
  cat(greta_stash$conda_install_notes)
}
