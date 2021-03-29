#' Helper function to turn off Reticulate Autoconfigure
#'
#' Sometimes it is required for installation of `greta` to change the .Renviron
#'   to turn off Reticulate's autoconfigure. This function assists the user
#'   in turning off autoconfigure.
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' turn_off_autoconfig()
#' }
turn_off_autoconfig <- function(){
  env_thing <- 'RETICULATE_AUTOCONFIGURE = "FALSE"'
  clipr::write_clip(env_thing)
  usethis::ui_todo(paste0(env_thing, " written to clipboard"))
  usethis::ui_todo("Add this to the last line of your .Renviron file")
  usethis::edit_r_environ()
}
