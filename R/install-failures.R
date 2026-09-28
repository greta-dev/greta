# The lines of `output` that report an error. Each tool starts such a line with
# its marker: pip "ERROR:", R "Error in" or "Error :", conda and Python
# "CondaError:" or "ModuleNotFoundError:", uv "error:".
install_error_lines <- function(output) {
  lines <- unlist(strsplit(output, "\n", fixed = TRUE))
  grep("^\\s*(\\w*Error\\b|ERROR\\b|error:)", lines, value = TRUE)
}

# What a failure message shows of a step's output: the lines that report an
# error, since all of stderr runs to hundreds of lines for a conda or pip
# install; when none do, its last lines, as the logfile has the rest
install_output_excerpt <- function(output_error, output_notes = "") {
  excerpt <- install_error_lines(paste(output_notes, output_error, sep = "\n"))
  if (length(excerpt) == 0) {
    stderr_lines <- unlist(strsplit(output_error, "\n", fixed = TRUE))
    excerpt <- utils::tail(base_remove_empty_string(stderr_lines), 20)
  }
  # install output uses braces, which cli would read as interpolation
  rlang::set_names(cli_escape(excerpt), rep(">", length(excerpt)))
}
