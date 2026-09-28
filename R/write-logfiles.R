#' Set logfile path when installing greta
#'
#' To help debug greta installation, you can save all installation output
#'   to a single logfile.
#'
#' @param path valid path to logfile - should end with `.html` so you
#'   can benefit from the html rendering.
#'
#' @return nothing - sets an environment variable for use with
#'   [install_greta_deps()].
#' @export
greta_set_install_logfile <- function(path) {
  Sys.setenv("GRETA_INSTALLATION_LOG" = path)
}

#' Write greta dependency installation log file
#'
#' Writes what [install_greta_deps()] and the loading of Python reported in
#'   this R session. Steps that have not run in this session are marked as
#'   such, so run it before restarting R. [install_greta_deps()] writes it
#'   itself, whether installation succeeds or fails.
#'
#' @param path a path with an HTML (.html) extension. Defaults to the
#'   `GRETA_INSTALLATION_LOG` environment variable if set, otherwise
#'   "greta-installation-logfile.html" in `tools::R_user_dir("greta")`.
#'
#' @return nothing - writes to file
#' @export
write_greta_install_log <- function(path = greta_install_logfile()) {
  cli::cli_progress_step(
    msg = "Writing logfile to {.path {path}}",
    msg_done = "Logfile written to {.path {path}}"
  )

  cli::cli_progress_step(
    msg = "Open with: {.run open_greta_install_log()}"
  )

  # {{ }} rather than {{{ }}}: install output is full of `<` and `>`, as in
  # "tensorflow<2.16", which unescaped would be read as HTML tags
  template <- '
<h1>greta installation logfile</h1>
<h2>Created: {{sys_date}}</h2>
<p>Use this logfile to explore potential issues in installation with greta.
Search it for "error" with Cmd/Ctrl+F.</p>
{{#has_problems}}
<h2>Problems found</h2>
<ul>
{{#problems}}
<li>{{.}}</li>
{{/problems}}
</ul>
{{/has_problems}}
{{#steps}}
<h2>{{title}}</h2>
{{#has_notes}}
<details>
<summary>Output</summary>
<pre><code>{{notes}}</code></pre>
</details>
{{/has_notes}}
<details{{#open}} open{{/open}}>
<summary>Errors and messages</summary>
<pre><code>{{errors}}</code></pre>
</details>
{{/steps}}
'

  steps <- install_log_steps()
  problems <- install_log_problems(steps)

  greta_install_data <- list(
    sys_date = Sys.time(),
    has_problems = length(problems) > 0,
    problems = problems,
    steps = steps
  )

  writeLines(whisker::whisker.render(template, greta_install_data), path)
}

# The install steps whose output greta stashes, by field prefix, with the
# logfile's heading for each
install_steps <- c(
  miniconda = "Miniconda",
  conda_create = "Conda environment",
  conda_install = "Python modules"
)

# Every greta_stash field the logfile reads. A step's output is left unset until
# it runs, which is how the logfile tells the two apart.
install_stash_fields <- function() {
  c(
    "python_load_diagnosis",
    paste0(rep(names(install_steps), each = 2), c("_notes", "_error"))
  )
}

# One entry per installation step, as the template reads them. The uv diagnosis
# is written by the load path rather than an install: uv's explanation of a
# failed resolution is too long for an error message and belongs here.
install_log_steps <- function() {
  step <- function(title, notes, errors) {
    ran <- !is.null(errors)
    notes <- paste(notes %||% character(), collapse = "\n")
    errors <- paste(
      errors %||% "This step has not run in this R session.",
      collapse = "\n"
    )
    output <- if (ran) paste(notes, errors, sep = "\n") else ""
    error_lines <- install_error_lines(output)
    list(
      title = title,
      notes = notes,
      errors = errors,
      has_notes = nzchar(notes),
      output = output,
      error_lines = error_lines,
      open = length(error_lines) > 0
    )
  }

  conda_steps <- lapply(names(install_steps), \(prefix) {
    step(
      install_steps[[prefix]],
      greta_stash[[paste0(prefix, "_notes")]],
      greta_stash[[paste0(prefix, "_error")]]
    )
  })

  c(
    list(step(
      "Managed (uv) environment",
      notes = NULL,
      errors = greta_stash$python_load_diagnosis
    )),
    conda_steps
  )
}

# What the logfile lists under "Problems found": advice for any known failure,
# then the lines that report an error, each once, and at most 50 of them
install_log_problems <- function(steps) {
  output <- vapply(steps, \(step) step$output, character(1))
  advice_text <- vapply(
    install_failure_advice(output),
    \(bullet) cli::ansi_strip(cli::format_inline(bullet)),
    character(1),
    USE.NAMES = FALSE
  )
  error_lines <- unique(unlist(lapply(steps, \(step) step$error_lines)))
  c(advice_text, utils::head(error_lines, 50))
}

# where the installation logfile goes: GRETA_INSTALLATION_LOG if set, otherwise
# the user directory
greta_install_logfile <- function() {
  logfile <- Sys.getenv("GRETA_INSTALLATION_LOG")
  if (nzchar(logfile)) {
    logfile
  } else {
    greta_default_logfile()
  }
}
#' Read a greta logfile
#'
#' This is a convenience function to facilitate reading logfiles. It opens
#'   a HTML browser using [utils::browseURL()]. It will search for
#'   the environment variable "GRETA_INSTALLATION_LOG" or default to
#'   `tools::R_user_dir("greta")`. To set
#'   "GRETA_INSTALLATION_LOG" you can use
#'   `Sys.setenv('GRETA_INSTALLATION_LOG'='path/to/logfile.html')`. Or use
#'   [greta_set_install_logfile()] to set the path, e.g.,
#'   `greta_set_install_logfile('path/to/logfile.html')`.
#'
#' @return opens a URL in your default HTML browser.
#' @export
open_greta_install_log <- function() {
  utils::browseURL(greta_install_logfile())
}
