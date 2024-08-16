#' Set logfile path when installing greta
#'
#' To help debug greta installation, you can save all installation output
#' to a single logfile.
#'
#' @param path valid path to logfile - should end with `.html` so you
#'   can benefit from the html rendering.
#'
#' @return nothing - sets an environment variable for use with
#'   [install_greta_deps()].
#' @export
greta_set_install_logfile <- function(path){
  Sys.setenv("GRETA_INSTALLATION_LOG"=path)
}

#' Write greta dependency installation log file
#'
#' This can only be run after installation has happened with
#'   [install_greta_deps()], and before restarting R.
#'
#' @param path a path with an HTML (.html) extension.
#'
#' @return nothing - writes to file
#' @export
write_greta_install_log <- function(path = greta_logfile) {

  cli::cli_progress_step(
    msg = "Writing logfile to {.path {path}}",
    msg_done = "Logfile written to {.path {path}}"
    )

  cli::cli_progress_step(
    msg = "Open with: {.fun read_greta_logfile}"
  )

  template <- '
  <h1>Greta installation logfile</h1>
  <h2>Created: {{sys_date}}</h2>
  <p>Use this logfile to explore potential issues in installation with greta</p>
  <p>Try searching the text for "error" with Cmd/Ctrl+F</p>

  <h2>Miniconda</h2>

    <details>
      <summary>
        Miniconda Installation Notes
      <pre>
        <code>
          {{{miniconda_notes}}}
        </code>
      </pre>
      </summary>
    </details>

    <details>
      <summary>
        Miniconda Installation Errors
      </summary>
      <pre>
        <code>
          {{{miniconda_error}}}
        </code>
      </pre>
    </details>

  <h2>Conda Environment</h2>

   <details>
      <summary>
      Conda Environment Notes
      </summary>
      <pre>
        <code>
     {{{conda_create_notes}}}
        </code>
      </pre>
    </details>

    <details>
      <summary>
      Conda Environment Errors
      </summary>
      <pre>
        <code>
      {{{conda_create_error}}}
        </code>
      </pre>
    </details>

  <h2>Python Module Installation</h2>

    <details>
      <summary>
        Python Module Installation Notes
      </summary>
      <pre>
        <code>
  {{{conda_install_notes}}}
        </code>
      </pre>
    </details>

      <details>
      <summary>
      Python Module Installation Errors
      </summary>
      <pre>
        <code>
       {{{conda_install_error}}}
        </code>
      </pre>
    </details>
  '

  greta_install_data <- list(
    sys_date = Sys.time(),
    miniconda_notes = greta_stash$miniconda_notes,
    miniconda_error = greta_stash$miniconda_error,
    conda_create_notes = greta_stash$conda_create_notes,
    conda_create_error = greta_stash$conda_create_error,
    conda_install_notes = greta_stash$conda_install_notes,
    conda_install_error = greta_stash$conda_install_error
  )

  writeLines(whisker::whisker.render(template, greta_install_data),
             path)

}

# returns NULL if no envvar
sys_get_env <- function(envvar){
  retrieved_envvar <- Sys.getenv(envvar)
  env_exists <- nzchar(log_env)
  if (env_exists){
    envvar
  } else {
    envvar <- NULL
  }

  envvar
}

#' Read a greta logfile
#'
#' @param path file to read. Optional. If not specified, it will search for
#'   the environment variable "GRETA_INSTALLATION_LOG"
#'
#' @return opens a URL in your default browser
#' @export
read_greta_install_log <- function(path = NULL){
  log_env <- sys_get_env("GRETA_INSTALLATION_LOG")

  path <- path %||% log_env

  browseURL(path)

}
