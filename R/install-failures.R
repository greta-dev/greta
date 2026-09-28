# Failures seen in pip, conda and uv output during installation, each with what
# to do about it. `pattern` is matched as a fixed string against the output, so
# it has to be text the tool prints verbatim.
known_install_failures <- function() {
  list(
    # pip refuses to install outside a virtualenv when the user has set
    # PIP_REQUIRE_VIRTUALENV, and a conda environment is not a virtualenv
    # (greta-dev/greta#719)
    list(
      pattern = "Could not find an activated virtualenv (required)",
      advice = c(
        "!" = "pip is set to refuse installs outside a virtualenv, with \\
        {.envvar PIP_REQUIRE_VIRTUALENV}. greta installs into a conda \\
        environment, which pip does not count as one.",
        "i" = "For this session, run \\
        {.code Sys.setenv(PIP_REQUIRE_VIRTUALENV = \"false\")} and install \\
        again. To make it permanent, turn it off where it is set: in a shell \\
        startup file such as {.file ~/.bashrc} or {.file ~/.zshrc}, or in \\
        pip's own configuration, which {.code pip config list} shows."
      )
    ),
    # pip finds no wheel for this Python: TensorFlow publishes wheels for a
    # narrow range of Python versions (greta-dev/greta#663)
    list(
      pattern = "Could not find a version that satisfies the requirement",
      advice = c(
        "!" = "pip found no release of a package that works with this \\
        Python. TensorFlow publishes wheels for a narrow range of Python \\
        versions, so the Python version is likely too new or too old for \\
        the TensorFlow version requested.",
        "i" = "Try another {.arg python_version} in {.fun greta_deps_spec}, \\
        then {.run greta::reinstall_greta_deps()}."
      )
    ),
    list(
      pattern = "No solution found when resolving",
      advice = c(
        "!" = "uv could not find versions of Python, TensorFlow and \\
        TensorFlow Probability that work together.",
        "i" = "Check the versions requested with {.run greta::greta_sitrep()}."
      )
    )
  )
}

# Advice for each known failure found in `output`, as cli bullets
install_failure_advice <- function(output) {
  found <- Filter(
    \(failure) any(grepl(failure$pattern, output, fixed = TRUE)),
    known_install_failures()
  )
  unlist(lapply(found, `[[`, "advice"))
}

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
