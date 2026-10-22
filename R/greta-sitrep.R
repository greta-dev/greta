#' Greta Situation Report
#'
#' This checks if Python, Tensorflow, Tensorflow Probability, and the greta
#'   conda environment are available, and also loads and initialises python
#'
#' @param verbosity character. How verbose the output of the situation report.
#'   Possible options: "minimal" (default), "detailed", and "quiet". "Minimal"
#'   provides greta's version and where it came from (such as CRAN, a GitHub
#'   commit, or a git checkout), then just information in python version,
#'   tensorflow version, tensorflow probability, and whether greta conda
#'   environment is available.
#'   "Quiet" presents no information, but prepares greta to be used. "Detailed"
#'   gives information on the version and path for R, greta, python,
#'   tensorflow, tensorflow probability, the greta conda environment, and a
#'   statement on greta usability.
#' @return Message on greta situation report. See "verbosity" parameter details
#'   above for more information.
#' @export
#'
#' @examples
#' \dontrun{
#' greta_sitrep()
#' }
greta_sitrep <- function(verbosity = c("minimal", "detailed", "quiet")) {
  verbosity <- rlang::arg_match(
    arg = verbosity,
    values = c("minimal", "detailed", "quiet"),
    error_arg = "verbosity"
  )

  # check_tf_version() checks greta's offline mode only before a Python start
  # it makes itself, and every verbosity here starts Python before reaching it
  # (the detailed one through py_config()), so greta_sitrep() makes its own
  # check first
  recheck_uv_offline()

  switch(
    verbosity,
    minimal = minimal_sitrep(),
    detailed = detailed_sitrep(),
    quiet = quiet_sitrep()
  )
}


minimal_sitrep <- function() {
  cli::cli_alert_info("{.pkg greta} {greta_version_source()}")
  check_if_python_available()
  check_if_tf_available()
  check_if_tfp_available()
  report_greta_conda_env_status()
  report_python_backend()
  report_offline_readiness()
  check_greta_ready_to_use()
}

# greta's version and where it came from, for greta_sitrep(): the package
# repository it was installed from, such as CRAN or r-universe; a remote, such
# as GitHub, and its commit; the commit of the git checkout devtools::load_all()
# loaded it from; or a local install, which records no commit
greta_version_source <- function(
  description = utils::packageDescription("greta"),
  path = getNamespaceInfo("greta", "path"),
  git_head = git_head_commit
) {
  remote_type <- description$RemoteType %||% ""
  # pak records a repository install's version as its RemoteSha, so only a
  # RemoteSha shaped like a commit is one
  records_commit <- grepl("^[0-9a-f]{7,40}$", description$RemoteSha %||% "")
  repository_remote_type <- remote_type %in% c("standard", "cran")
  repository_without_commit <- !is.null(description$Repository) &&
    !nzchar(remote_type) &&
    !records_commit
  from_repository <- repository_remote_type || repository_without_commit
  from_remote <- nzchar(remote_type) || records_commit
  from_checkout <- file.exists(file.path(path, ".git"))
  repository <- description$Repository %||%
    description$RemoteRepos %||%
    "a package repository"

  # the first kind of install that matches, in this order; which.max() gives
  # the first TRUE
  install_kinds <- c(
    repository = from_repository,
    remote = from_remote,
    checkout = from_checkout,
    local = TRUE
  )
  install_kind <- names(which.max(install_kinds))
  origin <- switch(
    install_kind,
    repository = sprintf("from %s", repository),
    remote = remote_source(description, records_commit),
    checkout = checkout_source(path, git_head(path)),
    local = "from a local install, which records no commit"
  )
  version_source <- sprintf("%s %s", description$Version, origin)
  version_source
}

# where a remote install came from, named as sessioninfo names it: the remote's
# type with its user and repository, or else its URL, with the commit when one
# is recorded
remote_source <- function(description, records_commit) {
  remote_type <- description$RemoteType %||% ""
  remote_name <- if (identical(remote_type, "github")) "GitHub" else remote_type
  # `[[`, since `$` would take a pak install's RemoteRepos for RemoteRepo
  user <- description[["RemoteUsername"]]
  repo <- description[["RemoteRepo"]]
  has_user_repo <- !is.null(user) && !is.null(repo)
  user_repo <- if (has_user_repo) {
    trimws(sprintf("%s %s/%s", remote_name, user, repo))
  }
  locations <- c(
    user_repo,
    description$RemoteUrl,
    description$Repository,
    remote_name,
    "a remote"
  )
  location <- locations[nzchar(locations)][[1]]
  commit <- if (records_commit) {
    sprintf(" at %s", substr(description$RemoteSha, 1, 7))
  } else {
    ""
  }
  remote <- sprintf("from %s%s", location, commit)
  remote
}

checkout_source <- function(path, head) {
  checkout <- sprintf("from the git checkout at %s", path)
  if (is.null(head)) {
    return(checkout)
  }
  changes <- if (isTRUE(head$dirty)) ", with uncommitted changes" else ""
  checkout_commit <- sprintf("%s, commit %s%s", checkout, head$sha, changes)
  checkout_commit
}

# the short commit a git checkout is at, and whether the package's sources have
# uncommitted changes, counting new files that load_all() would source: NA
# when git status fails, and NULL when git isn't available or there is no
# commit
git_head_commit <- function(path) {
  git <- Sys.which("git")
  if (!nzchar(git)) {
    return(NULL)
  }
  run_git <- function(...) {
    output <- tryCatch(
      suppressWarnings(system2(
        git,
        c("-C", shQuote(path), ...),
        stdout = TRUE,
        stderr = FALSE
      )),
      error = \(e) NULL
    )
    git_failed <- is.null(output) || !is.null(attr(output, "status"))
    if (git_failed) NULL else output
  }
  sha <- run_git("rev-parse", "--short=7", "HEAD")
  if (length(sha) != 1) {
    return(NULL)
  }
  package_sources <- c("DESCRIPTION", "NAMESPACE", "R", "src", "inst")
  changes <- run_git(
    "status",
    "--porcelain",
    "--untracked-files=normal",
    "--",
    package_sources
  )
  dirty <- if (is.null(changes)) NA else length(changes) > 0
  list(sha = sha, dirty = dirty)
}

detailed_sitrep <- function() {
  config_info <- reticulate::py_config()

  cli::cli_h1("R")
  cli::cli_ul("version: {.val {getRversion()}}")
  cli::cli_ul("path: {.path {R.home()}}")

  cli::cli_h1("{.pkg greta}")
  cli::cli_ul("version: {greta_version_source()}")
  cli::cli_ul("path: {.path {find.package('greta')}}")

  cli::cli_h1("{.pkg greta} Python backend")
  report_python_backend()
  report_offline_readiness()

  cli::cli_h1("{.pkg python}")
  check_if_python_available()
  cli::cli_ul("path: {.path {reticulate::miniconda_path()}}")

  cli::cli_h1("{.pkg greta conda environment}")
  report_greta_conda_env_status()
  conda_env_path <- greta_conda_env_path()
  cli::cli_ul("path: {.path {conda_env_path}}")

  conda_modules <- greta_list_py_modules()

  tf_in_conda <- nzchar(grep(
    "^(tensorflow)(\\s|$)",
    conda_modules,
    value = TRUE
  ))

  tfp_in_conda <- nzchar(grep(
    "^(tensorflow-probability)(\\s|$)",
    conda_modules,
    value = TRUE
  ))

  cli::cli_h1("{.pkg TensorFlow}")
  check_if_tf_available()
  cli::cli_ul("R path: {.path {find.package('tensorflow')}}")
  cli::cli_ul("Exists in conda env: {.val {tf_in_conda}}")

  cli::cli_h1("{.pkg TensorFlow Probability}")
  check_if_tfp_available()
  cli::cli_ul("Exists in conda env: {.val {tfp_in_conda}}")

  cli::cli_h1("Is {.pkg greta} ready to use?")
  check_greta_ready_to_use()
  cli::cli_inform(
    c(
      "i" = "Use the following code to list available python modules in \\
      {.var greta-env-tf2}:",
      "{.code system(paste('conda list -n', 'greta-env-tf2'), intern = TRUE)}"
    )
  )
}

#' List Python modules installed in greta env
#'
#' @returns matrix/data frame of Python modules that are installed in the greta environment - showing the name, version, build, and install channel.
#'
#' @export
greta_list_py_modules <- function() {
  # there is nothing to list without the conda env; in the uv workflow this is
  # the normal case, so return quietly rather than shelling out to conda (which
  # would exit non-zero and warn)
  if (!have_greta_conda_env()) {
    cli::cli_inform(c(
      "i" = "No {.val greta-env-tf2} conda environment found, so there are no \\
      conda modules to list."
    ))
    return(invisible(NULL))
  }
  conda_modules <- tryCatch(
    expr = {
      # This will find conda whether it's on PATH or installed by reticulate
      conda_bin <- reticulate::conda_binary()

      system2(
        conda_bin,
        args = c("list", "-n", "greta-env-tf2"),
        stdout = TRUE,
        stderr = TRUE
      )
    },
    error = function(e) {
      cli::cli_ul(
        c(
          "Encountered an error in running:",
          "{.code conda list -n greta-env-tf2}",
          "x" = "{.code {e$message}}",
          "!" = "conda may not be installed. Try {.code reticulate::install_miniconda()}"
        )
      )
      return(NULL)
    }
  )

  conda_modules
}

quiet_sitrep <- function() {
  suppressMessages(check_greta_ready_to_use())
}

conda_list_env_modules <- function() {
  system(
    paste("conda list -n", "greta-env-tf2"),
    intern = TRUE,
    ignore.stderr = TRUE
  )
}


check_if_python_available <- function(min_version = "3.3") {
  check_if_software_available(
    software_available = have_python(),
    version = py_version(),
    software_name = "python"
  )
}

check_if_tf_available <- function() {
  check_if_software_available(
    software_available = have_tf(),
    version = version_tf(),
    software_name = "TensorFlow"
  )
}

check_if_tfp_available <- function() {
  check_if_software_available(
    software_available = have_tfp(),
    version = version_tfp(),
    software_name = "TensorFlow Probability"
  )
}

check_if_greta_conda_env_available <- function() {
  check_if_software_available(
    software_available = have_greta_conda_env(),
    software_name = "greta conda environment"
  )
}

# "no conda env" is the healthy, expected state on the managed (uv) backend,
# so report it there as neutral information rather than as a failure; other
# backends (and a conda env that does exist) keep the availability check
report_greta_conda_env_status <- function(
  plan = greta_stash$python_backend %||% greta_python_plan()
) {
  managed <- identical(plan$backend, "managed")
  if (managed && !have_greta_conda_env()) {
    cli::cli_alert_info(
      "greta conda environment: not used (managed (uv) environment active)"
    )
    return(invisible(NULL))
  }
  check_if_greta_conda_env_available()
}

software_availability <- function() {
  software_available <- c(
    python = have_python(),
    tf = have_tf(),
    tfp = have_tfp(),
    greta_env = have_greta_conda_env()
  )
  software_available
}


get_current_ideal_deps <- function() {
  software_version <- data.frame(
    software = c(
      "python",
      "tfp",
      "tf"
    ),
    current = c(
      paste0(py_version()),
      paste0(version_tf()),
      paste0(version_tfp())
    ),
    # versions must be at least this version
    ideal = c(
      greta_deps_default$python_min,
      greta_deps_default$tf_min,
      greta_deps_default$tfp_min
    )
  )

  software_version$match <- c(
    compareVersion(software_version$current[1], software_version$ideal[1]) >= 0,
    compareVersion(software_version$current[2], software_version$ideal[2]) >= 0,
    compareVersion(software_version$current[3], software_version$ideal[3]) >= 0
  )

  software_version
}

check_greta_ready_to_use <- function(software_available) {
  software_available <- software_availability()

  greta_env_not_available <- !software_available["greta_env"]
  other_software_ready <- all(software_available[1:3])
  deps_avail_not_greta_env <- greta_env_not_available && other_software_ready
  if (deps_avail_not_greta_env) {
    check_tf_version("none")
    # on the managed (uv) backend, having no conda env is the expected state
    # (already reported by report_greta_conda_env_status()), so don't mention
    # conda again in the verdict
    plan <- greta_stash$python_backend %||% greta_python_plan()
    if (identical(plan$backend, "managed")) {
      cli::cli_alert_info(
        "All dependencies available; {.pkg greta} is ready to use!",
        wrap = TRUE
      )
    } else {
      cli::cli_alert_info(
        c(
          "i" = "Conda environment not set up, but all dependencies available\n\n",
          "i" = "{.pkg greta} is ready to use!"
        ),
        wrap = TRUE
      )
    }
  }
  if (!all(software_available)) {
    check_tf_version("warn")
  } else if (all(software_available)) {
    software_version <- get_current_ideal_deps()

    if (all(software_version$match)) {
      check_tf_version("none")
      cli::cli_alert_info("{.pkg greta} is ready to use!", wrap = TRUE)
    } else {
      check_tf_version("warn")
    }
  }
}


check_if_software_available <- function(
  software_available,
  version = NULL,
  ideal_version = NULL,
  software_name
) {
  cli::cli_process_start("checking if {.pkg {software_name}} available")
  # if the software is detected

  if (!software_available) {
    cli::cli_process_failed(
      msg_failed = "{.pkg {software_name}} not available"
    )
  }

  if (software_available) {
    if (is.null(ideal_version) & !is.null(version)) {
      cli::cli_process_done(
        msg_done = "{.pkg {software_name}} (v{version}) available"
      )
    }

    # if it has a version and ideal version
    has_ideal_version <- !is.null(version) & !is.null(ideal_version)
    if (has_ideal_version) {
      version_chr <- paste0(version)
      version_match <- compareVersion(version_chr, ideal_version) == 0

      if (version_match) {
        cli::cli_process_done(
          msg_done = "{.pkg {software_name}} (v{version}) available"
        )
      }
      if (!version_match) {
        cli::cli_process_failed(
          msg_failed = "{.pkg {software_name}} available, \\
          however {.strong {ideal_version}} is needed and \\
          {.strong {version}} was detected"
        )
      }
      # if there is no version for the software
    } else if (is.null(version)) {
      cli::cli_process_done(
        msg_done = "{.pkg {software_name}} available"
      )
    }
  }
}

compare_version_vec <- Vectorize(
  FUN = compareVersion,
  vectorize.args = "b",
  SIMPLIFY = TRUE
)

# find out whether the usr has conda installed and visible
#' @importFrom reticulate conda_binary
have_conda <- function() {
  conda_bin <- tryCatch(
    reticulate::conda_binary("auto"),
    error = function(e) NULL
  )
  !is.null(conda_bin)
}

#' @importFrom reticulate py_available
# A check that failed carries why. Returning a bare FALSE discards the only
# thing a user can act on -- uv reporting no resolvable Python, a module that
# will not import, an offline cache miss -- and leaves every cause producing the
# same sentence. `isTRUE()` and `if ()` still work on the result, so callers that
# only want the answer do not change.
check_result <- function(ok, reason = NULL) {
  structure(isTRUE(ok), reason = reason)
}

check_reason <- function(x) {
  attr(x, "reason", exact = TRUE)
}

# cli reads `{}` as interpolation, and these messages come from Python and uv,
# which use braces freely. Doubling them makes cli print them literally.
cli_escape <- function(x) {
  gsub("}", "}}", gsub("{", "{{", x, fixed = TRUE), fixed = TRUE)
}

have_python <- function() {
  tryCatch(
    expr = check_result(reticulate::py_available(initialize = TRUE)),
    error = function(e) check_result(FALSE, conditionMessage(e))
  )
}

#' @importFrom reticulate py_module_available
# Reads the version through the module rather than asking
# py_module_available(), which reports a failure by returning FALSE and so
# leaves nothing to explain it. Touching the module raises the underlying import
# error instead, which is the message worth keeping.
#
# `get_version` is a function rather than the module itself so that reading the
# version happens inside tryCatch(), where the error can be caught.
have_python_module <- function(label, get_version, minimum) {
  tryCatch(
    {
      version <- get_version()
      new_enough <- utils::compareVersion(minimum, version) <= 0
      if (new_enough) {
        check_result(TRUE)
      } else {
        check_result(
          FALSE,
          cli::format_inline(
            "{label} {version} is installed, but greta needs {minimum} or later"
          )
        )
      }
    },
    error = function(e) check_result(FALSE, conditionMessage(e))
  )
}

# This gate decides whether greta runs at all, so it reads the floors from
# greta_deps_default rather than carrying its own copy: a gate looser than the
# advertised range lets an environment load and then fail inside Keras, which
# reads as a greta bug rather than an unsupported install.
have_tfp <- function() {
  have_python_module(
    label = "TensorFlow Probability",
    get_version = \() tfp$`__version__`,
    minimum = greta_deps_default$tfp_min
  )
}

have_tf <- function() {
  have_python_module(
    label = "TensorFlow",
    get_version = \() suppressMessages(tf$`__version__`),
    minimum = greta_deps_default$tf_min
  )
}

version_tf <- function() {
  if (have_tf()) {
    tf$`__version__`
  } else {
    NULL
  }
}

version_tfp <- function() {
  if (have_tfp()) {
    tfp$`__version__`
  } else {
    NULL
  }
}
