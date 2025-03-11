#' Greta Situation Report
#'
#' This checks if Python, Tensorflow, Tensorflow Probability, and the greta
#'   conda environment are available, and also loads and initialises python
#'
#' @param verbosity character. How verbose the output of the situation report.
#'   Possible options: "minimal" (default), "detailed", and "quiet". "Minimal"
#'   provides just information in python version, tensorflow version,
#'   tensorflow proability, and whether greta conda environment is available.
#'   "Quiet" presents no information, but prepares greta to be used. "Detailed"
#'   gives information on the version and path for R, greta, python,
#'   tensorflow, tensorflow probability, the greta conda environment, and a
#'   statement on greta usability.
#' @return Message on greta situation report. See "verbsoity" parameter details
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

  switch(
    verbosity,
    minimal = minimal_sitrep(),
    detailed = detailed_sitrep(),
    quiet = quiet_sitrep()
  )
}


minimal_sitrep <- function() {
  check_if_python_available()
  check_if_tf_available()
  check_if_tfp_available()
  check_if_greta_conda_env_available()

  check_greta_ready_to_use()
}

detailed_sitrep <- function() {
  config_info <- reticulate::py_config()

  cli::cli_h1("R")
  cli::cli_ul("version: {.val {getRversion()}}")
  cli::cli_ul("path: {.path {R.home()}}")

  cli::cli_h1("{.pkg greta}")
  cli::cli_ul("version: {.val {packageVersion('greta')}}")
  cli::cli_ul("path: {.path {find.package('greta')}}")

  cli::cli_h1("{.pkg python}")
  check_if_python_available()
  cli::cli_ul("path: {.path {reticulate::miniconda_path()}}")

  cli::cli_h1("{.pkg greta conda environment}")
  check_if_greta_conda_env_available()
  conda_env_path <- greta_conda_env_path()
  cli::cli_ul("path: {.path {conda_env_path}}")
  conda_modules <- conda_list_env_modules()

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

quiet_sitrep <- function() {
  suppressMessages(check_greta_ready_to_use())
}

conda_list_env_modules <- function() {
  system(paste("conda list -n", "greta-env-tf2"), intern = TRUE)
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
      "3.8",
      "2.15.0",
      "0.23.0"
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
    cli::cli_alert_info(
      c(
        "i" = "Conda environment not set up, but all dependencies available\n\n",
        "i" = "{.pkg greta} is ready to use!"
      ),
      wrap = TRUE
    )
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
have_python <- function() {
  tryCatch(
    expr = reticulate::py_available(initialize = TRUE),
    error = function(e) FALSE
  )
}

#' @importFrom reticulate py_module_available
have_tfp <- function() {
  is_tfp_available <- py_module_available("tensorflow_probability")

  if (is_tfp_available) {
    pkg <- reticulate::import("pkg_resources")
    tfp_version <- pkg$get_distribution("tensorflow_probability")$version
    is_tfp_available <- utils::compareVersion("0.15.0", tfp_version) <= 0
  }

  return(is_tfp_available)
}

have_tf <- function() {
  is_tf_available <- py_module_available("tensorflow")

  if (is_tf_available) {
    tf_version <- suppressMessages(tf$`__version__`)
    is_tf_available <- utils::compareVersion("2.9.0", tf_version) <= 0
  }

  return(is_tf_available)
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
