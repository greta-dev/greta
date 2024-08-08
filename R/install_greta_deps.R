#' Install Python dependencies for greta
#'
#' This is a helper function to install Python dependencies needed. By default
#'   these are TF 2.15.0, TFP 0.23.0, and Python 3.10. These Python modules
#'   will be installed into a conda environment named "greta-env-tf2".
#'
#'   To see install notes or errors, there isn't an argument to specify,
#'     instead you will need to specify and environment variable,
#'     `GRETA_INSTALLATION_LOG`, with
#'     `Sys.setenv('GRETA_INSTALLATION_LOG'='path/to/logfile.html')`. Or use
#'     [greta_set_install_logfile()] to set the path, e.g.,
#'     `greta_set_install_logfile('path/to/logfile.html')`. You can also skip
#'     the restarting of R and use [write_greta_install_log()], which
#'     installation notes will indicate how to use if you haven't specified.
#'
#' @param python_deps object created with [greta_python_deps()] where you
#'   specify python, TF, and TFP versions. By default these are TF 2.15.0,
#'   TFP 0.23.0, and Python 3.10. These versions must be compatible
#'   with each other. If they are not, [greta_python_deps()] will error with
#'   more information and suggestions. See ?[greta_python_deps()] for more
#'   information, and see the data object `greta_deps_tf_tfp`
#'   (`?greta_deps_tf_tfp``).
#'
#' @param timeout maximum time in minutes until the installation for each
#'    installation component times out and exits. Default is 5 minutes per
#'    installation component.
#' @param restart character. Restart R after installation? Default is "ask".
#'  Other options are, "force", and "no". Using "force" will will force a
#'  restart after installation. Using  "no" will not restart. Note that this
#'  only restarts R during interactive sessions, and only in RStudio.
#'
#' @param ... Optional arguments, reserved for future expansion.
#'
#' @details
#'  By default, if using RStudio, it will now ask you if you want to restart
#'  the R session. If the session is not interactive, or is not in RStudio,
#'  it will not restart. You can also override this with `restart = TRUE`.
#'
#' @note This will automatically install Miniconda (a minimal version of the
#'  Anaconda scientific software management system), create a 'conda'
#'  environment for greta named 'greta-env-tf2' with required python and python
#'  package versions, and forcibly switch over to using that conda environment.
#'
#'  If you don't want to use conda or the "greta-env-tf2" conda environment, you
#'  can install these specific versions of tensorflow (version 2.6.0), and
#'  tensorflow-probability (version 0.14.1), and ensure that the python
#'  environment that is initialised in this R session has these versions
#'  installed. This is now always straightforward, so we recommend installing
#'  the python packages using `install_greta_deps()` for most users.
#'
#' @name install_greta_deps
#'
#' @export
#'
#' @examples
#' \dontrun{
#' install_greta_deps()
#' }
#' @importFrom reticulate py_available
#' @importFrom tensorflow install_tensorflow
#' @importFrom reticulate conda_create
#' @importFrom reticulate conda_install
#' @importFrom cli cli_alert_info
#' @importFrom cli cli_process_start
#' @importFrom cli cli_process_done
#' @importFrom cli cli_ul
#' @importFrom callr r_process_options
#' @importFrom callr r_process
#' @importFrom cli cli_alert_success
#' @importFrom cli cli_ul
install_greta_deps <- function(python_deps = greta_python_deps(),
                               timeout = 5,
                               restart = c("ask", "force", "no"),
                               ...) {

  check_greta_python_deps(python_deps)

  restart <- rlang::arg_match(
    arg = restart,
    values = c("ask", "force", "no")
  )

  # set warning message length
  options(warning.length = 2000)

  # install miniconda if needed
  if (!have_conda()) {
    greta_install_miniconda(timeout)
  }

  if (!have_greta_conda_env()) {
    greta_create_conda_env(
      timeout = timeout,
      python_deps = python_deps
    )
  }

  # TODO
  # Issue warning if you already have conda env +/ miniconda
  # suggest using `reinstall_greta_deps()`
  greta_install_python_deps(
    timeout = timeout,
    python_deps = python_deps
  )

  # TODO
  # Detect if you have tried to install greta multiple times in the same
  # session, and suggest that perhaps they want to use
  # `reinstall_greta_deps()`
  # perhaps even stopping the session with a "yesno"

  greta_logfile <- Sys.getenv("GRETA_INSTALLATION_LOG")

  logfile_exists <- nzchar(greta_logfile)

  no_logfile <- !logfile_exists


  if (logfile_exists) {
    write_greta_install_log(path = greta_logfile)
  }

  if (no_logfile) {
    cli::cli_alert_warning(
      text = c(
        "No logfile specified. If you want to save the logfile to see any \\
        install notes, or potential errors, you will need to \\
        {.strong not restart R}, then run:\n\n",
      "{.run write_greta_install_log('greta-logfile.html')}"
      ),
      wrap = TRUE
    )
  }

  cli::cli_alert_success("Installation of {.pkg greta} dependencies \\
                         is complete!",
                         wrap = TRUE)

  restart_or_not(restart)

}


restart_or_not <- function(restart){
  # Managing how to restart R
  # requires RStudio and also an interactive session
  has_rstudioapi_pkg <- requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::hasFun("restartSession")

  # Default (if using rstudio) - we ask the user if they want to restart?
  ask_restart <- interactive() && has_rstudioapi_pkg && (restart == "ask")

  # where the user has specified a restart
  user_force_restart <- (restart == "force") &&
    interactive() &&
    has_rstudioapi_pkg

  # Where there is no rstudio/not interactive, suggest restarting.
  suggest_restart <- (restart == "force" | restart == "no") &&
    (!interactive()  | !has_rstudioapi_pkg)

  if (suggest_restart) {
    cli::cli_inform(
      "Restart R, then load {.pkg greta} with: {.code library(greta)}"
    )
    return(invisible())
  }

  if (ask_restart) {
    if (yesno::yesno("Restart R and load greta?")) {
      rstudioapi::restartSession(
        command = "library(greta)",
        clean = TRUE
      )
    }
  }

  if (user_force_restart) {
    cli::cli_inform("Restarting R, then loading {.pkg greta}")
    rstudioapi::restartSession(
      command = "library(greta)",
      clean = TRUE
    )
  }

}

## TODO
## Add a way to pass this along to a custom simpler python installer function
## A la:
# reticulate::py_install(
#   packages = c(
#     'numpy',
#     'tensorflow==2.15',
#     'tensorflow-probability==0.23.0',
#     "keras==2.15.0"
#   ),
#   envname = "greta-env-tf2",
#   pip = TRUE
# )

#' Specify python dependencies for greta
#'
#' A helper function for specifying versions of Tensorflow (TF), Tensorflow Probability (TFP), and Python.
#'
#' @param tf_version Character. Tensorflow (TF) version in format major.minor.patch. Default is "2.15.0".
#' @param tfp_version Character.Tensorflow probability (TFP) version major.minor.patch. Default is "0.23.0".
#' @param python_version Character. Python version in format major.minor.patch. Default is "3.10".
#'
#' @return list of dependencies
#' @export
#'
#' @examples
#' greta_python_deps()
greta_python_deps <- function(tf_version = "2.15.0",
                              tfp_version = "0.23.0",
                              python_version = "3.10"){

  deps_list <- data.frame(
    tf_version = tf_version,
    tfp_version = tfp_version,
    python_version = python_version
  )

  deps_obj <- structure(
    deps_list,
    class = c("greta_python_deps", "data.frame")
  )

  # check for envvar to silence these checks
  check_tfp_tf_semantic(deps_obj)
  check_greta_tf_range(deps_obj)
  check_greta_tfp_range(deps_obj)
  check_greta_python_range(deps_obj$python_version)
  check_greta_deps_config(deps_obj)

  deps_obj

}

check_greta_python_deps <- function(deps,
                                    call = rlang::caller_env()) {
  if (!inherits(deps, "greta_python_deps")) {
    cli::cli_abort(
      message = "{.arg deps} must be created by {.fun greta_python_deps}.",
      call = call
    )
  }
}

#' Print method for greta python deps
#'
#' @param x greta python deps
#' @param ... extra args, not used
#' @export
print.greta_python_deps <- function(x, ...){
  print.data.frame(x)
}

#' Capture greta python dependencies.
#'
#' To assist with capturing and sharing python dependencies, we provide a way
#'   to capture the dependencies currently used.
#'
#' @return `greta_python_deps()` object
#' @export
#'
#' @examples
#' \dontrun{
#' my_deps <- greta_deps_receipt()
#' }
greta_deps_receipt <- function(){

  greta_python_deps(
    tf_version = version_tf(),
    tfp_version = version_tfp(),
    python_version = as.character(reticulate::py_version())
  )

}

check_greta_deps_range <- function(python_deps,
                                   deps,
                                   call = rlang::caller_env()){

  greta_tf_tfp <- greta_deps_tf_tfp[[deps]]
  version_provided <- numeric_version(python_deps[[deps]])

  version_name <- switch(deps,
                         tf_version = "TF",
                         tfp_version = "TFP")

  latest_version <- switch(deps,
                           tf_version = numeric_version("2.15.0"),
                           tfp_version = numeric_version("0.23.0"))

  later_tf_tfp <- version_provided > latest_version

  if (later_tf_tfp){
    gh_issue <- "https://github.com/greta-dev/greta/issues/675"
    cli::cli_abort(
      message = c(
        "{.pkg greta} Does not yet support \\
        {version_name} > {.val {latest_version}}",
        "i" = "See {.url {gh_issue}} for more information",
        "x" = "The provided version was {.val {version_provided}}",
        "i" = "The nearest valid version that is supported by \\
        {.pkg greta} is: {.val {latest_version}}",
        "i" = "Valid versions of TF, TFP, and Python are in \\
                  {.code greta_deps_tf_tfp}",
        "i" = "Inspect with:",
        "{.run View(greta_deps_tf_tfp)}"),
      call = call
    )
  }

  valid <- version_provided %in% greta_tf_tfp
  if (!valid) {
    closest_value <- closest_version(version_provided, greta_deps_tf_tfp[[deps]])
  }

  if (!valid){

    cli::cli_abort(
      message = c("{.val {version_name}} version provided does not match \\
                  supported versions",
                  "The version {.val {version_provided}} was not in \\
                  {.val {greta_deps_tf_tfp[[deps]]}}",
                  "i" = "The nearest valid version that is supported by \\
        {.pkg greta} is: {.val {closest_value}}",
                  "i" = "Valid versions of TF, TFP, and Python are in \\
                  {.code greta_deps_tf_tfp}",
                  "i" = "Inspect with:",
                  "{.run View(greta_deps_tf_tfp)}"),
      call = call
    )
  }
}

check_greta_tf_range <- function(python_deps, call = rlang::caller_env()) {
  check_greta_deps_range(python_deps = python_deps,
                         deps = "tf_version",
                         call = call)
}

check_greta_tfp_range <- function(python_deps, call = rlang::caller_env()) {
  check_greta_deps_range(python_deps = python_deps,
                         deps = "tfp_version",
                         call = call)
}

check_greta_python_range <- function(version_provided,
                                     call = rlang::caller_env()) {

  py_version_min <- unique(greta_deps_tf_tfp$python_version_min)
  py_version_max <- unique(greta_deps_tf_tfp$python_version_max)
  py_versions <- sort(unique(c(py_version_min, py_version_max)))

  min_py <- paste0(min(py_versions))
  max_py <- paste0(max(py_versions))

  outside_range <- outside_version_range(version_provided, py_versions)

  if (outside_range) {

    closest_value <- paste0(closest_version(version_provided, c(py_versions)))

    cli::cli_abort(
      message = c("Python version must be between \\
                {.val {min_py}}-{.val {max_py}}",
                  "x" = "The version provided was {.val {version_provided}}.",
                  "i" = "Try: {.val {closest_value}}"),
      call = call
    )
  }

}

check_greta_deps_config <- function(python_deps,
                                    call = rlang::caller_env()){

  check_greta_python_deps(python_deps)

  python_deps <- python_deps |>
    lapply(numeric_version) |>
    as.data.frame()

  os_matches <- greta_deps_tf_tfp |>
    subset(os_name() == os)

  no_os_matches <- nrow(os_matches) == 0
  if (no_os_matches) {
    valid_os <- unique(greta_deps_tf_tfp$os)
    cli::cli_abort(
      message = c("The os provided does not match one of {.val {valid_os}}",
                  "i" = "Valid versions of TF, TFP, and Python are in \\
                  {.code greta_deps_tf_tfp}",
                  "i" = "Inspect with:",
                  "{.run View(greta_deps_tf_tfp)}"),
      call = call
    )
  }

  config_matches <- os_matches |>
    subset(tfp_version == python_deps$tfp_version) |>
    subset(tf_version == python_deps$tf_version) |>
    subset(python_deps$python_version >= python_version_min) |>
    subset(python_deps$python_version <= python_version_max)

  no_matches <- nrow(config_matches) == 0

  # Build logic to prioritise valid TFP over others
  if (no_matches){

    tfp_matches <- subset(os_matches, tfp_version == python_deps$tfp_version)
    tf_matches <- subset(os_matches, tf_version == python_deps$tf_version)
    py_matches <- os_matches |>
      subset(python_deps$python_version >= python_version_min) |>
      subset(python_deps$python_version <= python_version_max)

    config_matches <- data.frame(
      tfp_match = nrow(tfp_matches) > 0,
      tf_match = nrow(tf_matches) > 0,
      py_match = nrow(py_matches) > 0
    )

    all_valid <- all(config_matches)
    any_valid <- any(config_matches)
    tfp_valid <- config_matches$tfp_match
    tf_valid <- config_matches$tf_match
    py_valid <- config_matches$py_match
    suggest_tfp <- all_valid | tfp_valid | tfp_valid && py_valid
    suggest_tf <- !all_valid && any_valid && tf_valid
    suggest_py <- !tfp_valid && !tf_valid && py_valid

    if (!any_valid){
      cli::cli_abort(
        message = c("Config does not match any installation combinations.",
                    "i" = "Valid versions of TF, TFP, and Python are in \\
                  {.code greta_deps_tf_tfp}",
                    "i" = "Inspect with:",
                    "{.run View(greta_deps_tf_tfp)}"),
        call = call
      )
    }

    possible_suggestions <- c(
      "suggest_tfp" = suggest_tfp,
      "suggest_tf" = suggest_tf,
      "suggest_py" = suggest_py
    )

    which_to_suggest <- names(which(possible_suggestions))

    # Could possibly just suggest that users consult an inbuilt dataset?

    suggested_match <- switch(
      which_to_suggest,
      "suggest_tfp" = tfp_matches,
      "suggest_tf" = tf_matches,
      "suggest_py" = py_matches
    )

    suggested_tfp <- as.character(max(suggested_match$tfp_version))
    suggested_tf <- as.character(max(suggested_match$tf_version))
    suggested_py <- as.character(max(suggested_match$python_version_max))

    cli::cli_abort(
      message = c("Provided {.code greta_python_deps} does not match valid \\
                  installation combinations.",
                  "See below for a suggested config to use:",
                  "{.code greta_python_deps(\\
                  tf_version = {.val {suggested_tf}}, \\
                  tfp_version = {.val {suggested_tfp}}, \\
                  python_version = {.val {suggested_py}}\\
                  )}",
                  "i" = "Valid versions of TF, TFP, and Python are in \\
                  {.code greta_deps_tf_tfp}",
                  "i" = "Inspect with:",
                  "{.run View(greta_deps_tf_tfp)}"
      ),
      call = call
    )

  }

}

check_tfp_tf_semantic <- function(deps_obj,
                                  call = rlang::caller_env()){
  check_semantic(deps_obj$tf_version)
  check_semantic(deps_obj$tfp_version)
}

split_dots <- function(x){
  strsplit(x = x,
           split = ".",
           fixed = TRUE)[[1]]
}

is_semantic <- function(x){
  separated <- split_dots(x)
  is_sem <- length(separated) == 3
  is_sem
}

check_semantic <- function(x,
                           arg = rlang::caller_arg(x),
                           call = rlang::caller_env()){

  not_semantic <- !is_semantic(x)

  if (not_semantic){
    cli::cli_abort(
      message = c("{.arg {arg}} must be semantic.",
                  "We saw {.val {x}}, but we require three separating dots:",
                  "i" = "{.val 1.1.1}",
                  "x" = "{.val 1.1}"),
      call = call
    )
  }
}
