#' Conflicts between the greta and other packages
#'
#' This function lists all the conflicts between packages in the greta
#' and other packages that you have loaded.
#'
#' There are XX conflicts that are deliberately ignored: \code{XX},
#' \code{union}, \code{setequal}, and \code{setdiff} from greta. reasons
#'
#' @export
#' @param only Set this to a character vector to restrict to conflicts only
#'   with these packages.
#' @examples
#' greta_conflicts()
greta_conflicts <- function(only = NULL) {
  envs <- grep("^package:", search(), value = TRUE)
  envs <- rlang::set_names(envs)

  if (!is.null(only)) {
    only <- union(only, core)
    envs <- envs[names(envs) %in% paste0("package:", only)]
  }

  objs <- invert(lapply(envs, ls_env))

  conflicts <- purrr::keep(objs, ~ length(.x) > 1)

  tidy_names <- paste0("package:", greta_packages())
  conflicts <- purrr::keep(conflicts, ~ any(.x %in% tidy_names))

  conflict_funs <- purrr::imap(conflicts, confirm_conflict)
  conflict_funs <- purrr::compact(conflict_funs)

  structure(conflict_funs, class = "greta_conflicts")
}

greta_conflict_message <- function(x) {
  header <- cli::rule(
    left = cli::style_bold("Conflicts"),
    right = "greta_conflicts()"
  )

  pkgs <- x |> purrr::map(~ gsub("^package:", "", .))
  others <- pkgs |> purrr::map(`[`, -1)
  other_calls <- purrr::map2_chr(
    others, names(others),
    ~ paste0(cli::col_blue(.x), "::", .y, "()", collapse = ", ")
  )

  winner <- pkgs |> purrr::map_chr(1)
  funs <- format(paste0(cli::col_blue(winner), "::", cli::col_green(paste0(names(x), "()"))))
  bullets <- paste0(
    cli::col_red(cli::symbol$cross), " ", funs, " masks ", other_calls,
    collapse = "\n"
  )

  conflicted <- paste0(
    cli::col_cyan(cli::symbol$info), " ",
    cli::format_inline("Use the {.href [conflicted package](http://conflicted.r-lib.org/)} to force all conflicts to become errors"
    ))

  paste0(
    header, "\n",
    bullets, "\n",
    conflicted
  )
}

#' @export
print.greta_conflicts <- function(x, ..., startup = FALSE) {
  cli::cat_line(greta_conflict_message(x))
  invisible(x)
}

confirm_conflict <- function(packages, name) {
  # Only look at functions
  objs <- packages |>
    purrr::map(~ get(name, pos = packages)) |>
    purrr::keep(is.function)

  if (length(objs) <= 1)
    return()

  # Remove identical functions
  objs <- objs[!duplicated(objs)]
  packages <- packages[!duplicated(packages)]
  if (length(objs) == 1){
    return()
  }

  packages
}

ls_env <- function(env) {
  x <- ls(pos = env)
  x
}

core <- c("ggplot2", "tibble", "tidyr", "readr", "purrr", "dplyr", "stringr", "forcats", "lubridate")

core_unloaded <- function() {
  search <- paste0("package:", core)
  core[!search %in% search()]
}

# Attach the package from the same package library it was
# loaded from before. https://github.com/tidyverse/tidyverse/issues/171
same_library <- function(pkg) {
  loc <- if (pkg %in% loadedNamespaces()) dirname(getNamespaceInfo(pkg, "path"))
  library(pkg, lib.loc = loc, character.only = TRUE, warn.conflicts = FALSE)
}

package_version_h <- function(pkg) {
  highlight_version(utils::packageVersion(pkg))
}


invert <- function(x) {
  if (length(x) == 0) return()
  stacked <- utils::stack(x)
  tapply(as.character(stacked$ind), stacked$values, list)
}


highlight_version <- function(x) {
  x <- as.character(x)

  is_dev <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    !is.na(x) & x >= 9000
  }

  pieces <- strsplit(x, ".", fixed = TRUE)
  pieces <- lapply(pieces, function(x) ifelse(is_dev(x), cli::col_red(x), x))
  vapply(pieces, paste, collapse = ".", FUN.VALUE = character(1))
}


greta_attach <- function() {
  to_load <- core_unloaded()

  suppressPackageStartupMessages(
    lapply(to_load, same_library)
  )

  invisible(to_load)
}

greta_attach_message <- function(to_load) {
  if (length(to_load) == 0) {
    return(NULL)
  }

  header <- cli::rule(
    left = cli::style_bold("Attaching core greta packages"),
    right = paste0("greta ", package_version_h("greta"))
  )

  to_load <- sort(to_load)
  versions <- vapply(to_load, package_version_h, character(1))

  packages <- paste0(
    cli::col_green(cli::symbol$tick), " ", cli::col_blue(format(to_load)), " ",
    cli::ansi_align(versions, max(cli::ansi_nchar(versions)))
  )

  if (length(packages) %% 2 == 1) {
    packages <- append(packages, "")
  }
  col1 <- seq_len(length(packages) / 2)
  info <- paste0(packages[col1], "     ", packages[-col1])

  paste0(header, "\n", paste(info, collapse = "\n"))
}

# is_loading_for_tests <- function() {
#   !interactive() && identical(Sys.getenv("DEVTOOLS_LOAD"), "greta")
# }

is_attached <- function(x) {
  paste0("package:", x) %in% search()
}

inform_startup <- function(msg, ...) {
  if (is.null(msg)) {
    return()
  }
  if (isTRUE(getOption("greta.quiet"))) {
    return()
  }

  rlang::inform(msg, ..., class = "packageStartupMessage")
}

.onAttach <- function(...) {
  # attached <- greta_attach()
  # if (!is_loading_for_tests()) {
    # inform_startup(greta_attach_message(attached))
  # }

  conflicts <- greta_conflicts()
  inform_startup(greta_conflict_message(conflicts))

  # if (!is_attached("conflicted") && !is_loading_for_tests()) {
  #   conflicts <- greta_conflicts()
  #   inform_startup(greta_conflict_message(conflicts))
  # }
}

greta_packages <- function(include_self = TRUE) {
  raw <- utils::packageDescription("greta")$Imports
  imports <- strsplit(raw, ",")[[1]]
  parsed <- gsub("^\\s+|\\s+$", "", imports)
  names <- vapply(strsplit(parsed, "\\s+"), "[[", 1, FUN.VALUE = character(1))

  if (include_self) {
    names <- c(names, "greta")
  }

  names
}


#' Resolve conflicts between greta package and others
#'
#' `greta_prefer()` uses the `conflicted` package to handle common
#' conflicts with tidymodels and other packages.
#'
#' The list of conflicts includes several dozen that are known issues with other
#' packages.
#'
#' Note that the \pkg{conflicted} package is used to manage which packages take
#' precedence. Using `greta_prefer()` will turn on general conflict
#' resolution during the R session.
#'
#' @param quiet If `TRUE`, all output will be suppressed
#' @seealso [greta_conflicts()]
#' @export
#' @examples
#' greta_prefer(quiet = FALSE)
greta_prefer <- function(quiet = TRUE) {
  res <-
    utils::capture.output({
      conflicted::conflict_prefer("%*%",          winner = "greta", quiet = quiet)
      conflicted::conflict_prefer("apply",        winner = "greta", quiet = quiet)
      conflicted::conflict_prefer("backsolve",    winner = "greta", quiet = quiet)
      conflicted::conflict_prefer("beta",         winner = "greta", quiet = quiet)
      conflicted::conflict_prefer("chol2inv",     winner = "greta", quiet = quiet)
      conflicted::conflict_prefer("colMeans",     winner = "greta", quiet = quiet)
      conflicted::conflict_prefer("colSums",      winner = "greta", quiet = quiet)
      conflicted::conflict_prefer("diag",         winner = "greta", quiet = quiet)
      conflicted::conflict_prefer("eigen",        winner = "greta", quiet = quiet)
      conflicted::conflict_prefer("forwardsolve", winner = "greta", quiet = quiet)
      conflicted::conflict_prefer("gamma",        winner = "greta", quiet = quiet)
      conflicted::conflict_prefer("identity",     winner = "greta", quiet = quiet)
      conflicted::conflict_prefer("rowMeans",     winner = "greta", quiet = quiet)
      conflicted::conflict_prefer("rowSums",      winner = "greta", quiet = quiet)
      conflicted::conflict_prefer("sweep",        winner = "greta", quiet = quiet)
      conflicted::conflict_prefer("tapply",       winner = "greta", quiet = quiet)

    },
    type = "message")
  if (!quiet) {
    header <- cli::rule(
      left = cli::style_bold("Conflicts"),
      right = "greta_prefer()"
    )
    res <- gsub("^\\[conflicted\\] ", "", res)
    res <- paste0(res, collapse = "\n")
    msg(header, startup = TRUE)
    msg(res, startup = TRUE)
  }
  invisible(NULL)
}
