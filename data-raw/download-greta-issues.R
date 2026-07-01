# Download all open greta GitHub issues (with comments) as markdown files.
# Saves one .md file per issue to issues/greta/.
# Already-downloaded issues are skipped, so this script is safe to re-run.
#
# Prerequisites:
#   - gh CLI authenticated (`gh auth status`)
#
# Run from the greta package root:
#   Rscript --quiet --vanilla data-raw/download-greta-issues.R

library(jsonlite)
library(purrr)
library(glue)
library(cli)
library(processx)

repo <- "greta-dev/greta"
issues_dir <- "~/github/njtierney/raglib/issues/greta"

# --- Fetch -------------------------------------------------------------------

fetch_issue_numbers <- function(repo) {
  raw <- system2(
    "gh",
    args = c(
      "issue",
      "list",
      "--repo",
      repo,
      "--state",
      "open",
      "--limit",
      "1000",
      "--json",
      "number"
    ),
    stdout = TRUE
  )
  fromJSON(paste(raw, collapse = ""))$number
}

fetch_issue <- function(number, repo, timeout = 30) {
  result <- run(
    "gh",
    args = c(
      "issue",
      "view",
      number,
      "--repo",
      repo,
      "--comments",
      "--json",
      "number,title,state,labels,body,comments,url,createdAt,author"
    ),
    stdout = "|",
    timeout = timeout,
    error_on_status = TRUE
  )
  fromJSON(result$stdout)
}

# --- Format ------------------------------------------------------------------

format_labels <- function(labels) {
  if (length(labels) == 0) "none" else paste(labels$name, collapse = ", ")
}

format_comment <- function(comment) {
  glue(
    "### Comment by @{comment$author$login} on {comment$createdAt}\n\n{comment$body}\n"
  )
}

format_comments <- function(comments) {
  if (!is.data.frame(comments) || nrow(comments) == 0) {
    return(NULL)
  }
  comment_text <- map(
    seq_len(nrow(comments)),
    \(i) format_comment(comments[i, ])
  ) |>
    paste(collapse = "\n")
  glue("## Comments\n\n{comment_text}")
}

format_issue_markdown <- function(issue) {
  body <- if (nchar(trimws(issue$body)) > 0) {
    issue$body
  } else {
    "_No description provided._"
  }
  comments <- format_comments(issue$comments)

  glue(
    "# Issue #{issue$number}: {issue$title}",
    "",
    "**URL**: {issue$url}",
    "**State**: {issue$state}",
    "**Author**: @{issue$author$login}",
    "**Created**: {issue$createdAt}",
    "**Labels**: {format_labels(issue$labels)}",
    "",
    "## Description",
    "",
    "{body}",
    "",
    if (!is.null(comments)) "{comments}" else "",
    .sep = "\n"
  )
}

# --- Download ----------------------------------------------------------------

issue_path <- function(number, dir) file.path(dir, glue("{number}.md"))

download_issue <- function(number, repo, dir) {
  path <- issue_path(number, dir)
  if (file.exists(path)) {
    cli_alert_info("Skipping #{number} (already downloaded)")
    return(invisible(path))
  }
  issue <- tryCatch(
    fetch_issue(number, repo),
    error = \(e) {
      cli_alert_danger("Failed #{number}: {conditionMessage(e)}")
      NULL
    }
  )
  if (is.null(issue)) {
    return(invisible(NULL))
  }
  writeLines(format_issue_markdown(issue), path)
  invisible(path)
}

# --- Run ---------------------------------------------------------------------

dir.create(issues_dir, recursive = TRUE, showWarnings = FALSE)

cli_alert_info("Fetching open issues from {repo}")
issue_numbers <- fetch_issue_numbers(repo)
cli_alert_success("Found {length(issue_numbers)} open issues.")

cli_alert_info("Downloading to {issues_dir}/")
walk(
  issue_numbers,
  \(n) download_issue(n, repo, issues_dir),
  .progress = "Downloading issues"
)
cli_alert_success("Done.")
