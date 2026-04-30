# Download all open greta GitHub issues (with comments) and ingest into a
# ragnar DuckDB store at greta-issues.ragnar.duckdb.
#
# Prerequisites:
#   - gh CLI authenticated (`gh auth status`)
#   - ragnar package installed
#   - Ollama running with embeddinggemma:latest
#
# Run from the greta package root:
#   Rscript --quiet --vanilla data-raw/create-greta-issues-rag.R

library(ragnar)
library(jsonlite)
library(purrr)
library(glue)
library(cli)
library(processx)

repo <- "greta-dev/greta"
store_path <- "~/github/njtierney/raglib/greta-issues.ragnar.duckdb"

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
  comment_text <- map(seq_len(nrow(comments)), \(i) {
    format_comment(comments[i, ])
  }) |>
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

# --- Ingest ------------------------------------------------------------------

issue_to_chunks <- function(number, repo) {
  issue <- tryCatch(
    fetch_issue(number, repo),
    error = \(e) {
      cli_alert_danger("Failed #{number}: {conditionMessage(e)}")
      NULL
    }
  )
  if (is.null(issue)) {
    return(NULL)
  }

  chunks <- markdown_chunk(format_issue_markdown(issue))
  chunks$metadata <- toJSON(
    list(issue = number, repo = repo),
    auto_unbox = TRUE
  )
  chunks
}

ingest_issues <- function(store, issue_numbers, repo) {
  chunks_list <- map(
    issue_numbers,
    \(n) issue_to_chunks(n, repo),
    .progress = "Fetching issues"
  ) |>
    compact()
  walk(chunks_list, \(chunks) ragnar_store_insert(store, chunks))
}

# --- Run ---------------------------------------------------------------------

cli_alert_info("Fetching open issues from {repo}")
issue_numbers <- fetch_issue_numbers(repo)
cli_alert_success("Found {length(issue_numbers)} open issues.")

cli_alert_info("Creating store at {store_path}")
store <- ragnar_store_create(
  store_path,
  embed = \(x) ragnar::embed_ollama(x, model = "embeddinggemma:latest"),
  overwrite = TRUE
)

ingest_issues(store, issue_numbers, repo)

cli_alert_info("Building index ...")
ragnar_store_build_index(store)
cli_alert_success("Done. Store written to {store_path}")
