# Collect one row per matrix cell into a single table on the run page.
#
# GitHub renders a step summary per job, so without this a run leaves eleven
# separate blocks to compare by eye, and "which versions can I use?" cannot be
# read off it at all. This job runs after the matrix with `if: always()`, so a
# run with failures still produces the table -- and the failures are half the
# answer, so they get a row and a reason rather than being left out.
#
# Each cell writes cell-result.tsv (see install-check.R) and uploads it as an
# artifact; those are downloaded into a directory tree below.

results_dir <- Sys.getenv("GRETA_RESULTS_DIR", "cell-results")

files <- list.files(
  results_dir,
  pattern = "cell-result[.]tsv$",
  recursive = TRUE,
  full.names = TRUE
)

if (!length(files)) {
  stop("no cell results found under ", results_dir, call. = FALSE)
}

cols <- c(
  "os",
  "backend",
  "requested",
  "tf",
  "tfp",
  "python",
  "tf_keras",
  "outcome",
  "detail"
)

rows <- do.call(
  rbind,
  lapply(files, function(f) {
    parts <- strsplit(readLines(f, warn = FALSE)[1], "\t", fixed = TRUE)[[1]]
    length(parts) <- length(cols)
    parts[is.na(parts)] <- ""
    setNames(as.data.frame(as.list(parts), stringsAsFactors = FALSE), cols)
  })
)

# requested version order, so the supported range reads as a range. "default"
# sorts last: it is whichever version greta currently pins, so it belongs at the
# newest end.
sort_key <- ifelse(
  rows$requested == "default",
  "99.99",
  rows$requested
)
rows <- rows[order(rows$os, rows$backend, numeric_version(sort_key)), ]

worked <- rows$outcome == "works"

lines <- c(
  "## Which combinations work",
  "",
  paste(
    "Every combination tried, in both directions.",
    "`requested` is what the cell asked for; the rest is what the resolver",
    "actually installed. A combination that does not work is part of the",
    "answer, so it gets a row and a reason."
  ),
  "",
  sprintf(
    "**%d of %d work.**",
    sum(worked),
    nrow(rows)
  ),
  "",
  "| OS | backend | requested | TensorFlow | TFP | Python | tf-keras | result |",
  "|---|---|---|---|---|---|---|---|",
  sprintf(
    "| %s | %s | %s | %s | %s | %s | %s | %s |",
    rows$os,
    rows$backend,
    rows$requested,
    rows$tf,
    rows$tfp,
    rows$python,
    rows$tf_keras,
    ifelse(
      worked,
      "works",
      ifelse(
        nzchar(rows$detail),
        paste0("**", rows$outcome, "** -- ", rows$detail),
        paste0("**", rows$outcome, "**")
      )
    )
  ),
  "",
  sprintf(
    "%d cells reported; any cell missing from this table failed before it could write a row.",
    nrow(rows)
  ),
  ""
)

summary_file <- Sys.getenv("GITHUB_STEP_SUMMARY")
if (nzchar(summary_file)) {
  cat(paste(lines, collapse = "\n"), file = summary_file, append = TRUE)
}

cat(paste(lines, collapse = "\n"), "\n")
