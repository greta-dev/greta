# Collect one row per matrix cell into a single table on the run page.
#
# GitHub renders a step summary per job, so without this a run leaves eleven
# separate blocks to compare by eye, and "which versions work" cannot be read
# off it. This job runs after the matrix with `if: always()`, so a run that had
# failures still produces the table -- a red cell is a result, and the table is
# where the answer lives.
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
  "status"
)

rows <- do.call(
  rbind,
  lapply(files, function(f) {
    parts <- strsplit(readLines(f, warn = FALSE)[1], "\t", fixed = TRUE)[[1]]
    setNames(as.data.frame(as.list(parts), stringsAsFactors = FALSE), cols)
  })
)

# newest TensorFlow first, so the supported range reads top-down
rows <- rows[order(rows$os, rows$backend, numeric_version(rows$tf)), ]

tick <- function(status) {
  ifelse(status == "works", "pass", paste0("**", status, "**"))
}

lines <- c(
  "## Which combinations work",
  "",
  paste(
    "Each row is one matrix cell. `requested` is what the cell asked for;",
    "the rest is what the resolver actually installed."
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
    tick(rows$status)
  ),
  "",
  sprintf(
    "%d of %d cells reported; any cell missing from this table failed before it could resolve a stack.",
    nrow(rows),
    as.integer(Sys.getenv("GRETA_EXPECTED_CELLS", nrow(rows)))
  ),
  ""
)

summary_file <- Sys.getenv("GITHUB_STEP_SUMMARY")
if (nzchar(summary_file)) {
  cat(paste(lines, collapse = "\n"), file = summary_file, append = TRUE)
}

cat(paste(lines, collapse = "\n"), "\n")
