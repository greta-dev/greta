# Ingest previously downloaded greta issue markdown files into a ragnar store.
# Reads all .md files from issues/greta/ and writes greta-issues.ragnar.duckdb.
#
# Prerequisites:
#   - ragnar package installed
#   - Ollama running with embeddinggemma:latest (`ollama serve`)
#   - Issues already downloaded via R/download-greta-issues.R
#
# Run from the greta package root:
#   Rscript --quiet --vanilla data-raw/ingest-greta-issues.R

library(ragnar)
library(purrr)
library(cli)

issues_dir <- "~/github/njtierney/raglib/issues/greta"
store_path <- "~/github/njtierney/raglib/greta-issues.ragnar.duckdb"

# --- Ingest ------------------------------------------------------------------

md_to_chunks <- function(path) {
  tryCatch(
    markdown_chunk(readLines(path, warn = FALSE) |> paste(collapse = "\n")),
    error = \(e) {
      cli_alert_danger("Failed to chunk {path}: {conditionMessage(e)}")
      NULL
    }
  )
}

# --- Run ---------------------------------------------------------------------

md_files <- list.files(issues_dir, pattern = "\\.md$", full.names = TRUE)
cli_alert_info("Found {length(md_files)} issue files in {issues_dir}/")

cli_alert_info("Creating store at {store_path} ...")
store <- ragnar_store_create(
  store_path,
  embed = \(x) ragnar::embed_ollama(x, model = "embeddinggemma:latest"),
  overwrite = TRUE
)

cli_alert_info("Chunking and inserting ...")
chunks_list <- map(md_files, md_to_chunks, .progress = "Chunking issues") |>
  compact()
walk(chunks_list, \(chunks) ragnar_store_insert(store, chunks))

cli_alert_info("Building index ...")
ragnar_store_build_index(store)
cli_alert_success("Done. Store written to {store_path}")
