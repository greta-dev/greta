# Crawl TensorFlow Probability API docs for only the symbols referenced by
# greta source, and ingest into tfp.ragnar.duckdb.
#
# The URL list is derived from greta R/ and inst/ via
# data-raw/derive-greta-tf-allowlist.R, so adding new tfp$... calls in greta
# source automatically expands the store on the next rebuild.
#
# Prerequisites:
#   - ragnar package installed
#   - Ollama running with embeddinggemma:latest (`ollama serve`)
#
# Run from the greta package root:
#   Rscript --quiet --vanilla data-raw/create-tfp-rag.R

library(ragnar)
library(purrr)
library(cli)

source("data-raw/derive-greta-tf-allowlist.R")

store_path <- "~/github/njtierney/raglib/tfp.ragnar.duckdb"

# --- Run ---------------------------------------------------------------------

pages <- derive_tfp_urls()
cli_alert_success("Derived {length(pages)} TFP URLs from greta source.")

cli_alert_info("Creating store at {store_path} ...")
store <- ragnar_store_create(
  store_path,
  embed = \(x) ragnar::embed_ollama(x, model = "embeddinggemma:latest"),
  overwrite = TRUE
)

cli_alert_info("Reading and chunking pages ...")
chunks_list <- map(
  pages,
  \(page)
    tryCatch(
      page |> read_as_markdown() |> markdown_chunk(),
      error = \(e) {
        cli_alert_danger("Failed {page}: {conditionMessage(e)}")
        NULL
      }
    ),
  .progress = "Ingesting TFP pages"
) |>
  compact()

walk(chunks_list, \(chunks) ragnar_store_insert(store, chunks))

cli_alert_info("Building index ...")
ragnar_store_build_index(store)
cli_alert_success("Done. Store written to {store_path}")
