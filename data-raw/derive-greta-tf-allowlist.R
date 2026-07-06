# Derive the exact list of TensorFlow and TensorFlow Probability API doc URLs
# referenced by greta source. Used by create-tf-rag.R and create-tfp-rag.R
# to build tightly-scoped RAG stores.
#
# Usage:
#   source("data-raw/derive-greta-tf-allowlist.R")
#   tf_urls  <- derive_tf_urls()
#   tfp_urls <- derive_tfp_urls()

library(purrr)

scan_dirs <- c("R", "inst")

# Base URLs for the API docs
tf_base <- "https://www.tensorflow.org/api_docs/python/tf"
tfp_base <- "https://www.tensorflow.org/probability/api_docs/python/tfp"

# Symbols that are not real API endpoints and should be dropped.
# - dtype shortcuts (tf$float, tf$int, tf$bool) - covered by tf/dtypes
# - partial chains ending in a version selector (tf$compat$v)
tf_drop <- c("float", "int", "bool", "compat$v")

# TFP package-level metadata, not API endpoints
tfp_drop <- c("python_version_min", "python_version_max", "os")

extract_symbols <- function(prefix, dirs) {
  pattern <- sprintf(
    "%s\\$[a-zA-Z_][a-zA-Z0-9_]*(\\$[a-zA-Z_][a-zA-Z0-9_]*)?",
    prefix
  )
  files <- list.files(dirs, recursive = TRUE, full.names = TRUE)
  text <- unlist(lapply(files, readLines, warn = FALSE))
  matches <- regmatches(text, gregexpr(pattern, text))
  syms <- unique(unlist(matches))
  syms[!grepl("_$", syms)]
}

symbol_to_url <- function(symbol, base) {
  parts <- strsplit(symbol, "\\$")[[1]][-1]
  paste0(base, "/", paste(parts, collapse = "/"))
}

derive_tf_urls <- function() {
  syms <- extract_symbols("tf", scan_dirs)
  tail_parts <- sub("^tf\\$", "", syms)
  syms <- syms[!tail_parts %in% tf_drop]
  unique(map_chr(syms, symbol_to_url, base = tf_base))
}

derive_tfp_urls <- function() {
  syms <- extract_symbols("tfp", scan_dirs)
  tail_parts <- sub("^tfp\\$", "", syms)
  syms <- syms[!tail_parts %in% tfp_drop]
  unique(map_chr(syms, symbol_to_url, base = tfp_base))
}
