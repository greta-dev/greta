# One row per matrix cell, shared by set-deps.R and install-check.R.
#
# Either can be the last thing a cell does: greta refuses a version outside the
# range it supports, so a cell below the floor stops in set-deps.R and never
# reaches install-check.R. Whichever stops first writes the row.

new_cell <- function() {
  cell <- new.env(parent = emptyenv())
  cell$os <- Sys.getenv("RUNNER_OS", "unknown")
  cell$backend <- Sys.getenv("GRETA_BACKEND", "uv")
  cell$requested <- Sys.getenv("GRETA_TF_VERSION", "default")
  cell$tf <- "-"
  cell$tfp <- "-"
  cell$python <- "-"
  cell$tf_keras <- "-"
  cell$outcome <- "did not load"
  cell$detail <- "did not get as far as loading Python"
  cell
}

write_cell_row <- function(cell) {
  writeLines(
    paste(
      cell$os,
      cell$backend,
      cell$requested,
      cell$tf,
      cell$tfp,
      cell$python,
      cell$tf_keras,
      cell$outcome,
      cell$detail,
      sep = "\t"
    ),
    "cell-result.tsv"
  )
}

# cli decorates errors with box drawing and bullets, which read badly inside a
# markdown table cell
first_line <- function(x) {
  line <- sub("\n.*$", "", x)
  line <- gsub("[|]", "/", line)
  trimws(gsub("[─-╿✖ℹ→×]", "", line))
}

# Tell the workflow not to run the exercise step. Without this a cell whose
# version greta refused would go on to install the *default* versions and
# report a passing row for a combination it was never asked about.
skip_exercise <- function() {
  env_file <- Sys.getenv("GITHUB_ENV")
  if (nzchar(env_file)) {
    cat("GRETA_SKIP_EXERCISE=true\n", file = env_file, append = TRUE)
  }
}
