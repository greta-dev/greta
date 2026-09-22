# Pre-render greta's expensive vignettes.
#
# greta vignettes load TensorFlow and run MCMC, which is far too heavy to run on
# CI or on every pkgdown build. Instead we keep the *source* of each expensive
# vignette as `<name>.Rmd.orig`, execute it ONCE here (locally, where greta's
# Python stack is available), and commit the resulting evaluated `<name>.Rmd`
# plus its figures. CI/pkgdown then only runs pandoc over already-evaluated
# markdown, so it never needs TensorFlow.
#
# Run this from the repository root whenever you change a `.Rmd.orig` (or the
# home page source):
#   Rscript vignettes/precompute.R
# then commit the regenerated `.Rmd`/`.md` files and their figures.

library(knitr)

# load_all() keeps source references, which rlang appends to the error header
# as `at greta/R/data_values.R:45:3` in any chunk with `error = TRUE`. Readers
# of the rendered vignette run an installed greta and never see that path, so
# turn the suffix off rather than commit a line number that will drift.
options(rlang_call_format_srcrefs = FALSE)

# the development version, not whatever is installed - these vignettes document
# the code in this checkout, and a vignette using a function added since the
# last install silently renders its own error message into the committed .Rmd
pkgload::load_all(".", quiet = TRUE)

# Some vignettes (e.g. example_models) gate their chunks on NOT_CRAN so the
# expensive TensorFlow code does not run on CRAN. Pre-computing IS the off-CRAN
# run, so opt in here.
Sys.setenv(NOT_CRAN = "true")

# Vignettes to pre-render, relative to vignettes/, without extension.
heavy_vignettes <- c(
  "get_started",
  "example_models",
  "mutable_data",
  "webpages/technical_details",
  "analyses/eight_schools",
  "analyses/election_88"
)

precompute_one <- function(rel) {
  base <- basename(rel)
  work_dir <- file.path("vignettes", dirname(rel))
  old <- setwd(work_dir)
  on.exit(setwd(old), add = TRUE)
  knitr::opts_chunk$set(fig.path = paste0(base, "-"))
  knitr::knit(paste0(base, ".Rmd.orig"), paste0(base, ".Rmd"))
  message("re-rendered vignettes/", rel, ".Rmd")
}

for (v in heavy_vignettes) {
  precompute_one(v)
}

# The pkgdown home page: pkgdown/index.Rmd -> pkgdown/index.md, which is
# pkgdown's first-choice home source. The example runs greta directly, so it is
# knit locally and the .md committed (keeping TensorFlow off general CI builds).
knitr::opts_chunk$set(fig.path = "man/figures/")
knitr::knit("pkgdown/index.Rmd", "pkgdown/index.md")
message("re-rendered pkgdown/index.md")
