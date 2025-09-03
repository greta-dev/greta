library(testthat)
library(greta)
options(testthat.progress.max_fails = 100)

if (Sys.getenv("NOT_CRAN") == "true")
  test_check("greta")
