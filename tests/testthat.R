library(testthat)
library(greta)
options(testthat.progress.max_fails = 100)

Sys.setenv("OMP_THREAD_LIMIT" = 2)

if (Sys.getenv("NOT_CRAN") == "true") test_check("greta")
