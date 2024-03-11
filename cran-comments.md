## Test environments
* local R installation, R 4.3.3
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 0 notes

* Days since last update: 38

## Submission notes

This release is due to a bug we discovered with trailing commas from the `glue` package (https://github.com/tidyverse/glue/issues/320). It is a very small change, but it is very critical. We have also 

## revdepcheck results

We checked 3 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

