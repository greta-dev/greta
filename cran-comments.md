## Test environments
* local R installation, R 4.4.2
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 1 notes

> Found the following (possibly) invalid URLs:
  URL: http://www.phidot.org/software/mark/docs/book/
    From: inst/doc/example_models.html
    Status: 403
    Message: Forbidden
    
We could not find an issue with this link, or an alternative link.

* Days since last update: 244 days

## Submission notes

This release is a substantial overhaul of the internals of greta to migrate the internals from tensorflow 1 to tensorflow 2.

## revdepcheck results

We checked 1 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

