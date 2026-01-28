## Test environments
* local R installation, R 4.5.2
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 1 notes

## Submission notes

"greta" was archived in late 2025. This release is a patch update to fix issues
which led to the archival. They also fix some fundamental a tensorflow updates, 
and dependency changes in the DESCRIPTION, such as not relying on `==` for 
versions. The package also now is careful when building the R package so as to 
not use more than 2 cores.

## revdepcheck results

As the package was archived, there are no revdep issues.
