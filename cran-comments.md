## Test environments
* local R installation, R 4.6.1
* win-builder (devel)
* mac-builder (devel)

## R CMD check results

0 errors | 0 warnings | 1 notes

## Submission notes

"greta" was archived in late 2025. This release is a patch update to fix issues
which led to the archival. They also fix some fundamental a tensorflow updates, 
and dependency changes in the DESCRIPTION, such as not relying on `==` for 
versions. We have also made other various improvements, mostly around having
improved installation handling of python. We now also set the number of cores 
to 2 by default.

## revdepcheck results

As the package was archived, there are no revdep issues.
