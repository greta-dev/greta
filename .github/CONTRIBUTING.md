# Contributing to greta

We love receiving contributions to greta! To make the prcoess as smooth as
possible, this outlines how to propose a change to greta. It's based on the
contributing guidelines for the tidyverse project.

### Fixing typos

Small typos or grammatical errors in documentation may be edited directly using
the GitHub web interface, so long as the changes are made in the _source_ file.

*  YES: you edit a roxygen comment in a `.R` file below `R/`.
*  NO: you edit an `.Rd` file below `man/`.

### Prerequisites

Before you make a substantial pull request, you should always file an issue and
make sure someone from the team agrees that it’s a problem. If you’ve found a
bug, create an associated issue and illustrate the bug with a minimal 
[reprex](https://www.tidyverse.org/help/#reprex).

### Pull request process

*  We recommend that you create a Git branch for each pull request (PR).  
*  Look at the Github actions build status before and after making changes.
The `README` should contain badges for any continuous integration services used
by the package.  
*  New code should follow the tidyverse [style guide](http://style.tidyverse.org).
You can use the [styler](https://CRAN.R-project.org/package=styler) package to
apply these styles, but please don't restyle code that has nothing to do with 
your PR.  
*  We use [roxygen2](https://cran.r-project.org/package=roxygen2), with
[Markdown syntax](https://cran.r-project.org/web/packages/roxygen2/vignettes/markdown.html), 
for documentation.  
*  We use [testthat](https://cran.r-project.org/package=testthat). Contributions
with test cases included are easier to accept.  
*  For user-facing changes, add a bullet to the top of `NEWS.md` below the
current development version header describing the changes made followed by your
GitHub username, and links to relevant issue(s)/PR(s).

### Testing an unsupported installation

`greta_deps_spec()` refuses TensorFlow and TensorFlow Probability versions greta does not support, so an unsupported installation cannot be built by accident. To build one on purpose, for example to check the messages greta gives when it meets one, set `GRETA_ALLOW_UNSUPPORTED_DEPS`:

```r
Sys.setenv(GRETA_ALLOW_UNSUPPORTED_DEPS = "true")
install_greta_deps(greta_deps_spec(tf_version = "2.17.0"))
```

`greta_deps_spec()` then warns instead of erroring. greta still checks the versions it finds when it loads, so the installation is reported the way a user would see it.

### Code of Conduct

Please note that the greta project is released with a
[Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this
project you agree to abide by its terms.

