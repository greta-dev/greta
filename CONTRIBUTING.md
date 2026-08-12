# Contributing to greta

Thanks for your interest in greta. This file covers working on the package
itself. For contributing an example model or an analysis case study, see the
[contribute page](https://greta-dev.github.io/greta/articles/webpages/contribute.html).

If you have a question about how to use greta, or how to fit a particular
model, please ask on the [greta forum](https://forum.greta-stats.org) rather
than opening an issue.

## Before you start

For anything beyond a small fix, open an issue first so we can discuss the
approach. The [issues tracker](https://github.com/greta-dev/greta/issues) also
lists work that is already planned.

## Setting up

greta needs Python, TensorFlow and TensorFlow Probability. You do not have to
install those yourself: the first time you use greta in a session it installs
them via `uv`. Run `greta_sitrep()` if you want to see what it picked up.

```r
# from a clone of the repo
devtools::load_all()
devtools::test()
```

## Conventions

- Format with [air](https://posit-dev.github.io/air/): `air format .`
- Use the base pipe `|>`, not `%>%`
- Use `\() ...` only for single-line anonymous functions; otherwise
  `function() {...}`
- greta supports R 4.1, so no `_$x` placeholder syntax
- Put comments on their own line, above the code they describe, not trailing
- Tests for `R/thing.R` go in `tests/testthat/test-thing.R`
- Every user-facing change gets a bullet in `NEWS.md`, on one line, mentioning
  the issue number in parentheses
- Re-run `devtools::document()` after changing any roxygen comment

## Submitting

Open a pull request against `main`, and mention the issue it addresses. CI runs
`R CMD check` on Linux, macOS and Windows; installation across a range of
TensorFlow versions is checked weekly by the
[install-check workflow](https://github.com/greta-dev/greta/actions/workflows/install-check.yaml).
