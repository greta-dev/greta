# usethis::use_release_issue() looks this up in the namespace and turns each
# element into a checklist item on the release issue.
release_bullets <- function() {
  c(
    paste0(
      "Run `revdepcheck::revdep_add(packages = c(\"greta.gp\", ",
      "\"greta.dynamics\"))` and then `revdep_check()` again. ",
      "`revdep_check()` on its own silently skips both: it finds reverse ",
      "dependencies with a regex that needs a separator on either side of ",
      "the package name, and each of them names greta first in `Depends` ",
      "([r-lib/revdepcheck#403]",
      "(https://github.com/r-lib/revdepcheck/issues/403)). ",
      "A clean `revdep/problems.md` is not evidence that they were checked."
    ),
    paste0(
      "Check greta.gam and greta.distributions by hand. Neither is on CRAN, ",
      "so no revdep run will ever cover them, and both read `.internals` ",
      "while they build, which is the first place a breaking change to it ",
      "shows up. Install this version into a library of its own, then ",
      "`R CMD check` each of them with that library first on `R_LIBS`."
    )
  )
}
