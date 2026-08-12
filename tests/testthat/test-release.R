test_that("release_bullets() names every revdep a release run can miss", {
  bullets <- release_bullets()

  expect_type(bullets, "character")

  # named rather than all(), so a failure names the package that went missing
  missed <- c("greta.gp", "greta.dynamics", "greta.gam", "greta.distributions")
  named <- vapply(missed, \(p) any(grepl(p, bullets, fixed = TRUE)), logical(1))
  expect_equal(named, setNames(rep(TRUE, length(missed)), missed))
})
