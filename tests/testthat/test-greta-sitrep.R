# sitrep lines that depend on the machine: the offline-readiness lines depend
# on the uv cache and on which Python packages are required, and greta's
# version line names the install or git commit it came from. Snapshots leave
# the first out and fix the second; both are tested on their own
# (report_offline_readiness() in test-python-backend.R)
local_stable_sitrep <- function(env = parent.frame()) {
  local_mocked_bindings(
    report_offline_readiness = function(...) invisible(),
    greta_version_source = function(...) "0.0.0 from a test",
    .env = env
  )
}

test_that("greta_version_source() says where greta was installed from", {
  not_a_checkout <- withr::local_tempdir()
  git_unused <- function(path) stop("git should not be needed")
  sha <- "1bdc2dab0123456789abcdef"
  cran <- list(
    Version = "0.7.0",
    Repository = "CRAN",
    RemoteType = "standard",
    RemoteSha = "0.7.0"
  )
  github <- list(
    Version = "0.6.0.9000",
    RemoteType = "github",
    RemoteUsername = "greta-dev",
    RemoteRepo = "greta",
    RemoteSha = sha
  )
  universe <- list(
    Version = "0.6.0.9000",
    Repository = "https://greta-dev.r-universe.dev",
    RemoteUrl = "https://github.com/greta-dev/greta",
    RemoteSha = sha
  )
  local_install <- list(Version = "0.6.0.9000")

  cran_source <- greta_version_source(cran, not_a_checkout, git_unused)
  github_source <- greta_version_source(github, not_a_checkout, git_unused)
  universe_source <- greta_version_source(universe, not_a_checkout, git_unused)
  local_source <- greta_version_source(
    local_install,
    not_a_checkout,
    git_unused
  )

  expect_identical(cran_source, "0.7.0 from CRAN")
  expect_identical(
    github_source,
    "0.6.0.9000 from GitHub greta-dev/greta at 1bdc2da"
  )
  expect_identical(
    universe_source,
    "0.6.0.9000 from https://github.com/greta-dev/greta at 1bdc2da"
  )
  expect_identical(
    local_source,
    "0.6.0.9000 from a local install, which records no commit"
  )
})

test_that("greta_version_source() names the repository pak or install.packages() installed greta from", {
  not_a_checkout <- withr::local_tempdir()
  git_unused <- function(path) stop("git should not be needed")
  pak_universe <- list(
    Version = "0.6.0.9000",
    Repository = "https://greta-dev.r-universe.dev",
    RemoteType = "standard",
    RemoteSha = "0.6.0.9000",
    RemoteUrl = "https://github.com/greta-dev/greta"
  )
  pak_p3m <- list(
    Version = "0.6.0",
    Repository = "RSPM",
    RemoteType = "standard",
    RemoteSha = "0.6.0"
  )
  install_packages_p3m <- list(Version = "0.6.0", Repository = "RSPM")

  pak_universe_source <- greta_version_source(
    pak_universe,
    not_a_checkout,
    git_unused
  )
  pak_p3m_source <- greta_version_source(pak_p3m, not_a_checkout, git_unused)
  install_packages_p3m_source <- greta_version_source(
    install_packages_p3m,
    not_a_checkout,
    git_unused
  )

  expect_identical(
    pak_universe_source,
    "0.6.0.9000 from https://greta-dev.r-universe.dev"
  )
  expect_identical(pak_p3m_source, "0.6.0 from RSPM")
  expect_identical(install_packages_p3m_source, "0.6.0 from RSPM")
})

test_that("greta_version_source() names a remote by its type when it has no URL", {
  not_a_checkout <- withr::local_tempdir()
  git_unused <- function(path) stop("git should not be needed")
  gitlab <- list(
    Version = "0.6.0.9000",
    RemoteType = "gitlab",
    RemoteUsername = "greta-dev",
    RemoteRepo = "greta",
    RemoteSha = "1bdc2dab0123456789abcdef"
  )

  gitlab_source <- greta_version_source(gitlab, not_a_checkout, git_unused)

  expect_identical(
    gitlab_source,
    "0.6.0.9000 from gitlab greta-dev/greta at 1bdc2da"
  )
})

test_that("greta_version_source() gives the commit of a git checkout", {
  checkout <- withr::local_tempdir()
  file.create(file.path(checkout, ".git"))
  local_install <- list(Version = "0.6.0.9000")
  clean <- function(path) list(sha = "1bdc2da", dirty = FALSE)
  edited <- function(path) list(sha = "1bdc2da", dirty = TRUE)
  status_unknown <- function(path) list(sha = "1bdc2da", dirty = NA)
  no_git <- function(path) NULL

  clean_source <- greta_version_source(local_install, checkout, clean)
  edited_source <- greta_version_source(local_install, checkout, edited)
  unknown_source <- greta_version_source(
    local_install,
    checkout,
    status_unknown
  )
  no_git_source <- greta_version_source(local_install, checkout, no_git)

  checkout_prefix <- paste("0.6.0.9000 from the git checkout at", checkout)
  expect_identical(clean_source, paste0(checkout_prefix, ", commit 1bdc2da"))
  expect_identical(
    edited_source,
    paste0(checkout_prefix, ", commit 1bdc2da, with uncommitted changes")
  )
  expect_identical(unknown_source, paste0(checkout_prefix, ", commit 1bdc2da"))
  expect_identical(no_git_source, checkout_prefix)
})

# a git repository with one commit of a package's DESCRIPTION, an R file, and
# a file outside the package's sources, removed when the caller exits
local_git_checkout <- function(env = parent.frame()) {
  repo <- withr::local_tempdir(.local_envir = env)
  dir.create(file.path(repo, "R"))
  writeLines("Package: test", file.path(repo, "DESCRIPTION"))
  writeLines("a <- 1", file.path(repo, "R", "a.R"))
  writeLines("notes", file.path(repo, "notes.txt"))
  git_in(repo, "init", "-q")
  git_in(repo, "add", ".")
  git_in(repo, "commit", "-q", "-m", "first")
  repo
}

git_in <- function(repo, ...) {
  system2(
    "git",
    c(
      "-C",
      shQuote(repo),
      "-c",
      "user.name=t",
      "-c",
      "user.email=t@t",
      "-c",
      "commit.gpgsign=false",
      ...
    ),
    stdout = TRUE,
    stderr = TRUE
  )
}

test_that("git_head_commit() reads a checkout's commit and changes to its package sources", {
  skip_if_not(nzchar(Sys.which("git")))
  repo <- local_git_checkout()
  expected_sha <- git_in(repo, "rev-parse", "--short=7", "HEAD")

  clean_head <- git_head_commit(repo)
  writeLines("b <- 2", file.path(repo, "R", "b.R"))
  new_r_file_head <- git_head_commit(repo)
  unlink(file.path(repo, "R", "b.R"))
  writeLines("more notes", file.path(repo, "notes.txt"))
  outside_edit_head <- git_head_commit(repo)

  expect_identical(clean_head, list(sha = expected_sha, dirty = FALSE))
  expect_identical(new_r_file_head, list(sha = expected_sha, dirty = TRUE))
  expect_identical(outside_edit_head, list(sha = expected_sha, dirty = FALSE))
})

test_that("git_head_commit() makes no claim about changes when git status fails", {
  skip_if_not(nzchar(Sys.which("git")))
  repo <- local_git_checkout()
  expected_sha <- git_in(repo, "rev-parse", "--short=7", "HEAD")
  writeLines("not an index", file.path(repo, ".git", "index"))

  corrupt_index_head <- git_head_commit(repo)

  expect_identical(corrupt_index_head, list(sha = expected_sha, dirty = NA))
})

test_that("greta_sitrep() turns greta's offline mode off before starting Python when others are required after load", {
  local_managed_load("numpy")
  apply_greta_python_plan(new_python_plan("managed", "default"))
  reticulate::py_require("pandas")
  local_mocked_bindings(
    have_python = function(...) FALSE,
    have_tf = function(...) FALSE,
    have_tfp = function(...) FALSE,
    quiet_sitrep = function() invisible()
  )
  old_init <- greta_stash$python_has_been_initialised
  withr::defer(greta_stash$python_has_been_initialised <- old_init)
  greta_stash$python_has_been_initialised <- FALSE

  greta_sitrep("quiet")

  expect_identical(Sys.getenv("UV_OFFLINE", unset = NA), NA_character_)
  expect_identical(Sys.getenv("GRETA_UV_OFFLINE", unset = NA), NA_character_)
})

test_that("check_tf_version errors when have_python, _tf, or _tfp is FALSE", {
  local_mocked_bindings(
    py_module_available = function(...) FALSE,
    have_python = function(...) FALSE,
    have_tf = function(...) FALSE,
    have_tfp = function(...) FALSE
  )

  expect_snapshot(
    error = TRUE,
    check_tf_version("error")
  )

  expect_snapshot_warning(
    check_tf_version("warn")
  )

  expect_snapshot(
    check_tf_version("message")
  )
})

test_that("check_tf_version fails when tfp not available", {
  greta_stash$python_has_been_initialised <- FALSE
  local_mocked_bindings(
    have_tfp = function(...) FALSE
  )
  expect_snapshot(
    error = TRUE,
    check_tf_version("error")
  )
})

test_that("greta_sitrep warns when have_python, _tf, or _tfp is FALSE", {
  skip_if_not(check_tf_version())
  skip_on_ci()
  skip_on_cran()
  local_mocked_bindings(
    have_python = function(...) FALSE,
    have_tf = function(...) FALSE,
    have_tfp = function(...) TRUE
  )

  expect_snapshot_warning(
    greta_sitrep()
  )

  local_mocked_bindings(
    have_python = function(...) FALSE,
    have_tf = function(...) FALSE,
    have_tfp = function(...) FALSE
  )

  expect_snapshot_warning(
    greta_sitrep()
  )

  local_mocked_bindings(
    have_python = function(...) TRUE,
    have_tf = function(...) FALSE,
    have_tfp = function(...) TRUE
  )

  expect_snapshot_warning(
    greta_sitrep()
  )

  local_mocked_bindings(
    have_python = function(...) FALSE,
    have_tf = function(...) TRUE,
    have_tfp = function(...) FALSE
  )

  expect_snapshot_warning(
    greta_sitrep()
  )

  local_mocked_bindings(
    have_python = function(...) FALSE,
    have_tf = function(...) FALSE,
    have_tfp = function(...) TRUE
  )

  expect_snapshot_warning(
    greta_sitrep()
  )

  local_mocked_bindings(
    have_python = function(...) TRUE,
    have_tf = function(...) TRUE,
    have_tfp = function(...) FALSE
  )

  expect_snapshot_warning(
    greta_sitrep()
  )

  local_mocked_bindings(
    have_python = function(...) TRUE,
    have_tf = function(...) TRUE,
    have_tfp = function(...) TRUE
  )

  local_stable_sitrep()
  local_python_plan()
  expect_snapshot(greta_sitrep())
})

test_that("greta_sitrep warns when different versions of python, tf, tfp", {
  skip_if_not(check_tf_version())
  skip_on_ci()
  skip_on_cran()

  local_mocked_bindings(
    py_version = function(...) "3.6"
  )

  local_stable_sitrep()
  local_python_plan()
  expect_snapshot(greta_sitrep())

  local_mocked_bindings(
    version_tf = function(...) "2.0.0"
  )

  expect_snapshot(greta_sitrep())

  local_mocked_bindings(
    version_tfp = function(...) "0.9.0"
  )

  expect_snapshot(greta_sitrep())
})

test_that("greta_sitrep reports a missing conda env as unused when managed", {
  skip_if_not(check_tf_version())
  skip_on_ci()
  skip_on_cran()

  local_mocked_bindings(
    have_greta_conda_env = function(...) FALSE
  )

  local_stable_sitrep()
  local_python_plan()
  expect_snapshot(greta_sitrep())
})

test_that("conda env status keeps the availability check off-managed", {
  local_mocked_bindings(
    have_greta_conda_env = function(...) FALSE
  )
  plan_managed <- new_python_plan("managed", "default")
  plan_conda <- new_python_plan("conda", "preference", python = "/envs/py")

  expect_snapshot(report_greta_conda_env_status(plan = plan_managed))
  expect_snapshot(report_greta_conda_env_status(plan = plan_conda))
})


test_that("greta_sitrep works with quiet, minimal, and detailed options", {
  skip_if_not(check_tf_version())
  skip_on_ci()
  skip_on_cran()
  local_stable_sitrep()

  # the resolved Python backend depends on machine state (RETICULATE_PYTHON, a
  # stored greta preference, or a detected conda env), so mask those lines to
  # keep the snapshot stable
  redact_backend <- function(x) {
    x <- gsub("selected via: .*", "selected via: <source>", x)
    x <- gsub('backend: ".*"', 'backend: "<backend>"', x)
    gsub("python: .*", "python: <python>", x)
  }

  expect_snapshot(
    greta_sitrep(verbosity = "quiet"),
    transform = redact_backend
  )
  local_python_plan()
  expect_snapshot(
    greta_sitrep(verbosity = "minimal"),
    transform = redact_backend
  )
  # test it errors when verbosity is not "quiet", "minimal", and "detailed"
  expect_snapshot(
    error = TRUE,
    greta_sitrep(verbosity = "bananas")
  )

  expect_no_error(
    greta_sitrep(verbosity = "detailed")
  )
})
