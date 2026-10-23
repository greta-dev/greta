## This package

greta is an R domain-specific language for statistical modelling. It builds models on TensorFlow and TensorFlow Probability, as a fast alternative to Stan, NIMBLE and JAGS. For background, read https://greta-dev.github.io/greta/.

### Adding a file to `R/`

Nothing special is needed. There is no `Collate` field and no `@include` tags: everything that used to run when a file was sourced is now deferred into a function that `.onLoad()` calls, so the order `R/` is sourced in does not matter. Keep it that way — a new file should define functions, not call `module()` or write into `greta_stash` at the top level.

### TensorFlow state in tests

greta keeps a lot of state in the TensorFlow session, and seeding is the worst of it: a `calculate()` call sets TensorFlow's global seed as a side effect, which made an `mcmc()` seed test pass for six years while the behaviour it asserted was broken. Where a test needs the global seed disturbed, draw the value rather than hardcoding it — two tests using the same constant makes the second one a no-op.

### Benchmarks

Measured performance claims in issues and PRs link to the run in `greta.benchmarks` that produced them.

## Maintainer conventions
<!-- njt-agents:start. Written by /use-njt-agents; this whole section is replaced on the next run. Put project notes under "## This package". -->

These are the maintainer's own conventions. Where they disagree with the sections below, these win.

### Overrides

- Use Australian English, not US English.
- Do not use `devtools::test_active_file()` or `devtools::test_coverage_active_file()`. Run a file's tests with `devtools::test(filter = "^{name}")`.
- Run R with `Rscript --quiet --vanilla -e "code"`.
- Group `NEWS.md` bullets into sections of related changes, instead of one alphabetical list.
- When to commit, push or open a pull request comes from the user's own agent instructions. The "Git" section below applies only where those say nothing.

### Coding

- Don't put comments at the end of a line of code. Put the comment on its own line above the code.

### Testing

- Avoid for loops in tests. List each case out, or use a functional approach.
- **Show that a new test fails without the change it tests.** Put the base commit in a worktree, copy the new test file over it, and run it. A test that passes both ways is testing something else, or passing on state.
- **Do not let a test depend on what an earlier test left behind.** Where a test needs global state disturbed (a seed, an option), set it inside the test and draw values rather than hardcoding them.

### GitHub

- If you use `gh` to read an issue, always use `--comments` so you see the whole thread.
- **Write it before you post it.** An issue body or comment goes in a file in `_dev/drafts/` first, with `issue` in the filename, and is rendered if it is a reprex. Check every link resolves before it goes anywhere. Writing the draft and posting it are separate decisions, and posting needs asking each time.
- An issue should say what is expected, what happens instead, what the fix is, and what it improves. Link the lines of code it is about as permalinks pinned to a commit.

### Skills

If these skills are available, read the relevant one before starting the task:

- `testing-r-packages`: writing or reviewing tests.
- `r-function-design`: function internals, input checking, splitting a function that does too much.
- `reprex`: writing a bug report or a minimal reproducible example.
- `code-comments`: writing or reviewing code comments.
- `ai-slop`: reviewing any prose before it ships.
- `njtierney-voice`: prose that goes out under the maintainer's name.
<!-- njt-agents:end -->

## Package development

### Key commands

(All these functions have been optimized for agentic use, so they can be called directly without other arguments.)

```R
# Executing code
devtools::load_all()
code

# Tests
devtools::test() # all tests
devtools::test(filter = "^{name}") # tests for files starting with {name}
devtools::test_active_file("R/{name}.R") # tests for R/{name}.R
devtools::test_active_file("R/{name}.R", desc = 'blah') # single test with exact description "blah" (no regexp)

# Test coverage
devtools::test_coverage() # all files
devtools::test_coverage_active_file("R/{name}.R") # coverage for R/{name}.R from tests in tests/testthat/test-{name}.R

# Documentation
devtools::document() # redocument package
pkgdown::check_pkgdown() # check website

# Run complete R CMD check
devtools::check()
```

### Running R

There are three possible ways to run code, listed in rough order of desirability:

- If you're running inside Posit Assistant or otherwise have an
  `executeCode()` tool available, use it to run code in a session that the
  user can also interact with.

- Otherwise, if an R REPL (e.g. `mcp__r__repl` or `btw::run_r`) is
  available, use that. Note that `mcp__r__repl` uses a sandbox that blocks
  network requests and reads/writes outside of the current directory.

- Otherwise, use `Rscript -e "code"`. On Windows, `Rscript -e` can segfault on
  multiline or complex code; in that case, write it to a temporary `.R` file
  and run `Rscript path/to/file.R`.

### Code style

- Follow the tidyverse style guide
- Always run `air format .` after generating code. (air is bundled with Positron so look there if you can't otherwise find it.)
- Use the base pipe operator (`|>`), not the magrittr pipe (`%>%`).
- Use `\() ...` for single-line anonymous functions. For all other cases, use `function() {...}`.

### Test style

- Tests for `R/{name}.R` go in `tests/testthat/test-{name}.R`.
- All new code should have an accompanying test.
- If there are existing tests, place new tests next to similar existing tests.
- Strive to keep your tests minimal with few comments.
- Never put code in a `test-{name}.R` file outside of a `test_that()` block. Instead, use `tests/testthat/helper.R` or `tests/testthat/helper-{name}.R`.
- Avoid `expect_true()` and `expect_false()` in favor of a specific expectation with a better failure message. A few expectations in newer releases that you might not know about are `expect_all_true()`, `expect_all_equal()`, and `expect_r6_class()`.
- When testing errors and warnings:
  - Only use `expect_error()` or `expect_warning()` if the error or warning has a known class.
  - Generally, prefer `expect_snapshot(error = TRUE)` for errors and `expect_snapshot()` for warnings because these allow the user to review the full text of the output.
- Avoid the `.package` argument to `local_mocked_bindings()`; this modifies the namespace of another package, which is not good practice. Instead create a mockable version of the function in the current package. See `?local_mocked_bindings` for more details.

### Documentation

- Every user-facing function should be exported and have roxygen2 documentation.
- Internal functions should not have roxygen documentation.
- Wrap roxygen2 comments to 80 characters.
- Whenever you add a new (non-internal) documentation topic, also add the topic to `_pkgdown.yml`.
- Always re-document the package after changing a roxygen2 comment.
- Use `pkgdown::check_pkgdown()` to check that all topics are included in the reference index.

### `NEWS.md`

- Every user-facing change should be given a bullet in `NEWS.md`.
- Changes that shouldn't get a bullet:
    - Small documentation changes.
    - Internal refactorings.
    - Fixes to bugs introduced in the current dev version.
- Each bullet should briefly describe the change to the end user and mention the related issue in parentheses.
- A bullet can consist of multiple sentences but should not contain any newlines (i.e. DO NOT line wrap).
- If the change is related to a function, put the name of the function early in the bullet.
- If the change is related to an issue, include the issue number in parentheses.
- Only include a GitHub username if the PR was created by someone who isn't an author.
- Order bullets alphabetically by function name. Put all bullets that don't mention function names at the beginning.

## Specialized skills

- Do you need to deprecate a function or argument? Read `usethis::learn_tidy_skill("deprecate")`.
- Are you adding input checking to an existing function or writing a new exported function? Read `usethis::learn_tidy_skill("arg-checking")`.
- Are you creating a new package? Read `usethis::learn_tidy_skill("package-setup")`.

## Git

- If the user asks you to commit, use markdown in the commit message, and don't line wrap.
- If the commit fixes an issue, include `Fixes #num.` on its own line.
- Only push when the user explicitly requests it.

## Writing

- Use sentence case for headings.
- Use US English.

### Proofreading

If the user asks you to proofread a file, act as an expert proofreader and editor with a deep understanding of clear, engaging, and well-structured writing.

Work paragraph by paragraph, always starting by making a TODO list that includes individual items for each top-level section.

Fix spelling, grammar, and other minor problems without asking the user. Label any unclear, confusing, or ambiguous sentences with a FIXME comment.

Only report what you have changed.
