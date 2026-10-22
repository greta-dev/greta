# AGENTS.md

Guidance for coding agents working in this repository.

## About This Project

This is an R domain specific language for doing statistical modelling. Specifically this interfaces with TensorFlow and TensorFlow Probability to facilitate efficient and fast statistical models in R, as an alternative to STAN, Nimble, and JAGS.

## R package development

### Key commands

```
# To run code
Rscript -e "devtools::load_all(); code"

# To run all tests
Rscript -e "devtools::test()"

# To run all tests for files starting with {name}
Rscript -e "devtools::test(filter = '^{name}')"

# To redocument the package
Rscript -e "devtools::document()"

# To check pkgdown documentation
Rscript -e "pkgdown::check_pkgdown()"

# To check the package with R CMD check
Rscript -e "devtools::check()"

# To format code
air format .
```

General advice:

* Use Australian (British) English spelling
* When running R from the console, always run it with `--quiet --vanilla`
* Always run `air format .` after generating code

### Coding

* Use the base pipe operator (`|>`) not the magrittr pipe (`%>%`)
* Use `\() ...` for single-line anonymous functions. For all other cases, use `function() {...}`
* Don't put comments at the end of a line of code. Put the comment on the line above it. For example, write `# use the uv-provisioned environment` on its own line above `greta_set_python_uv()`, not as a trailing comment on the same line.

### Adding a file to `R/`

Nothing special is needed. There is no `Collate` field and no `@include` tags: everything that used to run when a file was sourced is now deferred into a function that `.onLoad()` calls, so the order `R/` is sourced in does not matter. Keep it that way — a new file should define functions, not call `module()` or write into `greta_stash` at the top level.

### Testing

- Tests for `R/{name}.R` go in `tests/testthat/test-{name}.R`.
- All new code should have an accompanying test.
- If there are existing tests, place new tests next to similar existing tests.
- Strive to keep your tests minimal with few comments.
- Avoid using for loops in tests - prefer listing out each loop manually or using a functional approach.
- Do not use `devtools::test_active_file()`. All testing functions load the code for you.
- **Show that a new test fails without the change it tests.** Put the base
  commit in a worktree, copy the new test file in over it, and run it. A test
  that passes both ways is testing something else, or passing on state.
- **Do not let a test depend on what an earlier test left behind.** greta keeps
  a lot of state in the TensorFlow session, and seeding is the worst of it: a
  `calculate()` call sets TensorFlow's global seed as a side effect, which made
  an `mcmc()` seed test pass for six years while the behaviour it asserted was
  broken. Where a test needs the global seed disturbed, draw the value rather
  than hardcoding it — two tests using the same constant makes the second one a
  no-op.

### Documentation

- Every user-facing function should be exported and have roxygen2 documentation.
- Wrap roxygen comments at 80 characters.
- Internal functions should not have roxygen documentation.
- Always re-document the package after changing a roxygen2 comment.
- For background, read https://greta-dev.github.io/greta/

### `NEWS.md`

- Every user-facing change should be given a bullet in `NEWS.md`. Do not add bullets for small documentation changes or internal refactorings.
- Each bullet should briefly describe the change to the end user and mention the related issue in parentheses.
- A bullet can consist of multiple sentences but should not contain any new lines (i.e. DO NOT line wrap).
- If the change is related to a function, put the name of the function early in the bullet.
- Group bullets in related sections

### GitHub

- If you use `gh` to retrieve information about an issue, always use `--comments` to read all the comments.
- **Write it before you post it.** An issue body or comment goes in a file
  first, in `_dev/drafts/` with `issue` in the filename, and is rendered if it
  is a reprex. Check every link resolves before it goes anywhere; a link into a
  repo that has not been pushed yet will 404. Writing the draft and posting it
  are separate decisions, and posting needs asking each time, not once per
  session.
- An issue should say what is expected, what happens instead, what the fix is,
  and what it provides or improves. Link the lines of code it is about, as permalinks
  pinned to a commit, so they still point at the right lines later. Measured
  claims link to the run in `greta.benchmarks` that produced them.

### Writing

- Use sentence case for headings.
- Use Australian English.

### Proofreading

If the user asks you to proofread a file, act as an expert proofreader and editor with a deep understanding of clear, engaging, and well-structured writing.

Work paragraph by paragraph, always starting by making a TODO list that includes individual items for each top-level heading.

Fix spelling, grammar, and other minor problems without asking the user. Label any unclear, confusing, or ambiguous sentences with a FIXME comment.

Only report what you have changed.

## Core Architecture

### File Organization

- `R/` - All R source code, organized by functionality
