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

## Exposing internals with `module()`

### Why this exists

greta is designed to be extended. greta.distributions, greta.gp,
greta.dynamics and greta.gam each add distributions or model components, and
to do that they need greta's internal machinery: `check_dims()` to validate
arguments the way greta does, `distribution_node` to subclass, `op()` to build
a node in the graph.

Those functions cannot simply be exported. Exporting them would put dozens of
internal names into greta's user-facing API, each needing a help page, an
entry in the reference index, and an implicit promise not to change. They are
not for users.

So greta exports exactly one object for this, `.internals`, holding them in a
nested named tree. `module()` is the small helper that builds that tree. It
guesses names from the call, so this:

```r
module(check_dims, check_tf_version)
```

produces `list(check_dims = check_dims, check_tf_version = check_tf_version)`,
sorted by name, without writing every name twice.

### What it looks like from the other side

An extension package pulls out what it needs when it loads. From
[greta.distributions/R/internals.R](https://github.com/greta-dev/greta.distributions/blob/main/R/internals.R):

```r
check_dims <- .internals$checks$check_dims
create_progress_bar <- .internals$inference$progress_bar$create_progress_bar
distribution_node <- .internals$nodes$node_classes$distribution_node
op <- .internals$nodes$constructors$op
```

Every path in those lines is a `module()` call on greta's side: `checks` is
`checks_module()`, `nodes` is `nodes_module()`, and `progress_bar` is
`progress_bar_module()` nested inside `inference_module()`. Adding something
to a module is what makes it reachable from a line like these.

### What those pieces are for

An extension reaches into `.internals` because it is doing the same kind of
work greta does internally, and needs the same tools:

| to build | it needs |
|---|---|
| a distribution | `distribution_node` to subclass, `distrib()` to turn that class into a user-facing greta array, `check_dims()` to validate and broadcast argument dimensions, `fl()` and `tf_as_float()` to write the TensorFlow density at greta's float precision |
| an operation on greta arrays | `op()`, which builds the operation node and hands it back as a greta array, plus `as.greta_array()` and `get_node()` to move between the two representations |
| something that runs inference | `dag_class`, and `create_progress_bar()` to report progress the way greta does |

### When you would add one

Almost never, and it is worth being clear about who does what.

If you are writing an **extension package**, you never call `module()`. You
read `.internals`, as above.

If you are working on **greta itself**, adding a distribution or an operation
does not call for it either. That code *uses* the functions above; it does not
need to expose them.

The trigger is narrower than either: an extension needs something greta
already has, and it is not in `.internals` yet. In practice that arrives as a
request from the extension side, and the fix is to add the function to the
relevant module.

`module()` evaluates its arguments, so it must not run while a file is being
sourced. Wrap it in a function taking no arguments, and let `.onLoad()` call
it:

```r
checks_module <- function() {
  module(
    check_tf_version,
    check_dims
  )
}
```

Then add it to `init_internals()` in `R/internals.R`, **calling it with `()`**:

```r
init_internals <- function(stash) {
  module(
    checks = checks_module(),
    ...
  )
}
```

The same applies to a module nested inside another, such as
`progress_bar = progress_bar_module()` inside `inference_module()`.

Those parentheses are easy to lose and expensive to lose. Writing
`checks = checks_module` puts the function itself into `.internals` rather
than its result. greta still loads, its test suite still passes, and
`R CMD check` still passes, because nothing in greta reads `.internals`. The
failure surfaces in the extension package, when it builds:

```
Error in .internals$inference$progress_bar$create_progress_bar :
  object of type 'closure' is not subsettable
```

That is exactly what happened to `progress_bar_module()`, and it was found by
building greta.distributions rather than by greta's own test suite. It is
why `tests/testthat/test-onload.R` asserts the type of every `.internals`
sublist.

## Submitting

Open a pull request against `main`, and mention the issue it addresses. CI runs
`R CMD check` on Linux, macOS and Windows; installation across a range of
TensorFlow versions is checked weekly by the
[install-check workflow](https://github.com/greta-dev/greta/actions/workflows/install-check.yaml).
