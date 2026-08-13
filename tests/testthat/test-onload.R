# greta_stash and .internals are attached by .onLoad() rather than created at
# the top level of a file, so nothing here is checked by loading the package
# alone -- if the assignment stopped happening, or built the wrong thing, the
# suite would still pass and the breakage would surface in a dependent package.
#
# .internals in particular is exported and used by greta.gam, greta.gp,
# greta.dynamics and greta.distributions, which reach into its sublists at their
# own build time. A sublist arriving as a function rather than a list breaks
# them while leaving greta itself working, so its shape is asserted here.

test_that("init_greta_stash fills in the starting state", {
  # a fresh one, not the attached stash: its flags are session state that other
  # tests have already flipped by the time this runs
  fresh <- init_greta_stash()

  expect_type(fresh, "environment")
  expect_false(fresh$python_has_been_initialised)
  expect_false(fresh$deps_removed_this_session)
  expect_type(fresh$numerical_messages, "character")
  expect_named(fresh$callbacks, "parallel_progress")
  expect_type(fresh$tf_num_error, "character")
})

test_that("greta_stash is attached to the namespace by .onLoad", {
  # only the shape is asserted, not the flag values: this is the live stash and
  # earlier test files have already flipped them
  expect_type(greta_stash, "environment")
  expect_type(greta_stash$python_has_been_initialised, "logical")
  expect_type(greta_stash$numerical_messages, "character")
})

# Asserting the shape of .internals says nothing about whether it is exported:
# .onLoad() builds it either way, so the sublist tests below pass in a session
# where the NAMESPACE directive has gone missing, while the dependent packages
# fail to build. That happened -- dropping the Collate field left roxygen with
# no top-level object to see, and re-documenting silently removed the export.
test_that(".internals is exported, not merely attached", {
  expect_true(".internals" %in% getNamespaceExports("greta"))
})

test_that(".internals is attached with every sublist a dependent package uses", {
  expect_type(.internals, "list")
  expect_named(
    .internals,
    c(
      "checks",
      "greta_arrays",
      "greta_stash",
      "inference",
      "nodes",
      "tensors",
      "utils"
    )
  )

  # each of these is built by its own deferred *_module() function, and a
  # missing () would leave the function itself here instead of its result.
  # comparing types rather than asserting all(is.list()) so a failure names
  # which sublist arrived wrong
  sublists <- .internals[names(.internals) != "greta_stash"]
  expect_equal(
    vapply(sublists, typeof, character(1)),
    setNames(rep("list", length(sublists)), names(sublists))
  )

  expect_type(.internals$greta_stash, "environment")
})

test_that(".internals sublists reach the functions dependent packages call", {
  # greta.gam and greta.distributions take these at their own load time
  expect_type(.internals$checks$check_tf_version, "closure")
  expect_type(.internals$checks$check_dims, "closure")
  expect_type(.internals$checks$check_in_family, "closure")
  expect_type(.internals$checks$check_positive, "closure")

  # nested one level deeper, and the one that broke: progress_bar arrived as a
  # closure rather than a list when inference_module() referenced it uncalled
  expect_type(.internals$inference$progress_bar, "list")
  expect_type(.internals$inference$progress_bar$create_progress_bar, "closure")
  expect_type(.internals$nodes$constructors$op, "closure")
})
