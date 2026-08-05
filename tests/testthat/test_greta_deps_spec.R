test_that("greta_deps_receipt records reality without validating", {
  # a version above greta's supported range must still be captured, since the
  # receipt describes what is actually installed
  spec <- with_mocked_bindings(
    greta_deps_receipt(),
    version_tf = function() "2.15.1",
    version_tfp = function() "0.23.0",
    py_version = function() "3.10"
  )
  expect_s3_class(spec, "greta_deps_spec")
  expect_identical(spec$tf_version, "2.15.1")
})

test_that("greta_deps_receipt errors when deps are not installed", {
  expect_error(
    with_mocked_bindings(
      greta_deps_receipt(),
      version_tf = function() NULL,
      version_tfp = function() NULL
    ),
    regexp = "not both available"
  )
})

test_that("greta_deps_spec accepts supported TensorFlow versions", {
  expect_s3_class(greta_deps_spec(), "greta_deps_spec")
  expect_s3_class(greta_deps_spec(tf_version = "2.14.0"), "greta_deps_spec")
  expect_s3_class(greta_deps_spec(tf_version = "2.9.0"), "greta_deps_spec")
  expect_s3_class(greta_deps_spec(tf_version = "2.15.0"), "greta_deps_spec")
  expect_s3_class(greta_deps_spec(tf_version = "2.15.1"), "greta_deps_spec")
})

test_that("greta_deps_spec accepts TensorFlow 2.16+, which ships Keras 3", {
  expect_s3_class(greta_deps_spec(tf_version = "2.16.1"), "greta_deps_spec")
  expect_s3_class(greta_deps_spec(tf_version = "2.21.0"), "greta_deps_spec")
})

test_that("greta_deps_spec rejects TensorFlow newer than greta supports", {
  expect_error(greta_deps_spec(tf_version = "2.99.0"), "supports TensorFlow")
})

test_that("greta_deps_spec leaves TFP and Python to the resolver", {
  # these previously errored against the compatibility matrix; greta now only
  # bounds TF and lets uv / conda reject incompatible TFP or Python
  expect_s3_class(
    greta_deps_spec(tf_version = "2.15.0", tfp_version = "0.6.0"),
    "greta_deps_spec"
  )
  expect_s3_class(
    greta_deps_spec(python_version = "3.13"),
    "greta_deps_spec"
  )
})

test_that("version pins agree across spec defaults, uv pins, and TF ceiling", {
  # the load-bearing check: greta_deps_spec()'s literal formals must match
  # the canonical versions (drift here is caught at test time, not runtime)
  spec <- greta_deps_spec()
  expect_identical(spec$tf_version, greta_deps_default$tf)
  expect_identical(spec$tfp_version, greta_deps_default$tfp)
  expect_identical(spec$python_version, greta_deps_default$python)

  # uv pins derive minor-series wildcards from the canonical versions
  py_req <- greta_py_require_args()
  tf_minor <- sub("\\.[^.]*$", "", greta_deps_default$tf)
  tfp_minor <- sub("\\.[^.]*$", "", greta_deps_default$tfp)
  # the [tf] extra pulls tf-keras, which TFP imports unconditionally
  expect_identical(
    py_req$packages,
    c(
      paste0("tensorflow==", tf_minor, ".*"),
      paste0("tensorflow_probability[tf]==", tfp_minor, ".*")
    )
  )
  expect_identical(py_req$python_version, greta_deps_default$python_range)

  # the default TF version must itself pass the support ceiling
  expect_no_error(check_greta_tf_supported(greta_deps_spec()))

  # pins must never fall below their own floors
  expect_true(
    compareVersion(greta_deps_default$tf, greta_deps_default$tf_min) >= 0
  )
  expect_true(
    compareVersion(greta_deps_default$tfp, greta_deps_default$tfp_min) >= 0
  )
})

test_that("the python range only contains versions the TF pin has wheels for", {
  # TensorFlow drops old Pythons as it goes -- 2.21 dropped cp39 -- and uv fails
  # with "no solution" if it picks one the pinned TF has no wheel for. That is
  # not a greta error message, and it only surfaces on whichever platform uv
  # happens to choose the unsupported version, so it is worth catching here.
  #
  # This cannot check PyPI. It pins the floor that TF 2.21 requires, so raising
  # the TF pin forces someone to revisit it.
  range_floor <- sub("^>=([0-9.]+),.*$", "\\1", greta_deps_default$python_range)
  range_ceiling <- sub(
    "^.*<=([0-9.]+)$",
    "\\1",
    greta_deps_default$python_range
  )

  expect_gte(numeric_version(range_floor), numeric_version("3.10"))

  # the version greta installs has to be inside the range it asks uv for
  expect_gte(
    numeric_version(greta_deps_default$python),
    numeric_version(range_floor)
  )
  expect_lte(
    numeric_version(greta_deps_default$python),
    numeric_version(range_ceiling)
  )
})
