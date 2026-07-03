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

test_that("greta_deps_spec rejects TensorFlow newer than greta supports", {
  # TF 2.16+ ships Keras 3, which greta does not support (#675)
  expect_error(greta_deps_spec(tf_version = "2.16.1"), "supports TensorFlow")
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
  expect_identical(
    py_req$packages,
    c(
      paste0("tensorflow==", tf_minor, ".*"),
      paste0("tensorflow_probability==", tfp_minor, ".*")
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
