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
