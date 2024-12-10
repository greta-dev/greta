# these tests need to be run last because otherwise mockery infects the
# rest of the tests and Python is never detected :'(
# it turns out that putting these here doesn't actually help
# I'll leave these here for the time being, commented out.

# # forge a missing installation
# test_that("check_tf_version errors when have_python, _tf, or _tfp is FALSE", {
#     # mockery::stub(check_tf_version, 'reticulate::py_module_available', FALSE)
#     mockery::stub(check_tf_version, 'have_python', FALSE)
#     mockery::stub(check_tf_version, 'have_tf', FALSE)
#     mockery::stub(check_tf_version, 'have_tfp', FALSE)
#
#     expect_snapshot(error = TRUE,
#       check_tf_version("error")
#       )
#
#     expect_snapshot_warning(
#       check_tf_version("warn")
#     )
#
#     expect_snapshot(
#       check_tf_version("message")
#     )
#
# })
#
# test_that("check_tf_version fails when tfp not available", {
#     greta_stash$python_has_been_initialised <- FALSE
#     mockery::stub(check_tf_version, 'have_tfp', FALSE)
#     expect_error(
#       check_tf_version("error")
#     )
#   })
#
# test_that("greta_sitrep warns when have_python, _tf, or _tfp is FALSE", {
#   skip_if_not(check_tf_version())
#   skip_on_ci()
#   skip_on_cran()
#   mockery::stub(greta_sitrep, 'have_python', FALSE, 2)
#   mockery::stub(greta_sitrep, 'have_tf', TRUE, 2)
#   mockery::stub(greta_sitrep, 'have_tfp', TRUE, 2)
#
#   expect_snapshot(
#     greta_sitrep(),
#     cnd_class = FALSE
#   )
#
#   mockery::stub(greta_sitrep, 'have_python', FALSE, 2)
#   mockery::stub(greta_sitrep, 'have_tf', FALSE, 2)
#   mockery::stub(greta_sitrep, 'have_tfp', TRUE, 2)
#
#   expect_snapshot(
#     greta_sitrep(),
#     cnd_class = FALSE
#   )
#
#   mockery::stub(greta_sitrep, 'have_python', FALSE, 2)
#   mockery::stub(greta_sitrep, 'have_tf', FALSE, 2)
#   mockery::stub(greta_sitrep, 'have_tfp', FALSE, 2)
#
#   expect_snapshot(
#     greta_sitrep(),
#     cnd_class = FALSE
#   )
#
#   mockery::stub(greta_sitrep, 'have_python', TRUE, 2)
#   mockery::stub(greta_sitrep, 'have_tf', FALSE, 2)
#   mockery::stub(greta_sitrep, 'have_tfp', FALSE, 2)
#
#   expect_snapshot(
#     greta_sitrep(),
#     cnd_class = FALSE
#   )
#
#   mockery::stub(greta_sitrep, 'have_python', FALSE, 2)
#   mockery::stub(greta_sitrep, 'have_tf', TRUE, 2)
#   mockery::stub(greta_sitrep, 'have_tfp', FALSE, 2)
#
#   expect_snapshot(
#     greta_sitrep(),
#     cnd_class = FALSE
#   )
#
#   mockery::stub(greta_sitrep, 'have_python', FALSE, 2)
#   mockery::stub(greta_sitrep, 'have_tf', FALSE, 2)
#   mockery::stub(greta_sitrep, 'have_tfp', TRUE, 2)
#
#   expect_snapshot(
#     greta_sitrep(),
#     cnd_class = FALSE
#   )
#
#   mockery::stub(greta_sitrep, 'have_python', TRUE, 2)
#   mockery::stub(greta_sitrep, 'have_tf', TRUE, 2)
#   mockery::stub(greta_sitrep, 'have_tfp', FALSE, 2)
#
#   expect_snapshot(
#     greta_sitrep(),
#     cnd_class = FALSE
#   )
#
#   mockery::stub(greta_sitrep, 'have_python', TRUE, 2)
#   mockery::stub(greta_sitrep, 'have_tf', TRUE, 2)
#   mockery::stub(greta_sitrep, 'have_tfp', TRUE, 2)
#
#   expect_snapshot(
#     greta_sitrep(),
#     cnd_class = FALSE
#   )
#
# })
#
# test_that("greta_sitrep warns when different versions of python, tf, tfp", {
#   skip_if_not(check_tf_version())
#   skip_on_ci()
#   skip_on_cran()
#   mockery::stub(greta_sitrep, 'reticulate::py_version', "3.6", 2)
#
#   expect_snapshot(
#     greta_sitrep(),
#     cnd_class = FALSE
#   )
#
#   mockery::stub(greta_sitrep, 'version_tf', "2.0.0", 2)
#
#   expect_snapshot(
#     greta_sitrep(),
#     cnd_class = FALSE
#   )
#
#   mockery::stub(greta_sitrep, 'version_tfp', "0.9.0", 2)
#
#   expect_snapshot(
#     greta_sitrep(),
#     cnd_class = FALSE
#   )
#
# })
