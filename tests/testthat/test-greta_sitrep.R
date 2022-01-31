test_that("greta_sitrep warns when have_python, _tf, or _tfp is FALSE", {
  skip_on_ci()
  skip_on_cran()
  mockery::stub(greta_sitrep, 'have_python', FALSE, 2)
  mockery::stub(greta_sitrep, 'have_tf', TRUE, 2)
  mockery::stub(greta_sitrep, 'have_tfp', TRUE, 2)

  expect_snapshot(
    greta_sitrep()
  )

  mockery::stub(greta_sitrep, 'have_python', FALSE, 2)
  mockery::stub(greta_sitrep, 'have_tf', FALSE, 2)
  mockery::stub(greta_sitrep, 'have_tfp', TRUE, 2)

  expect_snapshot(
    greta_sitrep()
  )

  mockery::stub(greta_sitrep, 'have_python', FALSE, 2)
  mockery::stub(greta_sitrep, 'have_tf', FALSE, 2)
  mockery::stub(greta_sitrep, 'have_tfp', FALSE, 2)

  expect_snapshot(
    greta_sitrep()
  )

  mockery::stub(greta_sitrep, 'have_python', TRUE, 2)
  mockery::stub(greta_sitrep, 'have_tf', FALSE, 2)
  mockery::stub(greta_sitrep, 'have_tfp', FALSE, 2)

  expect_snapshot(
    greta_sitrep()
  )

  mockery::stub(greta_sitrep, 'have_python', FALSE, 2)
  mockery::stub(greta_sitrep, 'have_tf', TRUE, 2)
  mockery::stub(greta_sitrep, 'have_tfp', FALSE, 2)

  expect_snapshot(
    greta_sitrep()
  )

  mockery::stub(greta_sitrep, 'have_python', FALSE, 2)
  mockery::stub(greta_sitrep, 'have_tf', FALSE, 2)
  mockery::stub(greta_sitrep, 'have_tfp', TRUE, 2)

  expect_snapshot(
    greta_sitrep()
  )


  mockery::stub(greta_sitrep, 'have_python', TRUE, 2)
  mockery::stub(greta_sitrep, 'have_tf', TRUE, 2)
  mockery::stub(greta_sitrep, 'have_tfp', FALSE, 2)

  expect_snapshot(
    greta_sitrep()
  )

  mockery::stub(greta_sitrep, 'have_python', TRUE, 2)
  mockery::stub(greta_sitrep, 'have_tf', TRUE, 2)
  mockery::stub(greta_sitrep, 'have_tfp', TRUE, 2)

  expect_snapshot(
    greta_sitrep()
  )

})

test_that("greta_sitrep warns when different versions of python, tf, tfp", {
  skip_on_ci()
  skip_on_cran()
  mockery::stub(greta_sitrep, 'reticulate::py_version', "3.6", 2)

  expect_snapshot(
    greta_sitrep()
  )

  mockery::stub(greta_sitrep, 'version_tf', "2.0.0", 2)

  expect_snapshot(
    greta_sitrep()
  )

  mockery::stub(greta_sitrep, 'version_tfp', "0.9.0", 2)

  expect_snapshot(
    greta_sitrep()
  )


})
