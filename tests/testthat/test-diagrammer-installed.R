test_that("DiagrammeR installation is checked", {
  skip_on_cran()
  mockery::stub(
    where = plot.greta_model,
    what = 'is_DiagrammeR_installed',
    how = FALSE
    )
  m <- model(normal(0,1))
    expect_snapshot(
      error = TRUE,
      cran = FALSE,
      x = plot(m)
      )
})
