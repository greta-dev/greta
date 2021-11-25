test_that("DiagrammeR installation is checked", {
  mockery::stub(
    where = plot,
    what = 'is_DiagrammeR_installed',
    how = FALSE
    )
  m <- model(normal(0,1))
    expect_snapshot(
      error = TRUE,
      x = plot(m)
      )
})
