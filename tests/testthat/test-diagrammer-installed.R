test_that("DiagrammeR installation is checked", {
  m <- model(normal(0,1))
  mockery::stub(plot, 'is_DiagrammeR_installed', FALSE)
    expect_snapshot(
      error = TRUE,
      x = plot(m)
      )
})
