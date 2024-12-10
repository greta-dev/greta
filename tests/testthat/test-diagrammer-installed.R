test_that("DiagrammeR installation is checked", {
  skip_if_not(check_tf_version())
  skip_on_cran()
  local_mocked_bindings(
    is_DiagrammeR_installed = function() FALSE
  )
  m <- model(normal(0,1))
    expect_snapshot(
      error = TRUE,
      x = plot(m)
      )
})
