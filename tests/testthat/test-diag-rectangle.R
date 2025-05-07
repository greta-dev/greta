test_that("diag() works for non square matrices", {
  mat <- matrix(1:12, 3, 4)
  g_mat <- as_data(mat)
  mat
  diag_mat <- diag(mat)
  diag_g_mat <- diag(g_mat)

  expect_equal(diag_mat, diag_g_mat)
})
