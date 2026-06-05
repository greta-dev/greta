test_that("as.unknowns() handles bare numeric vectors", {
  expect_s3_class(as.unknowns(1.5), "unknowns")
  expect_s3_class(as.unknowns(c(1, 2, 3)), "unknowns")
  expect_s3_class(as.unknowns(1L), "unknowns")
})

test_that("as.unknowns() still works on arrays and matrices", {
  expect_s3_class(as.unknowns(array(1.5, dim = c(1, 1))), "unknowns")
  expect_s3_class(as.unknowns(matrix(1:4, 2, 2)), "unknowns")
})

test_that("setting dim<- to NULL on an unknowns does not error", {
  u <- unknowns(c(2, 2))
  expect_no_error(dim(u) <- NULL)
  expect_s3_class(u, "unknowns")
})

test_that("reshaping a greta array keeps unknowns placeholders (#582)", {
  v <- normal(0, 1, dim = 6)
  dim(v) <- c(2, 3)
  expect_true(is.unknowns(get_node(v)$value()))
})

test_that("outer() on a greta array keeps unknowns placeholders (#582)", {
  lnrho <- normal(0, 1, dim = 3)
  t_mat <- pmax(outer(1:5, 1:5, "-"), 0)
  arr <- outer(t_mat, lnrho, FUN = "*")
  expect_true(is.unknowns(get_node(arr)$value()))
})
