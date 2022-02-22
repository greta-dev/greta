test_that("head and tail work", {
  skip_if_not(check_tf_version())

  a <- randn(10, 1)
  b <- randn(10, 4)
  c <- randn(10, 3, 3)

  check_op(head, a)
  check_op(tail, a)

  check_op(head, b)
  check_op(tail, b)

  check_op(head, c)
  check_op(tail, c)
})
