test_that("limit_tf_cores_under_check caps TF thread pools under R CMD check", {
  withr::local_envvar("_R_CHECK_LIMIT_CORES_" = "TRUE")
  intra <- NULL
  inter <- NULL
  fake_threading <- list(
    set_intra_op_parallelism_threads = \(n) intra <<- n,
    set_inter_op_parallelism_threads = \(n) inter <<- n
  )

  expect_true(limit_tf_cores_under_check(threading = fake_threading))
  expect_equal(intra, 2L)
  expect_equal(inter, 2L)
})

test_that("limit_tf_cores_under_check does nothing when not under R CMD check", {
  called <- FALSE
  fake_threading <- list(
    set_intra_op_parallelism_threads = \(n) called <<- TRUE,
    set_inter_op_parallelism_threads = \(n) called <<- TRUE
  )

  withr::local_envvar("_R_CHECK_LIMIT_CORES_" = NA)
  expect_false(limit_tf_cores_under_check(threading = fake_threading))
  expect_false(called)

  withr::local_envvar("_R_CHECK_LIMIT_CORES_" = "false")
  expect_false(limit_tf_cores_under_check(threading = fake_threading))
  expect_false(called)
})

test_that("limit_tf_cores_under_check survives TensorFlow errors", {
  withr::local_envvar("_R_CHECK_LIMIT_CORES_" = "TRUE")
  fake_threading <- list(
    set_intra_op_parallelism_threads = \(n) {
      stop("cannot modify after context init")
    },
    set_inter_op_parallelism_threads = \(n) NULL
  )

  expect_false(limit_tf_cores_under_check(threading = fake_threading))
})
