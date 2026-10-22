test_that("tf_function_weakly() autographs the function it traces", {
  skip_if_not(check_tf_version())
  sign_of <- function(x) {
    if (x > 0) {
      tf$constant(1)
    } else {
      tf$constant(-1)
    }
  }
  traced <- tf_function_weakly(sign_of)
  expect_equal(as.numeric(traced(tf$constant(-2))), -1)
})

test_that("tf_function_weakly() traces a function with no arguments", {
  skip_if_not(check_tf_version())
  traced <- tf_function_weakly(\() tf$constant(1))
  expect_equal(as.numeric(traced()), 1)
})

test_that("tf_function_weakly() keeps f for as long as its handle, no longer", {
  skip_if_not(check_tf_version())
  freed <- FALSE
  # f reaches the handle, as a dag method reaches the dag that keeps it
  traced <- local({
    held_by_f <- new.env()
    reg.finalizer(held_by_f, \(env) freed <<- TRUE)
    twice <- \(x) x * 2
    held_by_f$traced <- tf_function_weakly(twice)
  })

  collect_garbage()
  expect_false(freed)
  expect_equal(as.numeric(traced(tf$constant(2))), 4)

  rm(traced)
  collect_garbage()
  expect_true(freed)
})

test_that("a traced function whose R function was freed errors clearly", {
  reference <- new.env(parent = emptyenv())
  reference$f <- rlang::new_weakref(new.env(), value = identity)
  gc()
  call_f <- call_weakly_held(reference, formals(identity))
  expect_error(call_f(1), "after the R function it traces was freed")
})
