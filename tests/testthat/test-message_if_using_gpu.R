test_that("message_if_using_gpu gives the correct message for cpu or gpu use", {
  expect_snapshot(
    message_if_using_gpu(cpu_only())
  )
  expect_snapshot(
    message_if_using_gpu(gpu_only())
  )
})

test_that("message_if_using_gpu does not message when option set",{
  withr::local_options(
    list("greta_gpu_message" = FALSE)
  )

  expect_snapshot(
    message_if_using_gpu(gpu_only())
  )

})

test_that("message_if_using_gpu does message when option set",{
  withr::local_options(
    list("greta_gpu_message" = TRUE)
  )

  expect_snapshot(
    message_if_using_gpu(gpu_only())
  )

})

test_that("is_using_gpu and is_using_cpu work",{

  expect_true(is_using_gpu(gpu_only()))
  expect_false(is_using_gpu(cpu_only()))

  expect_true(is_using_cpu(cpu_only()))
  expect_false(is_using_cpu(gpu_only()))
})

test_that("calculate provides a message when GPU is set", {
  skip_if_not(check_tf_version())

  x <- normal(0, 1)

  expect_snapshot(
    calc_x <- calculate(x, nsim = 1, compute_options = gpu_only())
  )
  expect_snapshot(
    calc_x <- calculate(x, nsim = 1, compute_options = cpu_only())
  )

})

test_that("calculate/mcmc does not message when option set",{
  skip_if_not(check_tf_version())
  withr::local_options(
    list("greta_gpu_message" = FALSE)
  )

  x <- normal(0, 1)

  expect_snapshot(
    calc_x <- calculate(x, nsim = 1, compute_options = gpu_only())
  )

  m <- model(x)

  expect_snapshot(
    mcmc_m <- mcmc(model = m,
                   n_samples = 1,
                   warmup = 0,
                   compute_options = gpu_only(),
                   verbose = FALSE)
  )

})

test_that("calculate/mcmc does message when option set",{
  skip_if_not(check_tf_version())
  withr::local_options(
    list("greta_gpu_message" = TRUE)
  )

  x <- normal(0, 1)

  expect_snapshot(
    calc_x <- calculate(x, nsim = 1, compute_options = gpu_only())
  )

  m <- model(x)

  expect_snapshot(
    mcmc_m <- mcmc(model = m,
                   n_samples = 1,
                   warmup = 0,
                   compute_options = gpu_only(),
                   verbose = FALSE)
  )

})

test_that("mcmc provides a message when GPU is set", {
  skip_if_not(check_tf_version())

  x <- normal(0, 1)
  m <- model(x)

  expect_snapshot(
  mcmc_gpu <- mcmc(model = m,
         n_samples = 1,
         warmup = 0,
         compute_options = gpu_only(),
         verbose = FALSE)
  )

  expect_snapshot(
  mcmc_cpu <- mcmc(model = m,
         n_samples = 1,
         warmup = 0,
         compute_options = cpu_only(),
         verbose = FALSE)
  )

})

test_that("mcmc prints out CPU and GPU text", {
  skip_if_not(check_tf_version())

  x <- normal(0,1)
  m <- model(x)
  expect_snapshot(
    draws <- mcmc(m, n_samples = 5, warmup = 5, compute_options = cpu_only())
  )
  expect_snapshot(
    draws <- mcmc(m, n_samples = 5, warmup = 5, compute_options = gpu_only())
  )
})
