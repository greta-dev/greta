set.seed(2020 - 02 - 11)

test_that("opt converges with TF optimisers", {
  skip_if_not(check_tf_version())

  x <- rnorm(5, 2, 0.1)
  z <- variable(dim = 5)
  distribution(x) <- normal(z, 0.1)

  m <- model(z)

  optimisers <- tibble::lst(
    gradient_descent,
    adadelta,
    adagrad,
    adam,
    adamax,
    ftrl,
    nadam,
    rms_prop
  )

  opt_df <- opt_df_run(optimisers, m, x)
  tidied_opt <- tidy_optimisers(opt_df, tolerance = 1e-2)

  # one expectation per optimiser: asserting over all of them at once reports
  # only that something failed, and the suite has eight to choose from
  for (i in seq_len(nrow(tidied_opt))) {
    optimiser <- tidied_opt$opt[[i]]
    expect_equal(tidied_opt$convergence[[i]], 0, label = optimiser)
    expect_lte(tidied_opt$iterations[[i]], 200, label = optimiser)
    expect_lt(max(tidied_opt$par_x_diff[[i]]), 1e-2, label = optimiser)
  }
})

test_that("opt gives appropriate warning with deprecated optimisers in TFP", {
  skip_if_not(check_tf_version())

  x <- rnorm(5, 2, 0.1)
  z <- variable(dim = 5)
  distribution(x) <- normal(z, 0.1)

  m <- model(z)

  expect_snapshot_warning(
    opt(m, optimiser = adagrad_da())
  )
  expect_snapshot_warning(
    opt(m, optimiser = proximal_adagrad())
  )
  expect_snapshot_warning(
    opt(m, optimiser = proximal_gradient_descent())
  )
})

test_that("opt converges with TFP optimisers", {
  skip_if_not(check_tf_version())

  x <- rnorm(3, 2, 0.1)
  z <- variable(dim = 3)
  distribution(x) <- normal(z, 0.1)

  m <- model(z)

  # There are only 2 TFP optimisers: bfgs & nelder_mead
  # check through each individually
  expect_snapshot(
    o <- opt(m, optimiser = bfgs(), max_iterations = 500)
  )

  # should have converged in fewer than 500 iterations and be close to truth
  expect_identical(o$convergence, 0)
  expect_lte(o$iterations, 500)
  expect_true(all(abs(x - o$par$z) < 1e-2))

  expect_snapshot(
    o <- opt(m, optimiser = nelder_mead(), max_iterations = 500)
  )

  # should have converged in fewer than 500 iterations and be close to truth
  expect_identical(o$convergence, 0)
  expect_lte(o$iterations, 500)
  expect_true(all(abs(x - o$par$z) < 1e-2))
})

test_that("opt reports convergence as the optimiser does", {
  skip_if_not(check_tf_version())
  x <- normal(0, 1, dim = 3)
  m <- model(x)

  # BFGS reaches the optimum of a standard normal in one iteration, one short
  # of an iteration limit of two. greta-dev/greta#569
  o <- opt(m, optimiser = bfgs(), max_iterations = 2)
  expect_equal(o$iterations, 1)
  expect_identical(o$convergence, 0)
})

test_that("opt fails with defunct optimisers", {
  skip_if_not(check_tf_version())

  x <- rnorm(3, 2, 0.1)
  z <- variable(dim = 3)
  distribution(x) <- normal(z, 0.1)

  m <- model(z)

  # check that the right ones error about defunct
  expect_snapshot(error = TRUE, o <- opt(m, optimiser = powell()))
  expect_snapshot(error = TRUE, o <- opt(m, optimiser = momentum()))
  expect_snapshot(error = TRUE, o <- opt(m, optimiser = cg()))
  expect_snapshot(error = TRUE, o <- opt(m, optimiser = newton_cg()))
  expect_snapshot(error = TRUE, o <- opt(m, optimiser = l_bfgs_b()))
  expect_snapshot(error = TRUE, o <- opt(m, optimiser = tnc()))
  expect_snapshot(error = TRUE, o <- opt(m, optimiser = cobyla()))
  expect_snapshot(error = TRUE, o <- opt(m, optimiser = slsqp()))
})

test_that("opt accepts initial values for TF optimisers", {
  skip_if_not(check_tf_version())

  x <- rnorm(5, 2, 0.1)
  z <- variable(dim = 5)
  distribution(x) <- normal(z, 0.1)

  m <- model(z)
  o <- opt(
    m,
    initial_values = initials(z = rnorm(5)),
    optimiser = gradient_descent()
  )

  # should have converged
  expect_identical(o$convergence, 0)

  # should be fewer than 100 iterations
  expect_lte(o$iterations, 100)

  # should be close to the truth
  expect_true(all(abs(x - o$par$z) < 1e-3))
})

test_that("a second opt() call reuses the traced optimiser and starts afresh", {
  skip_if_not(check_tf_version())
  x <- normal(0, 1, dim = 2)
  m <- model(x)
  optimise <- function() {
    opt(
      m,
      optimiser = adam(),
      initial_values = initials(x = c(1, -1)),
      max_iterations = 50
    )
  }

  first <- optimise()
  # adam()'s moments and iteration count are restarted, so the same initial
  # values give the same result
  expect_identical(optimise(), first)
  loops <- m$dag$optimiser_functions
  expect_length(loops, 1)
  expect_identical(trace_count(loops[[1]]$minimise), 1L)
})

test_that("opt() calls with different settings on one model match new models", {
  skip_if_not(check_tf_version())
  schedule <- function(rate) {
    tf$keras$optimizers$schedules$ExponentialDecay(
      initial_learning_rate = rate,
      decay_steps = 5L,
      decay_rate = 0.5
    )
  }
  # the two schedules are Python objects, which can't key a cached loop
  settings <- list(
    list(optimiser = adam(learning_rate = 0.1), adjust = TRUE),
    list(optimiser = adam(learning_rate = 0.2), adjust = TRUE),
    list(optimiser = adam(learning_rate = 0.1), adjust = FALSE),
    list(optimiser = adam(learning_rate = schedule(0.001)), adjust = TRUE),
    list(optimiser = adam(learning_rate = schedule(0.5)), adjust = TRUE)
  )
  # initials() finds x by name where opt() is called
  optimise <- function(m, x, setting) {
    opt(
      m,
      optimiser = setting$optimiser,
      adjust = setting$adjust,
      initial_values = initials(x = c(1, 2)),
      max_iterations = 30
    )
  }
  optimise_new_model <- function(setting) {
    x <- lognormal(0, 1, dim = 2)
    optimise(model(x), x, setting)
  }

  x <- lognormal(0, 1, dim = 2)
  m <- model(x)
  on_one_model <- lapply(settings, \(setting) optimise(m, x, setting))
  on_new_models <- lapply(settings, optimise_new_model)
  expect_identical(on_one_model, on_new_models)
  # one loop at a time, the last cached
  expect_length(m$dag$optimiser_functions, 1)
})

test_that("opt() traces again for a setting past 15 digits or another device", {
  skip_if_not(check_tf_version())
  x <- normal(0, 1, dim = 2)
  m <- model(x)
  loop_key <- function(...) {
    opt(m, max_iterations = 5, ...)
    names(m$dag$optimiser_functions)
  }

  first <- loop_key(optimiser = adam(learning_rate = 0.1))
  # 0.1 to 15 significant digits, so a key that rounds would share a loop
  nearby_rate <- 0.1 * (1 + 4 * .Machine$double.eps)
  nearby <- loop_key(optimiser = adam(learning_rate = nearby_rate))
  on_gpu <- suppressMessages(
    loop_key(
      optimiser = adam(learning_rate = 0.1),
      compute_options = gpu_only()
    )
  )
  expect_false(identical(nearby, first))
  expect_false(identical(on_gpu, first))
})

test_that("an opt() loop traced for one call does not outlive it", {
  skip_if_not(check_tf_version())
  python <- reticulate::py_run_string(
    "
def count_traced_functions():
    import gc
    import tensorflow as tf
    gc.collect()
    traced = tf.types.experimental.GenericFunction
    return sum(isinstance(o, traced) for o in gc.get_objects())
",
    local = TRUE
  )
  count_traced_functions <- function() {
    gc()
    python$count_traced_functions()
  }
  x <- normal(0, 1, dim = 2)
  m <- model(x)
  # a learning rate schedule is a Python object, so opt() traces a loop for
  # each call rather than keep one on the model
  schedule <- tf$keras$optimizers$schedules$ExponentialDecay(
    initial_learning_rate = 0.1,
    decay_steps = 5L,
    decay_rate = 0.5
  )
  optimise <- function() {
    opt(m, optimiser = adam(learning_rate = schedule), max_iterations = 5)
  }

  optimise()
  before <- count_traced_functions()
  for (i in 1:3) {
    optimise()
  }
  expect_identical(count_traced_functions(), before)
})

test_that("opt accepts initial values for TFP optimisers", {
  skip_if_not(check_tf_version())

  x <- rnorm(5, 2, 0.1)
  z <- variable(dim = 5)
  distribution(x) <- normal(z, 0.1)

  m <- model(z)
  o <- opt(
    m,
    initial_values = initials(z = rnorm(5)),
    optimiser = bfgs()
  )

  # should have converged
  expect_identical(o$convergence, 0)

  # should be fewer than 100 iterations
  expect_lte(o$iterations, 100)

  # should be close to the truth
  expect_true(all(abs(x - o$par$z) < 1e-3))
})

test_that("TF opt returns a hessian", {
  skip_if_not(check_tf_version())

  sd <- runif(5)
  x <- rnorm(5, 2, 0.1)
  z <- variable(dim = 5)
  distribution(x) <- normal(z, sd)

  m <- model(z)
  o <- opt(m, hessian = TRUE, optimiser = adam())

  hess <- o$hessian$z

  # should be a 5x5 numeric matrix
  expect_true(inherits(hess, "matrix"))
  expect_type(hess, "double")
  expect_identical(dim(hess), c(5L, 5L))

  # the model density is IID normal, so we should be able to recover the SD
  approx_sd <- sqrt(diag(solve(hess)))
  expect_true(all(abs(approx_sd - sd) < 1e-9))
})

test_that("TF opt with `gradient_descent` fails with bad initial values", {
  skip_if_not(check_tf_version())

  sd <- c(
    0.506878940621391,
    0.923730184091255,
    0.920889702159911,
    0.505555843701586,
    0.0164170106872916
  )

  x <- c(
    2.02058743665058,
    2.03576151926688,
    2.05396624437729,
    1.76648340291467,
    1.95296190632083
  )

  z <- variable(dim = 5)
  distribution(x) <- normal(z, sd)

  m <- model(z)
  expect_snapshot(
    error = TRUE,
    o <- opt(m, hessian = TRUE, optimiser = gradient_descent())
  )
})

test_that("TF opt with `adam` succeeds with bad initial values", {
  skip_if_not(check_tf_version())

  sd <- c(
    0.506878940621391,
    0.923730184091255,
    0.920889702159911,
    0.505555843701586,
    0.0164170106872916
  )

  x <- c(
    2.02058743665058,
    2.03576151926688,
    2.05396624437729,
    1.76648340291467,
    1.95296190632083
  )

  z <- variable(dim = 5)
  distribution(x) <- normal(z, sd)

  m <- model(z)
  expect_ok(
    o <- opt(m, hessian = TRUE, optimiser = adam())
  )

  hess <- o$hessian$z

  # should be a 5x5 numeric matrix
  expect_true(inherits(hess, "matrix"))
  expect_type(hess, "double")
  expect_identical(dim(hess), c(5L, 5L))

  # the model density is IID normal, so we should be able to recover the SD
  approx_sd <- sqrt(diag(solve(hess)))
  expect_true(all(abs(approx_sd - sd) < 1e-9))
})

##

test_that("TF opt returns multiple hessians", {
  skip_if_not(check_tf_version())

  sd <- runif(5)
  x <- rnorm(5, 2, 0.1)
  z1 <- variable(dim = 1)
  z2 <- variable(dim = 1)
  z3 <- variable(dim = 1)
  z4 <- variable(dim = 1)
  z5 <- variable(dim = 1)
  z <- c(z1, z2, z3, z4, z5)
  distribution(x) <- normal(z, sd)

  m <- model(z1, z2, z3, z4, z5)
  o <- opt(m, hessian = TRUE, optimiser = gradient_descent())

  hess <- o$hessian

  # should be a 5x5 numeric matrix
  expect_true(inherits(hess, "list"))
  expect_true(length(hess) == 5)
  expect_true(all(sapply(hess, is.matrix)))
  hess_dims <- lapply(hess, dim)
  expect_true(all(sapply(hess_dims, identical, c(1L, 1L))))

  # the model density is IID normal, so we should be able to recover the SD
  approx_sd <- sqrt(1 / unlist(hess))
  expect_true(all(abs(approx_sd - sd) < 1e-9))
})

test_that("hessians are right on both sides of the pfor threshold", {
  skip_if_not(check_tf_version())

  # the jacobian uses a while loop below pfor_min_elements() and vectorises
  # with pfor from it, so take a target on each side, sized from the
  # threshold so both routes stay covered if it moves. An IID normal's hessian
  # is diagonal with entries 1 / sd^2 wherever the optimiser stops, so one
  # step is enough
  recovered_sd <- function(d) {
    sd <- runif(d)
    x <- rnorm(d, 2, 0.1)
    z <- variable(dim = d)
    distribution(x) <- normal(z, sd)
    m <- model(z)
    o <- opt(m, hessian = TRUE, optimiser = adam(), max_iterations = 1)
    list(recovered = sqrt(1 / diag(drop(o$hessian$z))), sd = sd)
  }

  while_loop <- recovered_sd(pfor_min_elements() %/% 2)
  expect_equal(while_loop$recovered, while_loop$sd)

  pfor <- recovered_sd(pfor_min_elements())
  expect_equal(pfor$recovered, pfor$sd)
})

test_that("TFP opt returns a hessian", {
  skip_if_not(check_tf_version())

  sd <- runif(5)
  x <- rnorm(5, 2, 0.1)
  z <- variable(dim = 5)
  distribution(x) <- normal(z, sd)

  m <- model(z)
  o <- opt(m, hessian = TRUE)

  hess <- o$hessian$z

  # should be a 5x5 numeric matrix
  expect_true(inherits(hess, "matrix"))
  expect_type(hess, "double")
  expect_identical(dim(hess), c(5L, 5L))

  # the model density is IID normal, so we should be able to recover the SD
  approx_sd <- sqrt(diag(solve(hess)))
  expect_true(all(abs(approx_sd - sd) < 1e-9))
})

test_that("TFP opt returns multiple hessian", {
  skip_if_not(check_tf_version())

  sd <- runif(5)
  x <- rnorm(5, 2, 0.1)
  z1 <- variable(dim = 1)
  z2 <- variable(dim = 1)
  z3 <- variable(dim = 1)
  z4 <- variable(dim = 1)
  z5 <- variable(dim = 1)
  z <- c(z1, z2, z3, z4, z5)
  distribution(x) <- normal(z, sd)

  m <- model(z1, z2, z3, z4, z5)
  o <- opt(m, hessian = TRUE)

  hess <- o$hessian

  # should be a 5x5 numeric matrix
  expect_true(inherits(hess, "list"))
  expect_true(length(hess) == 5)
  expect_true(all(sapply(hess, is.matrix)))
  hess_dims <- lapply(hess, dim)
  expect_true(all(sapply(hess_dims, identical, c(1L, 1L))))

  # the model density is IID normal, so we should be able to recover the SD
  approx_sd <- sqrt(1 / unlist(hess))
  expect_true(all(abs(approx_sd - sd) < 1e-9))
})
