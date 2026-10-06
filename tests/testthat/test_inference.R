set.seed(2020 - 02 - 11)

test_that("bad mcmc proposals are rejected", {
  skip_if_not(check_tf_version())

  # set up for numerical rejection of initial location
  x <- rnorm(10000, 1e60, 1)
  z <- normal(-1e60, 1e-60)
  distribution(x) <- normal(z, 1e-60)
  m <- model(z, precision = "single")

  # # catch badness in the progress bar
  out <- get_output(
    mcmc(m, n_samples = 10, warmup = 0, pb_update = 10)
  )
  expect_match(out, "100% bad")

  expect_snapshot(
    error = TRUE,
    draws <- mcmc(
      m,
      chains = 1,
      n_samples = 2,
      warmup = 0,
      verbose = FALSE,
      initial_values = initials(z = 1e120)
    )
  )

  # really bad proposals
  x <- rnorm(100000, 1e120, 1)
  z <- normal(-1e120, 1e-120)
  distribution(x) <- normal(z, 1e-120)
  m <- model(z, precision = "single")
  expect_snapshot(
    error = TRUE,
    mcmc(m, chains = 1, n_samples = 1, warmup = 0, verbose = FALSE)
  )

  # proposals that are fine, but rejected anyway
  z <- normal(0, 1)
  m <- model(z, precision = "single")
  expect_ok(mcmc(
    m,
    hmc(
      epsilon = 100,
      Lmin = 1,
      Lmax = 1
    ),
    chains = 1,
    n_samples = 5,
    warmup = 0,
    verbose = FALSE
  ))
})

test_that("mcmc works with verbosity and warmup", {
  skip_if_not(check_tf_version())

  x <- rnorm(10)
  z <- normal(0, 1)
  distribution(x) <- normal(z, 1)
  m <- model(z)
  quietly(expect_ok(mcmc(m, n_samples = 50, warmup = 50, verbose = TRUE)))
})


test_that("mcmc works with cpu and gpu options", {
  skip_if_not(check_tf_version())

  x <- rnorm(10)
  z <- normal(0, 1)
  distribution(x) <- normal(z, 1)
  m <- model(z)
  quietly(
    expect_ok(mcmc(m, n_samples = 5, warmup = 5, compute_options = cpu_only()))
  )
  quietly(
    expect_ok(mcmc(m, n_samples = 5, warmup = 5, compute_options = gpu_only()))
  )
})

test_that("cpu_only() and gpu_only() can both be used in one session", {
  skip_if_not(check_tf_version())
  x <- normal(0, 1)
  m <- model(x)

  # tensorflow::set_random_seed() would set CUDA_VISIBLE_DEVICES = -1 for the
  # rest of the session, hiding the GPU from every later run
  before <- Sys.getenv("CUDA_VISIBLE_DEVICES")

  expect_ok(mcmc(
    m,
    n_samples = 2,
    warmup = 2,
    chains = 1,
    verbose = FALSE,
    compute_options = cpu_only()
  ))
  expect_ok(mcmc(
    m,
    n_samples = 2,
    warmup = 2,
    chains = 1,
    verbose = FALSE,
    compute_options = gpu_only()
  ))
  expect_ok(mcmc(
    m,
    n_samples = 2,
    warmup = 2,
    chains = 1,
    verbose = FALSE,
    compute_options = cpu_only()
  ))

  expect_identical(Sys.getenv("CUDA_VISIBLE_DEVICES"), before)
})

test_that("mcmc works with multiple chains", {
  skip_if_not(check_tf_version())

  x <- rnorm(10)
  z <- normal(0, 1)
  distribution(x) <- normal(z, 1)
  m <- model(z)

  # multiple chains, automatic initial values
  quietly(expect_ok(mcmc(
    m,
    warmup = 10,
    n_samples = 10,
    chains = 2,
    verbose = FALSE
  )))

  # multiple chains, user-specified initial values
  inits <- list(initials(z = 1), initials(z = 2))
  quietly(expect_ok(mcmc(
    m,
    warmup = 10,
    n_samples = 10,
    chains = 2,
    initial_values = inits,
    verbose = FALSE
  )))
})

test_that("mcmc handles initial values nicely", {
  skip_if_not(check_tf_version())

  # preserve R version
  current_r_version <- paste0(R.version$major, ".", R.version$minor)
  required_r_version <- "3.6.0"
  old_rng_r <- compareVersion(required_r_version, current_r_version) <= 0

  if (old_rng_r) {
    suppressWarnings(expr = {
      RNGkind(sample.kind = "Rounding")
      set.seed(2020 - 02 - 11)
    })
  }

  x <- rnorm(10)
  z <- normal(0, 1)
  distribution(x) <- normal(z, 1)
  m <- model(z)

  # too many sets of initial values
  inits <- replicate(3, initials(z = rnorm(1)), simplify = FALSE)
  expect_snapshot(
    error = TRUE,
    draws <- mcmc(
      m,
      warmup = 10,
      n_samples = 10,
      verbose = FALSE,
      chains = 2,
      initial_values = inits
    )
  )

  # initial values have the wrong length
  inits <- replicate(2, initials(z = rnorm(2)), simplify = FALSE)
  expect_snapshot(
    error = TRUE,
    draws <- mcmc(
      m,
      warmup = 10,
      n_samples = 10,
      verbose = FALSE,
      chains = 2,
      initial_values = inits
    )
  )

  inits <- initials(z = rnorm(1))
  quietly(
    expect_snapshot(
      draws <- mcmc(
        m,
        warmup = 10,
        n_samples = 10,
        chains = 2,
        initial_values = inits,
        verbose = FALSE
      )
    )
  )
})

test_that("progress bar gives a range of messages", {
  skip_if_not(check_tf_version())

  # 10/1010 should be <1%
  expect_snapshot(draws <- mock_mcmc(1010))

  # 10/500 should be 2%
  expect_snapshot(draws <- mock_mcmc(500))

  # 10/10 should be 100%
  expect_snapshot(draws <- mock_mcmc(10))
})

test_that("progress bar reaches its total when pb_update does not divide it", {
  pb <- create_progress_bar("sampling", c(0, 13), pb_update = 6, width = 50)
  out <- get_output(
    for (it in c(0, 6, 12, 13)) {
      iterate_progress_bar(pb, it, rejects = 0, chains = 1)
    }
  )
  expect_match(out, "13/13")
})

test_that("extra_samples works", {
  skip_if_not(check_tf_version())

  # set up model
  a <- normal(0, 1)
  m <- model(a)

  draws <- mcmc(m, warmup = 10, n_samples = 10, verbose = FALSE)

  more_draws <- extra_samples(draws, 20, verbose = FALSE)

  expect_true(inherits(more_draws, "greta_mcmc_list"))
  expect_true(coda::niter(more_draws) == 30)
  expect_true(coda::nchain(more_draws) == 2)
})

test_that("trace_batch_size works", {
  skip_if_not(check_tf_version())

  # set up model
  a <- normal(0, 1)
  m <- model(a)

  draws <-
    mcmc(
      m,
      warmup = 10,
      n_samples = 10,
      verbose = FALSE,
      trace_batch_size = 3
    )

  more_draws <- extra_samples(draws, 20, verbose = FALSE, trace_batch_size = 6)

  expect_true(inherits(more_draws, "greta_mcmc_list"))
  expect_true(coda::niter(more_draws) == 30)
  expect_true(coda::nchain(more_draws) == 2)
})

test_that("stashed_samples works", {
  skip_if_not(check_tf_version())

  # set up model
  a <- normal(0, 1)
  m <- model(a)

  draws <- mcmc(m, warmup = 10, n_samples = 10, verbose = FALSE)

  # with a completed sample, this should be NULL
  ans <- stashed_samples()
  expect_null(ans)

  # mock up a stash
  stash <- greta:::greta_stash
  samplers_stash <- replicate(
    2,
    list(
      traced_free_state = list(as.matrix(rnorm(17))),
      traced_values = list(as.matrix(rnorm(17))),
      thin = 1,
      model = m
    ),
    simplify = FALSE
  )
  assign("samplers", samplers_stash, envir = stash)

  # should convert to a greta_mcmc_list
  ans <- stashed_samples()
  expect_s3_class(ans, "greta_mcmc_list")

  # model_info attribute should have raw draws and the model
  model_info <- attr(ans, "model_info")
  expect_true(inherits(model_info, "list"))
  expect_s3_class(model_info$raw_draws, "mcmc.list")
  expect_true(inherits(model_info$model, "greta_model"))
})

test_that("samples has object names", {
  skip_if_not(check_tf_version())

  a <- normal(0, 1)
  b <- normal(a, 1, dim = 3)
  m <- model(a, b)

  # mcmc should give the right names
  draws <- mcmc(m, warmup = 2, n_samples = 10, verbose = FALSE)
  expect_snapshot(rownames(summary(draws)$statistics))

  # so should calculate
  c <- b^2
  c_draws <- calculate(c, values = draws)
  expect_snapshot(rownames(summary(c_draws)$statistics))
})


test_that("model errors nicely", {
  skip_if_not(check_tf_version())

  # model should give a nice error if passed something other than a greta array
  a <- 1
  b <- normal(0, a)
  expect_snapshot(error = TRUE, model(a, b))
})

test_that("hmc() draws its leapfrog count every iteration", {
  skip_if_not(check_tf_version())
  x <- as_data(rep(0, 10))
  z <- normal(0, 10)
  distribution(x) <- normal(z, 1)
  m <- model(z)

  # With a step size equal to the posterior sd, each leapfrog step turns the
  # chain a sixth of a circle, so 6 steps bring every proposal back to where
  # it started and 9 steps to its mirror image. A leapfrog count drawn once for
  # the whole call freezes the chain whenever it is 6 or 9.
  # greta-dev/greta#547
  posterior_sd <- 1 / sqrt(10 + 1 / 100)
  spread <- vapply(
    1:10,
    function(seed) {
      set.seed(seed)
      draws <- mcmc(
        m,
        sampler = hmc(Lmin = 6, Lmax = 9, epsilon = posterior_sd),
        warmup = 0,
        n_samples = 200,
        chains = 1,
        verbose = FALSE
      )
      sd(abs(as.vector(draws[[1]])))
    },
    numeric(1)
  )
  expect_gt(min(spread), 0.05)
})

test_that("mcmc supports rwmh sampler with normal proposals", {
  skip_if_not(check_tf_version())
  x <- normal(0, 1)
  m <- model(x)
  expect_ok(
    draws <- mcmc(
      m,
      sampler = rwmh("normal"),
      n_samples = 100,
      warmup = 100,
      verbose = FALSE
    )
  )
})

test_that("mcmc supports rwmh sampler with uniform proposals", {
  skip_if_not(check_tf_version())
  set.seed(5)
  x <- uniform(0, 1)
  m <- model(x)
  expect_ok(
    draws <- mcmc(
      m,
      sampler = rwmh("uniform"),
      n_samples = 100,
      warmup = 100,
      verbose = FALSE
    )
  )
})

test_that("mcmc supports slice sampler with single precision models", {
  skip_if_not(check_tf_version())
  set.seed(5)
  x <- uniform(0, 1)
  m <- model(x, precision = "single")
  expect_ok(
    draws <- mcmc(
      m,
      sampler = slice(),
      n_samples = 100,
      warmup = 100,
      verbose = FALSE
    )
  )
})

test_that("initials works", {
  skip_if_not(check_tf_version())

  # errors on bad objects
  expect_snapshot(error = TRUE, initials(a = FALSE))

  expect_snapshot(error = TRUE, initials(FALSE))

  # prints nicely
  expect_snapshot(
    initials(a = 3)
  )
})

test_that("prep_initials errors informatively", {
  skip_if_not(check_tf_version())

  a <- normal(0, 1)
  b <- uniform(0, 1)
  d <- lognormal(0, 1)
  e <- variable(upper = -1)
  f <- ones(1)
  z <- a * b * d * e * f
  m <- model(z)

  # bad objects:
  expect_snapshot(
    error = TRUE,
    mcmc(m, initial_values = FALSE, verbose = FALSE)
  )

  expect_snapshot(
    error = TRUE,
    mcmc(m, initial_values = list(FALSE), verbose = FALSE)
  )

  # an unrelated greta array
  g <- normal(0, 1)
  expect_snapshot(
    error = TRUE,
    mcmc(m, chains = 1, initial_values = initials(g = 1), verbose = FALSE)
  )

  # non-variable greta arrays
  expect_snapshot(
    error = TRUE,
    mcmc(m, chains = 1, initial_values = initials(f = 1), verbose = FALSE)
  )

  expect_snapshot(
    error = TRUE,
    mcmc(m, chains = 1, initial_values = initials(z = 1), verbose = FALSE)
  )

  # out of bounds errors
  expect_snapshot(
    error = TRUE,
    mcmc(m, chains = 1, initial_values = initials(b = -1), verbose = FALSE)
  )

  expect_snapshot(
    error = TRUE,
    mcmc(m, chains = 1, initial_values = initials(d = -1), verbose = FALSE)
  )

  expect_snapshot(
    error = TRUE,
    mcmc(m, chains = 1, initial_values = initials(e = 2), verbose = FALSE)
  )
})

test_that("samplers print informatively", {
  skip_if_not(check_tf_version())

  expect_snapshot(
    hmc()
  )
  expect_snapshot(
    rwmh()
  )
  expect_snapshot(
    slice()
  )
  expect_snapshot(
    hmc(Lmin = 1)
  )

  # # check print sees changed parameters
  # out <- capture_output(hmc(Lmin = 1), TRUE)
  # expect_match(out, "Lmin = 1")
})

test_that("thinning keeps n_samples %/% thin draws, whatever the bursts", {
  skip_if_not(check_tf_version())
  set.seed(5)
  x <- uniform(0, 1)
  m <- model(x)

  # verbose = TRUE, since bursts follow pb_update only when the progress bar
  # is shown. Each case would leave a burst shorter than thin if sampling were
  # cut every pb_update iterations, or every iteration with one_by_one: the
  # last burst (#609, #318), every burst with pb_update below thin, and every
  # burst with one_by_one (#567)
  cases <- list(
    list(n_samples = 1000, thin = 100, pb_update = 101, one_by_one = FALSE),
    list(n_samples = 100, thin = 3, pb_update = 2, one_by_one = FALSE),
    list(n_samples = 30, thin = 2, pb_update = 50, one_by_one = TRUE)
  )
  for (case in cases) {
    quietly(
      draws <- mcmc(
        m,
        n_samples = case$n_samples,
        warmup = 10,
        thin = case$thin,
        pb_update = case$pb_update,
        one_by_one = case$one_by_one,
        chains = 1,
        verbose = TRUE
      )
    )
    expect_equal(coda::niter(draws), case$n_samples %/% case$thin)
    expect_equal(thin(draws), case$thin)
  }

  # extra_samples() takes its own thin and pb_update (#567)
  quietly(draws <- mcmc(m, n_samples = 30, warmup = 10, chains = 1))
  quietly(
    more <- extra_samples(
      draws,
      n_samples = 202,
      thin = 3,
      pb_update = 50,
      verbose = TRUE
    )
  )
  expect_equal(coda::niter(more), 30 + 202 %/% 3)
})

test_that("one_by_one runs one iteration per burst, whatever thin is", {
  skip_if_not(check_tf_version())
  x <- uniform(0, 1)
  m <- model(x)
  draws <- mcmc(
    m,
    warmup = 10,
    n_samples = 30,
    thin = 3,
    one_by_one = TRUE,
    chains = 1,
    verbose = FALSE
  )
  sampler <- get_model_info(draws)$samplers[[1]]
  expect_identical(sampler$n_bursts, 10L + 30L)
  expect_equal(coda::niter(draws), 10)
})

test_that("warmup tunes inside one call to TensorFlow without a progress bar", {
  skip_if_not(check_tf_version())
  x <- normal(0, 1)
  m <- model(x)
  draws <- mcmc(m, warmup = 200, n_samples = 100, chains = 2, verbose = FALSE)
  sampler <- get_model_info(draws)$samplers[[1]]
  # one call for warmup and one for sampling
  expect_identical(sampler$n_bursts, 2L)
  expect_false(isTRUE(all.equal(
    sampler$parameters$epsilon,
    hmc()$parameters$epsilon
  )))
})

test_that("a sampler's function is traced for its number of chains", {
  skip_if_not(check_tf_version())
  x <- normal(0, 1, dim = 2)
  m <- model(x)
  draws <- mcmc(m, warmup = 10, n_samples = 10, chains = 3, verbose = FALSE)
  sampler <- get_model_info(draws)$samplers[[1]]

  signature <- sampler$tf_iterations$input_signature[[1]]
  expect_identical(as.integer(unlist(signature$shape$as_list())), c(3L, 2L))
})

test_that("slice() runs a single chain", {
  skip_if_not(check_tf_version())
  x <- normal(0, 1)
  m <- model(x)
  expect_ok(
    draws <- mcmc(
      m,
      sampler = slice(),
      warmup = 10,
      n_samples = 10,
      chains = 1,
      verbose = FALSE
    )
  )
  expect_equal(coda::niter(draws), 10)
})

test_that("mcmc() traces a model's sampler loop once, across calls", {
  skip_if_not(check_tf_version())
  x <- normal(0, 1)
  m <- model(x)
  sampler_function <- function() {
    draws <- mcmc(m, warmup = 10, n_samples = 10, chains = 2, verbose = FALSE)
    get_model_info(draws)$samplers[[1]]$tf_iterations
  }
  first <- sampler_function()
  second <- sampler_function()
  expect_identical(reticulate::py_id(second), reticulate::py_id(first))
  expect_identical(second$experimental_get_tracing_count(), 1L)
})

test_that("samplers sharing a model get the draws they would get alone", {
  skip_if_not(check_tf_version())
  x <- normal(0, 1)
  m <- model(x)
  runs <- list(
    list(sampler = rwmh("normal"), chains = 2),
    list(sampler = rwmh("uniform"), chains = 2),
    list(sampler = rwmh("normal"), chains = 3),
    list(sampler = hmc(), chains = 2),
    list(sampler = slice(), chains = 2),
    list(sampler = rwmh("normal"), chains = 2)
  )
  draws_from <- function(run) {
    local_greta_seed()
    draws <- mcmc(
      m,
      sampler = run$sampler,
      warmup = 20,
      n_samples = 10,
      chains = run$chains,
      verbose = FALSE
    )
    as.matrix(draws)
  }
  shared <- lapply(runs, draws_from)
  alone <- lapply(runs, function(run) {
    m$dag$define_tf_log_prob_function()
    draws_from(run)
  })
  expect_identical(shared, alone)
})

test_that("seeded draws do not depend on how the chain is split into calls", {
  skip_if_not(check_tf_version())
  x <- normal(0, 1)
  m <- model(x)
  draws_with <- function(...) {
    set.seed(2026 - 10 - 06)
    quietly(draws <- mcmc(m, warmup = 40, n_samples = 30, chains = 2, ...))
    as.matrix(draws)
  }

  one_call_per_phase <- draws_with(verbose = FALSE)
  expect_equal(draws_with(verbose = TRUE, pb_update = 7), one_call_per_phase)
  expect_equal(draws_with(one_by_one = TRUE), one_call_per_phase)
})

test_that("thin larger than n_samples is an informative error", {
  skip_if_not(check_tf_version())
  x <- uniform(0, 1)
  m <- model(x)
  expect_snapshot(
    error = TRUE,
    mcmc(m, n_samples = 10, warmup = 20, thin = 20, verbose = FALSE)
  )
})

test_that("each draw is thin iterations after the last, across bursts", {
  skip_if_not(check_tf_version())
  # rwmh accepts every proposal on a target this wide, and each of the n
  # parameters takes an independent step with sd epsilon / n, so the variance
  # across parameters of the move between two states, over one step's
  # variance, counts the iterations between them
  n <- 2000
  x <- normal(0, 1e6, dim = n)
  m <- model(x)
  step_var <- (0.1 / n)^2
  thin <- 3

  # without one_by_one, pb_update = 2 * thin cuts sampling into bursts of two
  # draws, and the one iteration left after the last draw runs on its own; with
  # it, every iteration is a burst of its own
  for (one_by_one in c(FALSE, TRUE)) {
    quietly(
      draws <- mcmc(
        m,
        sampler = rwmh(epsilon = 0.1, diag_sd = 1),
        warmup = 0,
        n_samples = 4 * thin + 1,
        thin = thin,
        pb_update = 2 * thin,
        one_by_one = one_by_one,
        chains = 1,
        initial_values = initials(x = rep(0, n)),
        verbose = TRUE
      )
    )
    final_state <- get_model_info(draws)$samplers[[1]]$free_state
    states <- rbind(0, as.matrix(draws), final_state)
    iterations <- apply(diff(states), 1, stats::var) / step_var
    expect_identical(round(iterations), c(thin, thin, thin, thin, 1))
  }
})
