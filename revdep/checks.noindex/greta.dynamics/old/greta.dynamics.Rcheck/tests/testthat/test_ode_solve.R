context("ODE solver")

test_that("ode_solve works like deSolve::ode", {
  skip_if_not(greta:::check_tf_version())
  source("helpers.R")

  # deSolve version of the Lotka Volterra model
  LVmod <- function(Time, State, Pars) {
    with(as.list(c(State, Pars)), {
      Ingestion <- rIng * Prey * Predator
      GrowthPrey <- rGrow * Prey * (1 - Prey / K)
      MortPredator <- rMort * Predator

      dPrey <- GrowthPrey - Ingestion
      dPredator <- Ingestion * assEff - MortPredator

      return(list(c(dPrey, dPredator)))
    })
  }

  # greta version of the model
  lotka_volterra <- function(y, t, rIng, rGrow, rMort, assEff, K) {
    Prey <- y[1, 1]
    Predator <- y[1, 2]

    Ingestion <- rIng * Prey * Predator
    GrowthPrey <- rGrow * Prey * (1 - Prey / K)
    MortPredator <- rMort * Predator

    dPrey <- GrowthPrey - Ingestion
    dPredator <- Ingestion * assEff - MortPredator

    cbind(dPrey, dPredator)
  }

  pars <- c(
    rIng = 0.2, # /day, rate of ingestion
    rGrow = 1.0, # /day, growth rate of prey
    rMort = 0.2, # /day, mortality rate of predator
    assEff = 0.5, # -, assimilation efficiency
    K = 10
  ) # mmol/m3, carrying capacity

  yini <- c(Prey = 1, Predator = 2)
  times <- seq(0, 200, by = 1)

  # loop through the solvers (ode45 should be similar to the dopri5 method in TF)
  methods <- c("ode45", "rk4", "midpoint")

  for (method in methods) {
    deSolve_method <- method

    if (deSolve_method == "midpoint") {
      deSolve_method <- rk_midpoint
    }

    r_out <- deSolve::ode(yini, times, LVmod, pars,
      method = deSolve_method
    )

    y <- ode_solve(lotka_volterra,
      y0 = t(yini),
      times,
      rIng = pars["rIng"],
      rGrow = pars["rGrow"],
      rMort = pars["rMort"],
      assEff = pars["assEff"],
      K = pars["K"],
      method = method
    )
    g_out <- cbind(times, y)

    greta_out <- calculate(g_out)[[1]]
    difference <- abs(greta_out - r_out)
    expect_true(all(difference < 1e-4))
  }
})

test_that("inference works with ode_solve", {
  skip_if_not(greta:::check_tf_version())
  source("helpers.R")

  lotka_volterra <- function(y, t, rIng, rGrow, rMort, assEff, K) {
    Prey <- y[1, 1]
    Predator <- y[1, 2]

    Ingestion <- rIng * Prey * Predator
    GrowthPrey <- rGrow * Prey * (1 - Prey / K)
    MortPredator <- rMort * Predator

    dPrey <- GrowthPrey - Ingestion
    dPredator <- Ingestion * assEff - MortPredator

    cbind(dPrey, dPredator)
  }

  rIng <- uniform(0, 2) # /day, rate of ingestion
  rGrow <- uniform(0, 3) # /day, growth rate of prey
  rMort <- uniform(0, 1) # /day, mortality rate of predator
  assEff <- uniform(0, 1) # -, assimilation efficiency
  K <- uniform(0, 30) # mmol/m3, carrying capacity

  yini <- c(Prey = 1, Predator = 2)
  times <- seq(0, 200, by = 1)

  y <- ode_solve(lotka_volterra,
    y0 = t(yini),
    times,
    rIng = rIng,
    rGrow = rGrow,
    rMort = rMort,
    assEff = assEff,
    K = K,
    method = "rk4"
  )

  # simulate some data and fit to it
  y_true <- calculate(y, nsim = 1)[[1]][1, , ]
  y_obs <- y_true + rnorm(prod(dim(y_true)), 0, 0.1)
  distribution(y_obs) <- normal(y_true, 0.1)

  m <- model(rIng)

  # should be fine in opt() and mcmc()
  o <- opt(m)
  expect_true(is.list(o))
  expect_identical(names(o), c("par", "value", "iterations", "convergence"))

  draws <- mcmc(m, chains = 2, warmup = 100, n_samples = 100, verbose = FALSE)
  expect_s3_class(draws, "greta_mcmc_list")
})
