# see `help(run_script, package = 'touchstone')` on how to run this
# interactively

# TODO OPTIONAL Add directories you want to be available in this file or during the
# benchmarks.
# touchstone::pin_assets("some/dir")

# installs branches to benchmark
touchstone::branch_install()

# benchmark a function call from your package (two calls per branch)
touchstone::benchmark_run(
  # expr_before_benchmark = source("dir/data.R"), #<-- TODO OTPIONAL setup before benchmark
  expr_before_benchmark = library(greta),
  create_normal = normal(0,1),
  n = 2
)

touchstone::benchmark_run(
  expr_before_benchmark = library(greta),
  create_model = model(normal(0,1)),
  n = 5
)

touchstone::benchmark_run(
  expr_before_benchmark = library(greta),
  run_mcmc = mcmc(model(normal(0,1))),
  n = 5
)

touchstone::benchmark_run(
  expr_before_benchmark = library(greta),
  basic_example = {
    x <- iris$Petal.Length
    y <- iris$Sepal.Length

    int <- normal(0, 5)
    coef <- normal(0, 3)
    sd <- lognormal(0, 3)

    mean <- int + coef * x
    distribution(y) <- normal(mean, sd)
    m <- model(int, coef, sd)
    draws <- mcmc(m, n_samples = 1000, chains = 4)
  },
  n = 2
)

# TODO OPTIONAL benchmark any R expression (six calls per branch)
# touchstone::benchmark_run(
#   more = {
#     if (TRUE) {
#       y <- yourpkg::f2(x = 3)
#     }
#   }, #<- TODO put the call you want to benchmark here
#   n = 6
# )


# create artifacts used downstream in the GitHub Action
touchstone::benchmark_analyze()
