devtools::load_all(".")

n <- 10

mu1 <- 0
sd1 <- 2
sd2 <- 1

# prior (n draws)
p_theta <- function(n) {
  rnorm(n, mu1, sd1)
}

# likelihood
p_x_bar_theta <- function(theta) {
  rnorm(n, theta, sd2)
}

# define the greta model (single precision for slice sampler)
x <- as_data(rep(0, n))
greta_theta <- normal(mu1, sd1)
distribution(x) <- normal(greta_theta, sd2)
model <- model(greta_theta, precision = "single")
n_iter <- 10
n_warmup <- 10
n_chains <- 2
n_thin <- 1
time_taken <- system.time({
  geweke_slice <- check_geweke(
    sampler = slice(),
    model = model,
    data = x,
    p_theta = p_theta,
    p_x_bar_theta = p_x_bar_theta,
    chains = n_chains,
    niter = n_iter,
    warmup = n_warmup,
    thin = n_thin
  )
})

geweke_thin <- apply_thinning(geweke_slice, n_thin)

geweke_stat_slice <- geweke_ks(geweke_thin)

geweke_stat_slice

the_time <- round(time_taken[3])

cat("this took", the_time, "seconds")

png("geweke-plot-slice.png", width = 1200, height = 800, res = 150)

qq_title <- build_qq_title(
  "slice",
  n_warmup,
  n_iter,
  n_chains,
  n_thin,
  geweke_stat_rwmh,
  the_time
)
geweke_qq(geweke_thin, title = qq_title)
dev.off()

png("geweke-mcmc-plot-slice.png", width = 1200, height = 800, res = 150)
plot(geweke_slice$draws)
dev.off()

bplot_dens <- bayesplot::mcmc_dens_chains(geweke_slice$draws)

ggplot2::ggsave(
  filename = "geweke-gg-mcmc-plot-slice.png",
  plot = bplot_dens,
  width = 1200,
  height = 800,
  dpi = 150,
  units = "px",
  bg = "white"
)
