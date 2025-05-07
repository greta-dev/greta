devtools::load_all(".")

# parameters ----
n <- 10
mu1 <- 0
sd1 <- 2
sd2 <- 1

# prior (n draws) ----
p_theta <- function(n) {
  rnorm(n, mu1, sd1)
}

# likelihood ----
p_x_bar_theta <- function(theta) {
  rnorm(n, theta, sd2)
}

# define the greta model ----
x <- as_data(rep(0, n))
greta_theta <- normal(mu1, sd1)
distribution(x) <- normal(greta_theta, sd2)
model <- model(greta_theta)

# mcmc parameters ----
n_iter <- 2000
n_warmup <- 2000
n_chains <- 2
n_thin <- 5
geweke_sampler <- rwmh()

#  checking ----
time_taken <- system.time({
  geweke_ahmc <- check_geweke(
    sampler = geweke_sampler,
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

# processing ----
geweke_thin <- apply_thinning(geweke_ahmc, n_thin)

geweke_stat <- geweke_ks(geweke_thin)

geweke_stat

the_time <- round(time_taken[3])

cat("This took", the_time, "seconds")

# ---- plotting ----
plot_metadata <- build_plot_metadata(
  sampler = geweke_sampler,
  time = the_time,
  n_warmup = n_warmup,
  n_iter = n_iter,
  n_chains = n_chains,
  n_thin = n_thin,
  ks_stat = geweke_stat
)

png(plot_metadata$qq_filename, width = 1200, height = 800, res = 150)

geweke_qq(geweke_thin, title = plot_metadata$qq_title)
dev.off()

png(plot_metadata$coda_filename, width = 1200, height = 800, res = 150)
plot(geweke_ahmc$draws)
dev.off()

bplot_dens <- bayesplot::mcmc_dens_chains(geweke_ahmc$draws)

ggplot2::ggsave(
  filename = plot_metadata$bayesplot_filename,
  plot = bplot_dens,
  width = 1200,
  height = 800,
  dpi = 150,
  units = "px",
  bg = "white"
)
