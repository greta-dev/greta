devtools::load_all(".")

# Set up graphics device to capture plots
# Create directory for plots if it doesn't exist
# if (!dir.exists("plots")) dir.create("plots", recursive = TRUE)
#
# script_basename <- basename(tools::file_path_sans_ext(script_path))
# plot_filename <- paste0("plots/geweke_", script_basename, ".png")
# png(plot_filename, width = 1200, height = 800, res = 150)
#
# # Source the user's script
# tryCatch(
#   {
#     source(script_path)
#     cat("Successfully sourced script:", script_path, "\n")
#   },
#   error = function(e) {
#     cat("Error occurred while running the script:\n")
#     cat(as.character(e), "\n")
#     quit(status = 1)
#   }
# )
#
# # Close the device to save the plot
# dev.off()
# cat("Plot saved to", plot_filename, "\n")
#
# # Save test statistics to a text file
# stats_filename <- paste0("plots/geweke_stats_", script_basename, ".txt")
# if (exists("geweke_stat_adaptive_hmc")) {
#   capture.output(geweke_stat_adaptive_hmc, file = stats_filename)
#   cat("Geweke test statistics saved to", stats_filename, "\n")
# } else {
#   cat("Warning: No geweke_stat_adaptive_hmc object found\n")
# }
#
# # List all created files
# cat("Files in plots directory:\n")
# print(list.files("plots"))

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

geweke_adaptive_hmc <- check_geweke(
  sampler = adaptive_hmc(),
  model = model,
  data = x,
  p_theta = p_theta,
  p_x_bar_theta = p_x_bar_theta,
  chains = 10,
  niter = 10000,
  warmup = 10000,
  thin = 5
)

geweke_stat_adaptive_hmc <- geweke_ks(geweke_adaptive_hmc)

geweke_stat_adaptive_hmc

png("geweke-plot.png", width = 1200, height = 800, res = 150)
geweke_qq(geweke_adaptive_hmc, title = "adaptive hmc sampler Geweke test")
dev.off()
