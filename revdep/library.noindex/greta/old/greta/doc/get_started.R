## ----setup, include = FALSE---------------------------------------------------
greta_run <- greta:::check_tf_version("message")
diagrammer_svg_installed <- requireNamespace("DiagrammeRsvg", quietly = TRUE)
rsvg_installed <- requireNamespace("rsvg", quietly = TRUE)
svg_things_installed <- diagrammer_svg_installed && rsvg_installed
run_vigette <- svg_things_installed && greta_run
knitr::opts_chunk$set(echo = TRUE,
                      eval = run_vigette,
                      cache = TRUE,
                      comment = NA,
                      progress = FALSE)

set.seed(2018-07-02)

library(greta)

if (!file.exists("figures"))
  dir.create("figures")

file.copy("../man/figures/plotlegend.png",
          "figures/plotlegend.png")


## ----install_greta, eval = FALSE----------------------------------------------
#  install.packages("greta")

## ----install_greta_github, eval = FALSE---------------------------------------
#  devtools::install_github("greta-dev/greta")

## ----load---------------------------------------------------------------------
#  library(greta)

## ----install_tensorflow, eval = FALSE-----------------------------------------
#  reticulate::install_miniconda()
#  reticulate::conda_create(
#          envname = "greta-env",
#          python_version = "3.7"
#        )
#  reticulate::conda_install(
#          envname = "greta-env",
#          packages = c(
#            "numpy==1.16.4",
#            "tensorflow-probability==0.7.0",
#            "tensorflow==1.14.0"
#          )
#        )

## ----install-deps-plain, eval = FALSE-----------------------------------------
#  reticulate::install_miniconda()
#  reticulate::conda_install(
#          packages = c(
#            "numpy==1.16.4",
#            "tensorflow-probability==0.7.0",
#            "tensorflow==1.14.0"
#          )
#        )

## ----install_diagrammer, eval = FALSE-----------------------------------------
#  install.packages("igraph")
#  install.packages("DiagrammeR")

## ----ones---------------------------------------------------------------------
#  (z <- ones(3, 3))

## ----ones_op------------------------------------------------------------------
#  (z2 <- z + z ^ 2)

## ----variable-----------------------------------------------------------------
#  (a <- variable(dim = c(3, 3)))
#  (a2 <- a + a ^ 2)

## ----first_model, eval = FALSE------------------------------------------------
#  library(greta)
#  
#  # data
#  x <- as_data(iris$Petal.Length)
#  y <- as_data(iris$Sepal.Length)
#  
#  # variables and priors
#  int <- normal(0, 1)
#  coef <- normal(0, 3)
#  sd <- student(3, 0, 1, truncation = c(0, Inf))
#  
#  # operations
#  mean <- int + coef * x
#  
#  # likelihood
#  distribution(y) <- normal(mean, sd)
#  
#  # defining the model
#  m <- model(int, coef, sd)
#  
#  # plotting
#  plot(m)
#  
#  # sampling
#  draws <- mcmc(m, n_samples = 1000)

## ----data---------------------------------------------------------------------
#  x <- as_data(iris$Petal.Length)
#  y <- as_data(iris$Sepal.Length)

## ----print_greta_array--------------------------------------------------------
#  as_data(iris[1:5, 1:4])

## ----logical_data-------------------------------------------------------------
#  (is_setosa <- iris$Species[c(1, 41, 81, 121)] == "setosa")
#  as_data(is_setosa)

## ----dim----------------------------------------------------------------------
#  dim(as_data(is_setosa))

## ----structures---------------------------------------------------------------
#  ones(1, 3)
#  zeros(2, 2)

## ----greta_array--------------------------------------------------------------
#  greta_array(pi, dim = c(2, 2))
#  greta_array(0:1, dim = c(3, 3))

## ----variables----------------------------------------------------------------
#  int <- normal(0, 1)
#  coef <- normal(0, 3)
#  sd <- student(3, 0, 1, truncation = c(0, Inf))

## ----int_variable-------------------------------------------------------------
#  (int <- variable())

## ----positive_variable--------------------------------------------------------
#  (sd <- variable(lower = 0))

## ----matrix_variable----------------------------------------------------------
#  variable(lower = 0, dim = c(2, 3))

## ----truncated1---------------------------------------------------------------
#  (z <- normal(0, 1, truncation = c(-1, 1)))

## ----linear_predictor---------------------------------------------------------
#  mean <- int + coef * x

## ----mean---------------------------------------------------------------------
#  dim(mean)
#  head(mean)

## ----extract------------------------------------------------------------------
#  mean[1:3]

## ----replace------------------------------------------------------------------
#  z <- zeros(4, 3)
#  z[, 1] <- normal(0, 1, dim = 4)
#  z

## ----drop---------------------------------------------------------------------
#  z <- matrix(1, nrow = 2, ncol = 2)
#  dim(z[, 1])
#  dim(z[, 1, drop = FALSE])

## ----drop_greta---------------------------------------------------------------
#  z_greta <- as_data(z)
#  dim(z_greta[, 1])

## ----function1----------------------------------------------------------------
#  atanh <- function (z)
#    (log(1 + z) - log(1 - z)) / 2
#  
#  atanh(z_greta)

## ----likelihood---------------------------------------------------------------
#  distribution(y) <- normal(mean, sd)

## ----hidden_model, echo = FALSE-----------------------------------------------
#  x <- as_data(iris$Petal.Length)
#  y <- as_data(iris$Sepal.Length)
#  int <- normal(0, 1)
#  coef <- normal(0, 3)
#  sd <- student(3, 0, 1, truncation = c(0, Inf))
#  mean <- int + coef * x
#  distribution(y) <- normal(mean, sd)

## ----define_model, eval=FALSE-------------------------------------------------
#  m <- model(int, coef, sd)

## ----plot,eval=FALSE----------------------------------------------------------
#  plot(m)

## ----plot-hidden, echo = FALSE, eval=FALSE------------------------------------
#  gr <- plot(m)
#  DiagrammeR::export_graph(attr(gr, "dgr_graph"),
#                           file_name = "figures/full_graph.png",
#                           file_type = "png",
#                           width = 958 * 2,
#                           height = 450 * 2)

## ----plot_coef, echo = FALSE, eval=FALSE--------------------------------------
#  coef <- normal(0, 3)
#  m_coef <- model(coef)
#  gr <- plot(m_coef)
#  DiagrammeR::export_graph(attr(gr, "dgr_graph"),
#                           file_name = "figures/coef_graph.png",
#                           file_type = "png",
#                           width = 325 * 2,
#                           height = 123 * 2)

## ----plot_likelihood, echo = FALSE, eval=FALSE--------------------------------
#  sd <- variable()
#  y <- as_data(iris$Sepal.Length)
#  mean <- ones(150)
#  distribution(y) <- normal(mean, sd)
#  m_likelihood <- model(sd)
#  gr <- plot(m_likelihood)
#  dgr <- attr(gr, "dgr_graph")
#  # make the mean look like an operation node
#  idx <- which(dgr$nodes_df$label == 'mean\n')
#  dgr$nodes_df$shape[idx] <- 'circle'
#  dgr$nodes_df$fillcolor[idx] <- 'lightgray'
#  dgr$nodes_df$color[idx] <- 'lightgray'
#  dgr$nodes_df$width[idx] <- 0.2
#  dgr$nodes_df$height[idx] <- 0.2
#  dgr$nodes_df <- dgr$nodes_df[c(3, 1, 2, 4), ]
#  DiagrammeR::export_graph(dgr,
#                           file_name = "figures/likelihood_graph.png",
#                           file_type = "png",
#                           width = 325 * 2,
#                           height = 105 * 2)
#  

## ----mcmc, message = FALSE, results = "hide", progress = FALSE, eval=FALSE----
#  draws <- mcmc(m, n_samples = 1000)

## ----coda_summary, eval=FALSE-------------------------------------------------
#  summary(draws)

## ----mcmcvis, echo = TRUE, message = FALSE, out.width = c('400px', '400px'), fig.height=4, fig.width=5, fig.show='hold', eval=FALSE----
#  library (bayesplot)
#  # set theme to avoid issues with fonts
#  ggplot2::theme_set(ggplot2::theme_bw())
#  mcmc_trace(draws, facet_args = list(nrow = 3, ncol = 1))
#  mcmc_intervals(draws)

