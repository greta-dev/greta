## ----setup, include=FALSE------------------------------------------------

knitr::opts_chunk$set(echo = TRUE,
                      eval = TRUE,
                      cache = TRUE,
                      comment = NA,
                      progress = FALSE)

set.seed(123)

library(greta)

if (!file.exists('figures'))
  dir.create('figures')

file.copy('../man/figures/plotlegend.png',
          'figures/plotlegend.png')

## ----install_greta, eval = FALSE-----------------------------------------
#  install.packages("greta")

## ----install_greta_github, eval = FALSE----------------------------------
#  devtools::install_github("goldingn/greta")  # latest release
#  devtools::install_github("goldingn/greta@dev")  # development version

## ----load----------------------------------------------------------------
library(greta)

## ----install_tensorflow, eval = FALSE------------------------------------
#  install_tensorflow()

## ----first_model, eval = FALSE-------------------------------------------
#  library(greta)
#  
#  # data
#  x <- as_data(iris$Petal.Length)
#  y <- as_data(iris$Sepal.Length)
#  
#  # variables and priors
#  int = normal(0, 1)
#  coef = normal(0, 3)
#  sd = student(3, 0, 1, truncation = c(0, Inf))
#  
#  # operations
#  mean <- int + coef * x
#  
#  # likelihood
#  distribution(y) = normal(mean, sd)
#  
#  # defining the model
#  m <- model(int, coef, sd)
#  
#  # plotting
#  plot(m)
#  
#  # sampling
#  draws <- mcmc(m, n_samples = 1000)

## ----variables-----------------------------------------------------------
int = normal(0, 1)
coef = normal(0, 3)
sd = student(3, 0, 1, truncation = c(0, Inf))

## ----int_variable--------------------------------------------------------
(int = variable())

## ----positive_variable---------------------------------------------------
(sd = variable(lower = 0))

## ----matrix_variable-----------------------------------------------------
variable(lower = 0, dim = c(2, 3))

## ----truncated1----------------------------------------------------------
(z = normal(0, 1, truncation = c(-1, 1)))

## ----hidden_model, echo = FALSE------------------------------------------
x <- as_data(iris$Petal.Length)
y <- as_data(iris$Sepal.Length)
int = normal(0, 1)
coef = normal(0, 3)
sd = student(3, 0, 1, truncation = c(0, Inf))
mean <- int + coef * x
distribution(y) = normal(mean, sd)

## ----plot_hidden, echo = FALSE, results='hide'---------------------------
gr <- plot(m)
fname <- "figures/full_graph.png"
DiagrammeR::export_graph(gr,
                         file_name = fname,
                         file_type = 'png',
                         width = 958 * 2,
                         height = 450 * 2)

## ----plot_likelihood, echo = FALSE, results='hide'-----------------------
sd = variable()
y <- as_data(iris$Sepal.Length)
mean <- ones(150)
distribution(y) = normal(mean, sd)
m_likelihood <- model(sd)
gr <- plot(m_likelihood)
idx <- which(gr$nodes_df$label == 'mean\n')
gr$nodes_df$shape[idx] <- 'circle'
gr$nodes_df$fillcolor[idx] <- 'lightgray'
gr$nodes_df$color[idx] <- 'lightgray'
gr$nodes_df$width[idx] <- 0.2
gr$nodes_df$height[idx] <- 0.2
gr$nodes_df <- gr$nodes_df[c(3, 1, 2, 4), ]
fname <- "figures/likelihood_graph.png"
DiagrammeR::export_graph(gr,
                         file_name = fname,
                         file_type = "png",
                         width = 325 * 2,
                         height = 105 * 2)

## ----mcmcvis_public, eval = FALSE----------------------------------------
#  library (bayesplot)
#  mcmc_trace(draws)
#  mcmc_intervals(draws)

## ----mcmcvis, echo = FALSE, message = FALSE, out.width=c('400px', '400px'), fig.height=4, fig.width=5, fig.show='hold'----
library (bayesplot)
mcmc_trace(draws, facet_args = list(nrow = 3, ncol = 1))
mcmc_intervals(draws)

