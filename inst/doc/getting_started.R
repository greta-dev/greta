## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,
                      eval = TRUE,
                      cache = TRUE,
                      comment = NA,
                      progress = FALSE)
set.seed(1)
library(greta)

## ----ones----------------------------------------------------------------
(z <- ones(3, 3))

## ----ones_op-------------------------------------------------------------
(z2 <- z + z ^ 2)

## ----free----------------------------------------------------------------
(a <- free(dim = c(3, 3)))
(a2 <- a + a ^ 2)

## ----first_model, eval = FALSE-------------------------------------------
#  library(greta)
#  
#  # data
#  x <- as_data(iris$Petal.Length)
#  y <- as_data(iris$Sepal.Length)
#  
#  # variables and priors
#  int = free()
#  coef = normal(0, 3)
#  sd = lognormal(0, 3)
#  
#  # operations
#  mean <- int + coef * x
#  
#  # likelihood
#  distribution(y) = normal(mean, sd)
#  
#  # defining the model
#  model <- define_model(int, coef, sd)
#  
#  # plotting
#  plot(model)
#  
#  # sampling
#  draws <- mcmc(model, n_samples = 1000)

## ----data----------------------------------------------------------------
x <- as_data(iris$Petal.Length)
y <- as_data(iris$Sepal.Length)

## ----print_greta_array---------------------------------------------------
as_data(iris[1:5, 1:4])

## ----logical_data--------------------------------------------------------
(is_setosa <- iris$Species[c(1, 41, 81, 121)] == 'setosa')
as_data(is_setosa)

## ----dim-----------------------------------------------------------------
dim(as_data(is_setosa))

## ----structures----------------------------------------------------------
ones(1, 3)
zeros(2, 2)

## ----greta_array---------------------------------------------------------
greta_array(pi, dim = c(2, 2))
greta_array(0:1, dim = c(3, 3))

## ----variables-----------------------------------------------------------
int = free()
coef = normal(0, 3)
sd = lognormal(0, 3)

## ----int-----------------------------------------------------------------
int

## ----positive_free-------------------------------------------------------
free(lower = 0, dim = c(2, 3))

## ----priors--------------------------------------------------------------
coef
sd

## ----normal_prior2, eval = FALSE-----------------------------------------
#  coef = free()
#  distribution(coef) = normal(0, 3)

## ----sd_distribution, eval = FALSE---------------------------------------
#  sd = free(lower = 0)
#  distribution(sd) = lognormal(0, 3)

## ----truncated-----------------------------------------------------------
z = free(lower = -1, upper = 2)
distribution(z) = normal(0, 1)
z

## ----linear_predictor----------------------------------------------------
mean <- int + coef * x

## ----mean----------------------------------------------------------------
dim(mean)
head(mean)

## ----extract-------------------------------------------------------------
mean[1:3]

## ----replace-------------------------------------------------------------
z <- zeros(4, 3)
(z[, 1] <- normal(0, 1, dim = 4))

## ----drop----------------------------------------------------------------
z <- matrix(1, nrow = 2, ncol = 2)
dim(z[, 1])
dim(z[, 1, drop = FALSE])

## ----drop_greta----------------------------------------------------------
z_greta <- as_data(z)
dim(z_greta[, 1])

## ----function1-----------------------------------------------------------
atanh <- function (z)
  (log(1 + z) - log(1 - z)) / 2

atanh(z_greta)

## ----likelihood----------------------------------------------------------
distribution(y) = normal(mean, sd)

## ----hidden_model, echo = FALSE------------------------------------------
x <- as_data(iris$Petal.Length)
y <- as_data(iris$Sepal.Length)
int = free()
coef = normal(0, 3)
sd = lognormal(0, 3)
mean <- int + coef * x
distribution(y) = normal(mean, sd)

## ----define--------------------------------------------------------------
model <- define_model(int, coef, sd)

## ----plot, eval = FALSE--------------------------------------------------
#  plot(model)

## ----plot_hidden, echo = FALSE, results='hide'---------------------------
gr <- plot(model)
fname <- tempfile(fileext = '.png')
DiagrammeR::export_graph(gr,
                         file_name = fname,
                         file_type = 'png',
                         width = 852,
                         height = 970)

## ----plot_show, echo = FALSE, out.width = 500----------------------------
knitr::include_graphics(fname)

## ----plot_coef, echo = FALSE, results='hide'-----------------------------
coef = normal(0, 3)
m_coef <- define_model(coef)
gr <- plot(m_coef)
fname <- tempfile(fileext = '.png')
DiagrammeR::export_graph(gr,
                         file_name = fname,
                         file_type = 'png',
                         width = 436,
                         height = 432)

## ----plot_coef_show, echo = FALSE, out.width = 256-----------------------
knitr::include_graphics(fname)

## ----plot_likelihood, echo = FALSE, results='hide'-----------------------
sd = free()
y <- as_data(iris$Sepal.Length)
mean <- ones(150)
distribution(y) = normal(mean, sd)
m_likelihood <- define_model(sd)
gr <- plot(m_likelihood)
idx <- which(gr$nodes_df$label == 'mean\n')
gr$nodes_df$shape[idx] <- 'circle'
gr$nodes_df$fillcolor[idx] <- 'lightgray'
gr$nodes_df$width[idx] <- 0.2
gr$nodes_df$height[idx] <- 0.2
fname <- tempfile(fileext = '.png')
DiagrammeR::export_graph(gr,
                         file_name = fname,
                         file_type = 'png',
                         width = 400,
                         height = 432)

## ----plot_likelihood_show, echo = FALSE, out.width = 235-----------------
knitr::include_graphics(fname)

## ----legend_show, echo = FALSE, out.width = 750--------------------------
knitr::include_graphics('../man/figures/plotlegend.png')

## ----install_diagrammer, eval = FALSE------------------------------------
#  install.packages('DiagrammeR')

## ----mcmc, message=FALSE, results='hide', progress = FALSE---------------
draws <- mcmc(model, n_samples = 1000)

## ----coda_summary--------------------------------------------------------
summary(draws)

## ----mcmcvis, out.width=c('400px', '400px'), fig.height=4, fig.width=5, fig.show='hold'----
library (MCMCvis)
MCMCtrace(draws)
MCMCplot(draws)

