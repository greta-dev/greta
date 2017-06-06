## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,
                      eval = TRUE,
                      cache = TRUE,
                      comment = NA,
                      progress = FALSE)
set.seed(123)
library(greta)

## ----define_model--------------------------------------------------------
m <- model(int, coef, sd)

## ----plot, eval = FALSE--------------------------------------------------
#  plot(m)

## ----plot_hidden, echo = FALSE, results='hide'---------------------------
gr <- plot(m)
fname <- tempfile(fileext = '.png')
DiagrammeR::export_graph(gr,
                         file_name = fname,
                         file_type = 'png',
                         width = 895 * 2,
                         height = 313 * 2)

## ----plot_show, echo = FALSE, out.width = 800----------------------------
knitr::include_graphics(fname)

## ----legend_show, echo = FALSE, out.width = 750--------------------------
knitr::include_graphics('../man/figures/plotlegend.png')

## ----plot_coef, echo = FALSE, results='hide'-----------------------------
coef = normal(0, 3)
m_coef <- model(coef)
gr <- plot(m_coef)
fname <- tempfile(fileext = '.png')
DiagrammeR::export_graph(gr,
                         file_name = fname,
                         file_type = 'png',
                         width = 325 * 2,
                         height = 123 * 2)

## ----plot_coef_show, echo = FALSE, out.width = 500-----------------------
knitr::include_graphics(fname)

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
gr$nodes_df$width[idx] <- 0.2
gr$nodes_df$height[idx] <- 0.2
gr$nodes_df <- gr$nodes_df[c(3, 1, 2, 4), ]

fname <- tempfile(fileext = '.png')
DiagrammeR::export_graph(gr,
                         file_name = fname,
                         file_type = 'png',
                         width = 325 * 2,
                         height = 105 * 2)

## ----plot_likelihood_show, echo = FALSE, out.width = 500-----------------
knitr::include_graphics(fname)

## ----install_diagrammer, eval = FALSE------------------------------------
#  install.packages('DiagrammeR')

## ----mcmc, message=FALSE, results='hide', progress = FALSE---------------
draws <- mcmc(m, n_samples = 1000)

## ----coda_summary--------------------------------------------------------
summary(draws)

## ----mcmcvis, out.width=c('400px', '400px'), fig.height=4, fig.width=5, fig.show='hold'----
library (MCMCvis)
MCMCtrace(draws)
MCMCplot(draws, xlim = c(-1, 5))

