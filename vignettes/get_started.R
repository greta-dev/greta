knitr::opts_chunk$set(echo = TRUE,
                      eval = greta:::check_tf_version("message"),
                      cache = TRUE,
                      comment = NA,
                      progress = FALSE)

set.seed(2018-07-02)

library(greta)

if (!file.exists('figures'))
  dir.create('figures')

file.copy('../man/figures/plotlegend.png',
          'figures/plotlegend.png')

## ----plot_hidden, echo = FALSE, results = "hide"-------------------------
gr <- plot(m)
fname <- "figures/full_graph.png"
DiagrammeR::export_graph(gr,
                         file_name = fname,
                         file_type = 'png',
                         width = 958 * 2,
                         height = 450 * 2)

## ----plot_coef, echo = FALSE, results = "hide"---------------------------
coef <- normal(0, 3)
m_coef <- model(coef)
gr <- plot(m_coef)
fname <- "figures/coef_graph.png"
DiagrammeR::export_graph(gr,
                         file_name = fname,
                         file_type = 'png',
                         width = 325 * 2,
                         height = 123 * 2)

## ----plot_likelihood, echo = FALSE, results = "hide"---------------------
sd <- variable()
y <- as_data(iris$Sepal.Length)
mean <- ones(150)
distribution(y) <- normal(mean, sd)
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

