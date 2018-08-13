## ----setup, include = FALSE----------------------------------------------

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

