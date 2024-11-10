## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  echo = FALSE,
  out.width = "100%"
)

## ----show-installation-process------------------------------------------------
knitr::include_graphics("../man/figures/greta-install-p1.png")

## ----show-installation-process-2----------------------------------------------
knitr::include_graphics("../man/figures/greta-install-p2.png")

## ----show-installation-process-3, out.width = "60%"---------------------------
knitr::include_graphics("../man/figures/greta-install-p3.png")

## ----run-greta, eval = FALSE--------------------------------------------------
#  normal(0,1)

## ----show-library-call--------------------------------------------------------
knitr::include_graphics("../man/figures/greta-load-library.png")

## ----show-greta-initialising--------------------------------------------------
knitr::include_graphics("../man/figures/greta-load-initialising.png")

## ----show-greta-initialised---------------------------------------------------
knitr::include_graphics("../man/figures/greta-load-initialised.png")

## ----show-greta-fail----------------------------------------------------------
knitr::include_graphics("../man/figures/greta-load-fail.png")

## ----install_tensorflow, eval = FALSE, echo = TRUE----------------------------
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

