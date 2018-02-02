# package file

#' greta: simple and scalable statistical modelling in R
#' @name greta
#'
#' @description greta lets you write statistical models interactively in native
#'   R code, then sample from them efficiently using Hamiltonian Monte Carlo.
#'
#'   The computational heavy lifting is done by TensorFlow, Google's automatic
#'   differentiation library. So greta is particularly fast where the model
#'   contains lots of linear algebra, and greta models can be run across CPU
#'   clusters or on GPUs.
#'
#'   See the simple example below, and take a look at the
#'   \href{https://greta-dev.github.io/greta}{greta website} for more information
#'   including
#'   \href{https://greta-dev.github.io/greta/get_started.html}{tutorials} and
#'   \href{https://greta-dev.github.io/greta/example_models.html}{examples}.
#'
#' @docType package
#' @import tensorflow
#' @import R6
#' @importFrom grDevices colorRampPalette
#' @examples
#' \dontrun{
#' # a simple Bayesian regression model for the iris data
#'
#' # priors
#' int = normal(0, 5)
#' coef = normal(0, 3)
#' sd = lognormal(0, 3)
#'
#' # likelihood
#' mean <- int + coef * iris$Petal.Length
#' distribution(iris$Sepal.Length) = normal(mean, sd)
#'
#' # build and sample
#' m <- model(int, coef, sd)
#' draws <- mcmc(m, n_samples = 100)
#' }
NULL

# crate the node list object whenever the package is loaded
.onLoad <- function (libname, pkgname) {

  # silence TF's CPU instructions message
  Sys.setenv(TF_CPP_MIN_LOG_LEVEL=2)

  # warn if TF version is bad
  check_tf_version('startup')

  # switch back to 0-based extraction in tensorflow
  options(tensorflow.one_based_extract = FALSE)

  # default float type
  if (reticulate::py_module_available('tensorflow'))
    options(greta_tf_float = tf$float32)

}


