# package file

#' greta: Probabilistic Modelling with TensorFlow
#' @name greta
#'
#' @description greta lets you write probabilistic models interactively in
#'   native R code, then sample from them efficiently using Hamiltonian Monte
#'   Carlo.
#'
#'   The computational heavy lifting is done by TensorFlow, Google's automatic
#'   differentiation library. greta is particularly fast where the model
#'   contains lots of linear algebra, and greta models can be easily set up to
#'   run across CPUs or GPUs just by installing the relevant version of
#'   TensorFlow.
#'
#'   See the example below for the general set up of a greta model, and
#'   \link{greta-distributions}, \link{greta-operators}, \link{greta-functions},
#'   \link{greta-transforms} and \link{greta-structures} for details of the
#'   currently implemented syntax and how to combine them into models
#'
#' @docType package
#' @import tensorflow
#' @import R6
#' @importFrom grDevices colorRampPalette
#' @examples
#' \dontrun{
#' # define a simple model
#' mu = free()
#' sigma = lognormal(1, 0.1)
#' x = rnorm(10)
#' distribution(x) = normal(mu, sigma)
#'
#' m <- define_model(mu, sigma)
#'
#' # and sample from it
#' draws <- mcmc(m,
#'               n_samples = 100,
#'               warmup = 10)
#' }
NULL

# crate the node list object whenever the package is loaded
.onLoad <- function (libname, pkgname) {

  # silence TF's CPU instructions message
  Sys.setenv(TF_CPP_MIN_LOG_LEVEL=2)

  # warn if TF version is bad
  check_tf_version('warn')

  # default float type
  options(greta_tf_float = tf$float32)

}


