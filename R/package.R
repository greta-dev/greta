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
#'   [greta website](https://greta-stats.org) for more information
#'   including
#'   [tutorials](https://greta-stats.org/articles/get_started.html) and
#'   [examples](https://greta-stats.org/articles/example_models.html).
#'
#' @importFrom tensorflow tf
#' @examples
#' \dontrun{
#' # a simple Bayesian regression model for the iris data
#'
#' # priors
#' int <- normal(0, 5)
#' coef <- normal(0, 3)
#' sd <- lognormal(0, 3)
#'
#' # likelihood
#' mean <- int + coef * iris$Petal.Length
#' distribution(iris$Sepal.Length) <- normal(mean, sd)
#'
#' # build and sample
#' m <- model(int, coef, sd)
#' draws <- mcmc(m, n_samples = 100)
#' }
"_PACKAGE"

# clear CRAN checks spotting floating global variables
#' @importFrom utils globalVariables
utils::globalVariables(
  c("N",
  "greta_deps_tf_tfp",
  "greta_logfile",
  "os",
  "python_version_max",
  "python_version_min",
  "tf_version",
  "tfp_version",
  "greta")
)
