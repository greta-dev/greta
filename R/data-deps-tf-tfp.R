#' Suggested valid Python dependencies for greta
#'
#' This is a dataset that contains suggested valid versions of Tensorflow (TF),
#'   Tensorflow Probability (TFP), and Python for linux, mac, and windows
#'   machines. It was constructed from
#'   <https://www.tensorflow.org/install/source> and
#'   <https://www.tensorflow.org/install/source_windows>, and by inspecting
#'   <https://github.com/tensorflow/probability/releases>.
#'
#' We recommend using the default versions provided in `greta_deps_spec()`.
#'
#' @format ## `greta_deps_tf_tfp`
#' A data frame with 63 rows and 5 columns:
#' \describe{
#'   \item{os}{Operating System}
#'   \item{tfp_version, tf_version}{numeric versions in format major.minor.patch for TFP and TF}
#'   \item{python_version_min, python_version_max}{numeric versions range in format major.minor.patch for Python}
#' }
# @source <>
"greta_deps_tf_tfp"
