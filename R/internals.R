#' @name internals
#' @aliases .internals
#' @title internal greta methods
#'
#' @description a list of functions and R6 class objects that can be used to
#'   develop extensions to greta. Most users will not need to access these
#'   methods, and it is not recommended to use them directly in model code.
#'
#' @section Usage: \preformatted{
#'  # functions operating on TensorFlow objects
#'  .internals$tf_functions$tf_as_logical(x)
#'                          tf_as_float(x)
#'                          tf_as_integer(x)
#'                          tf_lchoose(n, k)
#'                          tf_lbeta(a, b)
#'                          tf_flat_to_chol(x, dims)
#'                          tf_flat_to_chol_correl(x, dims)
#'                          tf_chol_to_symmetric(U)
#'
#'  # utility functions
#'  .internals$utils$vble(truncation, dim = 1)
#'                   op(...)
#'                   distrib(distribution, ...)
#'
#'  # R6 node object generators
#'  .internals$node_class$node
#'                        distribution_node
#'                        data_node
#'                        variable_node
#'                        operation_node
#'                        distributions$uniform_distribution
#'                                      normal_distribution
#'                                      lognormal_distribution
#'                                      bernoulli_distribution
#'                                      binomial_distribution
#'                                      beta_binomial_distribution
#'                                      negative_binomial_distribution
#'                                      hypergeometric_distribution
#'                                      poisson_distribution
#'                                      gamma_distribution
#'                                      inverse_gamma_distribution
#'                                      weibull_distribution
#'                                      exponential_distribution
#'                                      pareto_distribution
#'                                      student_distribution
#'                                      laplace_distribution
#'                                      beta_distribution
#'                                      cauchy_distribution
#'                                      logistic_distribution
#'                                      chi_squared_distribution
#'                                      f_distribution
#'                                      multivariate_normal_distribution
#'                                      wishart_distribution
#'                                      lkj_correlation_distribution
#'                                      multinomial_distribution
#'                                      categorical_distribution
#'                                      dirichlet_distribution
#'                                      dirichlet_multinomial_distribution
#' }
#'
#' @details
#'
#' This help file lists the available internals, but they are not fully
#' documented and are subject to change and deprecation without warning (care
#' will be taken not to break dependent CRAN packages). For an overview of how
#' greta works internally, see the \emph{technical details} vignette. See
#' \url{https://github.com/goldingn} for examples of R packages implementing
#' greta modules and new samplers, and using greta as a backend. Please get in
#' contact via GitHub if the behaviour of any of these functions is unclear.
#'
#' The methods are broadly structured into three sections:
#'
#' \code{.internals$tf_functions} - functions for operating on TensorFlow's
#' tensor objects. They are used by node objects to perform calculations.
#'
#' \code{.internals$utils} - miscellaneous functions
#'
#' \code{.internals$node_class} - R6 generators for the node objects. Their main
#' method is \code{$new()}, which is used to create a new object of the class.
#' \code{node} is the base class from which distribution, data, variable and
#' operation nodes inherit. \code{distribution} contains generators for nodes
#' representing specific distributions, these inherit from
#' \code{distribution_node}. \code{.internals$utils$op()},
#' \code{.internals$utils$vble()}, and \code{.internals$utils$distrib()} are
#' utility functions to create operation, variable and distribution nodes.
#'
NULL

#' @export
.internals <- list(tf_functions = tf_functions,
                   utils = as_module(vble,
                                     op,
                                     distrib),
                   node_class = as_module(node,
                                          distribution_node,
                                          data_node,
                                          variable_node,
                                          operation_node,
                                          distributions = as_module(uniform_distribution,
                                                                    normal_distribution,
                                                                    lognormal_distribution,
                                                                    bernoulli_distribution,
                                                                    binomial_distribution,
                                                                    beta_binomial_distribution,
                                                                    negative_binomial_distribution,
                                                                    hypergeometric_distribution,
                                                                    poisson_distribution,
                                                                    gamma_distribution,
                                                                    inverse_gamma_distribution,
                                                                    weibull_distribution,
                                                                    exponential_distribution,
                                                                    pareto_distribution,
                                                                    student_distribution,
                                                                    laplace_distribution,
                                                                    beta_distribution,
                                                                    cauchy_distribution,
                                                                    chi_squared_distribution,
                                                                    logistic_distribution,
                                                                    f_distribution,
                                                                    multivariate_normal_distribution,
                                                                    wishart_distribution,
                                                                    lkj_correlation_distribution,
                                                                    multinomial_distribution,
                                                                    categorical_distribution,
                                                                    dirichlet_distribution,
                                                                    dirichlet_multinomial_distribution)))
