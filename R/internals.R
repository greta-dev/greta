#' @name internals
#' @title internal methods
#'
#' @description \code{.internals} contains a list of functions and R6 class
#'   objects that can be used to develop extensions to greta. Most greta users
#'   will not need to access these methods, and it is not recommended to use
#'   \code{.internals} in model code.
#' @export
.internals <- list(node_types = as_module(distribution_node,
                                          data_node,
                                          variable_node,
                                          operation_node),
                   distribution_nodes = as_module(uniform_distribution,
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
                                                  dirichlet_multinomial_distribution),
                   tf_functions = as_module(),
                   utils = as_module(vble,
                                     op))
