#' @name internals
#' @aliases .internals
#' @title internal greta methods
#'
#' @description A list of functions and R6 class objects that can be used to
#'   develop extensions to greta. Most users will not need to access these
#'   methods, and it is not recommended to use them directly in model code.
#'
#' @section Usage: \preformatted{
#'  .internals$greta_arrays$unknowns        # greta array print methods
#'  .internals$inference$progress_bar       # progress bar tools
#'                       samplers           # MCMC samplers
#'                       stash              # stashing MCMC samples
#'  .internals$nodes$constructors           # node creation wrappers
#'                   distribution_classes   # R6 distribution classes
#'                   mixture_classes        # R6 mixture distribution classes
#'                   node_classes           # R6 node classes
#'  .internals$tensors                      # functions on tensors
#'  .internals$utils$checks                 # checking function inputs
#'                   colours                # greta colour scheme
#'                   dummy_arrays           # mocking up extract/replace
#'                   misc                   # code simplification etc.
#'                   samplers               # mcmc helpers
#' }
#'
#' @details
#'
#' This help file lists the available internals, but they are not fully
#' documented and are subject to change and deprecation without warning (though
#' care will be taken not to break dependent packages on CRAN). For an overview
#' of how greta works internally, see the \emph{technical details} vignette. See
#' \url{https://github.com/greta-dev} for examples of R packages extending and
#' building on greta.
#'
#' Please get in contact via GitHub if you want to develop an extension to
#' greta and need more details of how to use these internal functions.
#'
#' You can use \code{attach()} to put a sublist in the search path. E.g.
#' \code{attach(.internals$nodes$constructors)} will enable you to call
#' \code{op()}, \code{vble()} and \code{distrib()} directly.
NULL

#' @export
.internals <- module(greta_arrays = greta_array_module,
                     nodes = module(constructors = node_constructors_module,
                                    node_classes = node_classes_module,
                                    distribution_classes = distribution_classes_module,
                                    mixture_classes = mixture_module),
                     inference = inference_module,
                     tensors = tf_functions_module,
                     utils = utilities_module)
