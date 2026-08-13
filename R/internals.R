# The export below has to be written out literally. `.internals` is built by
# `.onLoad()` (see R/zzz.R), so there is no top-level object for roxygen to
# find and `@export` silently generates nothing -- which is how the directive
# went missing the moment the package was re-documented after the `Collate`
# field was dropped. R itself is happy with it: loadNamespace() runs `.onLoad()`
# *before* sealing the namespace and processing exports, so `.internals` exists
# by the time the directive is resolved.
#
# greta.gp, greta.dynamics and greta.gam all read `.internals`, and none of them
# is reached by a revdep run, so losing this export surfaces as three broken
# builds rather than as a failing check here.

#' @name internals
#' @aliases .internals
#' @rawNamespace export(.internals)
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
#'  .internals$greta_stash                  # internal information storage
#' }
#'
#' @details
#'
#' This help file lists the available internals, but they are not fully
#' documented and are subject to change and deprecation without warning (though
#' care will be taken not to break dependent packages on CRAN). For an overview
#' of how greta works internally, see the *technical details* vignette. See
#' <https://github.com/greta-dev> for examples of R packages extending and
#' building on greta.
#'
#' Please get in contact via GitHub if you want to develop an extension to
#' greta and need more details of how to use these internal functions.
#'
#' You can use `attach()` to put a sublist in the search path. E.g.
#' `attach(.internals$nodes$constructors)` will enable you to call
#' `op()`, `vble()` and `distrib()` directly.
#' @return A nested list (`module`) of internal greta functions and R6
#'   generators.
NULL

nodes_module <- function() {
  module(
    constructors = node_constructors_module(),
    node_classes = node_classes_module(),
    distribution_classes = distribution_classes_module(),
    mixture_classes = mixture_module(),
    joint_classes = joint_module()
  )
}

# Each *_module() is a function rather than a value so that nothing is
# evaluated while files are sourced; see .onLoad(). `stash` is passed in rather
# than read from the namespace, so this does not depend on having been called
# after greta_stash was attached.
init_internals <- function(stash) {
  module(
    greta_arrays = greta_array_module(),
    nodes = nodes_module(),
    inference = inference_module(),
    tensors = tf_functions_module(),
    utils = utilities_module(),
    checks = checks_module(),
    greta_stash = stash
  )
}
