# check tensorflow and tensorflow-probability are installed and have valid
# versions. error, warn, or message if not and (if not an error) return an
# invisible logical saying whether it is valid

#' @importFrom utils compareVersion
#' @importFrom reticulate py_available
#' @importFrom cli cli_process_start
#' @importFrom cli cli_process_done
#' @importFrom cli cli_process_failed
check_tf_version <- function(alert = c("none",
                                       "error",
                                       "warn",
                                       "message",
                                       "startup")) {

  # temporarily turn off the reticulate autoconfigure functionality
  ac_flag <- Sys.getenv("RETICULATE_AUTOCONFIGURE")
  on.exit(
    Sys.setenv(
      RETICULATE_AUTOCONFIGURE = ac_flag
    )
  )
  Sys.setenv(RETICULATE_AUTOCONFIGURE = FALSE)

  alert <- match.arg(alert)

  if (!greta_stash$python_has_been_initialised) {

    cli_process_start(
      msg = "Initialising python and checking dependencies, this may take a \\
      moment."
    )
  }

  requirements_valid <- c(
    python_exists = have_python(),
    correct_tf = have_tf(),
    correct_tfp = have_tfp()
  )

  if ((all(requirements_valid))) {

    if (!greta_stash$python_has_been_initialised) {

      cli_process_done(
        msg_done = "Initialising python and checking dependencies ... done!")
      cat("\n")
      greta_stash$python_has_been_initialised <- TRUE

    }

  }

  if (!all(requirements_valid)) {

    cli_process_failed()

    cli_msg <- c(
      "We have detected that you do not have the expected python packages \\
          setup.",
      "You can set these up by running this R code in the console:",
      "{.code install_greta_deps()}",
      "Then, restart R and run:",
      "{.code library(greta)}",
      "({.strong Note}: Your R session should not have initialised \\
          Tensorflow yet.)",
      "For more information, see {.code ?install_greta_deps}"
    )

    # if there was a problem, append the solution
    message_text <- cli::format_message(cli_msg)
    warning_text <- cli::format_warning(cli_msg)
    error_text <- cli::format_error(cli_msg)

    switch(
      alert,
      error = stop(error_text, call. = FALSE),
      warn = warning(warning_text, call. = FALSE),
      message = message(message_text),
      startup = packageStartupMessage(message_text),
      none = NULL
    )
  }

  invisible(all(requirements_valid))

}


# check dimensions of arguments to ops, and return the maximum dimension
check_dims <- function(..., target_dim = NULL) {

  # coerce args to greta arrays
  elem_list <- list(...)
  elem_list <- lapply(elem_list, as.greta_array)

  # dimensions of each
  dim_list <- lapply(elem_list, dim)

  # as text, for printing
  dims_paste <- vapply(dim_list, paste, "", collapse = "x")
  dims_text <- paste(dims_paste, collapse = ", ")

  # which are scalars
  scalars <- vapply(elem_list, is_scalar, FALSE)

  # if more than one is non-scalar, need to check them
  if (sum(!scalars) > 1) {
    match_first <- vapply(dim_list[!scalars],
                          identical,
                          FUN.VALUE = FALSE,
                          dim_list[!scalars][[1]]
    )

    # if they're non-scalar, but have the same dimensions, that's fine too
    if (!all(match_first)) {

      # otherwise it's not fine
      cli::cli_abort(
        "incompatible dimensions: {dims_text}"
      )

    }
  }

  # if there's a target dimension, make sure they all match it
  if (!is.null(target_dim)) {

    # make sure it's 2D
    if (length(target_dim) == 1) {
      target_dim <- c(target_dim, 1)
    }

    target_dim <- as.integer(target_dim)

    # if they are all scalars, that's fine too
    if (!all(scalars)) {

      # check all arguments against this
      matches_target <- vapply(dim_list[!scalars],
                               identical,
                               FUN.VALUE = FALSE,
                               target_dim
      )

      # error if not
      if (!all(matches_target)) {

        cli::cli_abort(
          c(
            "incorrect array dimensions",
            "x" = "array dimensions should be \\
              {paste(target_dim, collapse = 'x')},",
            "but input dimensions were {dims_text}."
          )
        )

      }

    }

    output_dim <- target_dim
  } else {

    # otherwise, find the correct output dimension
    dim_lengths <- lengths(dim_list)
    dim_list <- lapply(dim_list, pad_vector, to_length = max(dim_lengths))
    output_dim <- do.call(pmax, dim_list)
  }

  output_dim
}

# make sure a greta array is 2D
check_2d <- function(x) {
  if (length(dim(x)) != 2L) {
    cli::cli_abort(
      c(
        "Dimensions of parameters not compatible with multivariate \\
        distribution parameters of multivariate distributions cannot have \\
        more than two dimensions",
        "object {.var x} has dimensions: {paste(dim(x), collapse = 'x')}"
      )
    )
  }
}

check_square <- function(x) {
  dim <- dim(x)
  ndim <- length(dim)
  is_square <- ndim == 2 && dim[1] == dim[2]
  if (!is_square) {
    cli::cli_abort(
      c(
        "Not 2D square greta array",
        "x" = "expected a 2D square greta array, but object {.var x} had \\
        dimension: {paste(dim, collapse = 'x')}"
      )
    )
  }
}

check_sigma_square_2d_greta_array <- function(sigma){
  # check dimensions of Sigma
  if (nrow(sigma) != ncol(sigma) | length(dim(sigma)) != 2) {
    cli::cli_abort(
      c(
        "{.arg Sigma} must be a square 2D greta array",
        "However, {.arg Sigma} has dimensions \\
            {.val {paste(dim(sigma), collapse = 'x')}}"
      )
    )
  }
}

check_mean_sigma_have_same_dimensions <- function(mean, sigma) {
  dim_mean <- ncol(mean)
  dim_sigma <- nrow(sigma)

  if (dim_mean != dim_sigma) {
    cli::cli_abort(
      c(
        "{.arg mean} and {.arg Sigma} must have the same dimensions",
        "However they are different: {dim_mean} vs {dim_sigma}"
      )
    )
  }
}

check_chol2symm_square_symmetric_upper_tri_matrix <- function(x) {
  dim <- dim(x)
  if (length(dim) != 2 || dim[1] != dim[2]) {
    cli::cli_abort(
      c(
        "{.fun chol2symm} must have square symmetric matrix, assumed to be \\
        upper triangular",
        "{.code dim(x)} returns: {dim(x)}"
      )
    )
  }
}

check_chol2symm_2d_square_upper_tri_greta_array <- function(x) {
  dim <- dim(x)
  if (length(dim) != 2 || dim[1] != dim[2]) {
    cli::cli_abort(
      c(
        "{.fun chol2symm} must have two-dimensional, square, upper-triangular \\
        {.cls greta_array}s",
        "{.code dim(x)} returns: {dim(x)}"
      )
    )
  }
}

# given lists of greta arrays for the vector and scalar parameters (can be
# matrices and column vectors, respectively, where number of rows implies the
# number of realisations) and an optional target number of realisations, error
# if there's a mismatch, and otherwise return the output number of realisations
check_n_realisations <- function(vectors = list(),
                                 scalars = list(),
                                 target = NULL) {

  # get the number of rows in the vector and scalar objects
  nrows <- lapply(c(vectors, scalars), nrow)

  # which are single rows
  single_rows <- unlist(nrows) == 1

  # if more than one has multiple rows, need to check them
  if (sum(!single_rows) > 1) {
    match_first <- vapply(nrows[!single_rows],
                          identical,
                          FUN.VALUE = FALSE,
                          nrows[!single_rows][[1]]
    )

    # if they're non-scalar, but have the same dimensions, that's fine too
    if (!all(match_first)) {

      # otherwise it's not fine
      cli::cli_abort(
        c(
          "incompatible number of rows",
          x = "{paste(nrows, collapse = ' vs ')}"
        )
      )
    }
  }

  # if there's a target number of realisations, check it's valid and make sure
  # they all match it
  if (!is.null(target)) {

    # make sure it's a scalar
    if (length(target) != 1 || target < 1) {
      cli::cli_abort(
        c(
          "{.code n_realisations is not a positive scalar interger}",
          "{.code n_realisations} must be a positive scalar integer giving \\
            the number of rows of the output",
          "x" = "We see {.code n_realisations} = {.code {target}} \\
            having class: \\
            {.cls {class(target)}} and length \\
            {.var {length(target)}}"
        )
      )
    }

    target <- as.integer(target)

    # if they are all scalars, that's fine too
    if (!all(single_rows)) {

      # check all arguments against this
      matches_target <- vapply(nrows[!single_rows],
                               identical,
                               FUN.VALUE = FALSE,
                               target
      )

      # error if not
      if (!all(matches_target)) {
        cli::cli_abort(
          c(
            "Realisations do not match rows",
            "number of realisations should be {target},",
            "but arguments had {paste(nrows, collapse = ', ')} rows"
          )
        )
      }
    }

    n_realisations <- target
  } else {

    # otherwise, find the correct output dimension
    n_realisations <- max(unlist(nrows))
  }

  n_realisations
}


# check the dimension of maultivariate parameters matches, and matches the
# optional target dimension
check_dimension <- function(vectors = list(),
                            squares = list(),
                            target = NULL,
                            min_dimension = 2L) {

  # get the number of columns in the vector and scalar objects
  ncols <- lapply(c(vectors, squares), ncol)

  # if there's a target dimension, check then use that:
  if (!is.null(target)) {

    # make sure it's a scalar
    if (length(target) != 1 || target < 1 || !is.finite(target)) {
      cli::cli_abort(
        c(
          "{.var dimension} must be a positive scalar integer giving the \\
          dimension of the distribution",
          "{.code dim(target)} returns: {dim(target)}"
        )
      )
    }

    dimension <- as.integer(target)
  } else {

    # otherwise, get it from the first parameter
    dimension <- ncols[[1]]
  }

  # check it's big enough
  if (dimension < min_dimension) {
    cli::cli_abort(
      c(
        "the dimension of this distribution must be at least \\
        {min_dimension}, but was {dimension}",
        "multivariate distributions treat each {.emph row} as a separate \\
        realisation - perhaps you need to transpose something?"
      )
    )
  }

  # make sure all the parameters match this dimension
  match_dimension <- vapply(ncols, identical, dimension,
                            FUN.VALUE = FALSE
  )

  # otherwise it's not fine
  if (!all(match_dimension)) {
    cli::cli_abort(
      c(
        "distribution dimensions do not match implied dimensions",
        "The distribution dimension should be {dimension}, but parameters \\
        implied dimensions: {paste(ncols, collapse = ' vs ')}",
        "Multivariate distributions treat each {.emph row} as a separate \\
        realisation - perhaps you need to transpose something?"
      )
    )
  }

  dimension
}

# check dimensions of arguments to multivariate distributions
# if n_realisations isn't given, get it from the objects passed in
# if dimension isn't given, get it from the objects passed in
# if n_realisations *is* given, and the objects have one row, replicate them
# if n_realisations is given, and the objects have multiple rows, they must
# match.

# the objects passed in can either be vector-like (like 'mean'),
# scalar-like (like 'size'), or square (like 'Sigma').
check_multivariate_dims <- function(vectors = list(),
                                    scalars = list(),
                                    squares = list(),
                                    n_realisations = NULL,
                                    dimension = NULL,
                                    min_dimension = 2L) {

  # coerce args to greta arrays
  vectors <- lapply(vectors, as.greta_array)
  scalars <- lapply(scalars, as.greta_array)
  squares <- lapply(squares, as.greta_array)

  # make sure they are all 2D and the squares are square
  lapply(c(vectors, scalars, squares), check_2d)
  lapply(squares, check_square)

  # check and return the output number of distribution realisations
  n_realisations <- check_n_realisations(
    vectors,
    scalars,
    n_realisations
  )

  # check and return the distribution dimension
  dimension <- check_dimension(
    vectors,
    squares,
    dimension,
    min_dimension
  )

  # return the output greta array dimension
  c(n_realisations, dimension)
}


# check truncation for different distributions
check_positive <- function(truncation) {
  if (truncation[1] < 0) {
    cli::cli_abort(
      c(
        "lower bound must be 0 or higher",
        "lower bound is: {.val {truncation[1]}}"
      )
    )
  }
}

check_unit <- function(truncation) {
  if (truncation[1] < 0 | truncation[2] > 1) {
    cli::cli_abort(
      c(
        "lower and upper bounds must be between 0 and 1",
        "lower bound is: {.val {truncation[1]}}",
        "upper bound is: {.val {truncation[2]}}"
      )
    )
  }
}

# check whether the function calling this is being used as the 'family' argument
# of another modelling function
check_in_family <- function(function_name, arg) {
  if (missing(arg)) {
    # if the first argument is missing, the user might be doing
    # `family = binomial()` or similar
    arg_is_link <- TRUE
  } else {
    # if the first argument is one of these text strings, the user might be
    # doing `family = binomial("logit")` or similar
    links <- c(
      "logit", "probit", "cloglog", "cauchit",
      "log", "identity", "sqrt"
    )
    arg_is_link <- inherits(arg, "character") &&
      length(arg) == 1 && arg %in% links
  }

  # if it's being executed in an environment where it's named 'family', the user
  # might be doing `family = binomial` or similar
  greta_function <- get(function_name, envir = asNamespace("greta"))
  family <- parent.frame(2)$family
  function_is_family <- !is.null(family) && identical(family, greta_function)

  # nice user-friendly error message
  if (arg_is_link | function_is_family) {
    cli::cli_abort(
      c(
        "Wrong function name provided in another model",
        "It looks like you're using {.pkg greta}'s {.fun {function_name}} \\
        function in the family argument of another model.",
        "Maybe you want to use {.code family = stats::{function_name}},instead?"
      )
    )
  }
}


# get & return information about the future plan, and error nicely if invalid

#' @importFrom future plan future
check_future_plan <- function() {

  plan_info <- future::plan()

  plan_is <- list(
    parallel = !inherits(plan_info, "sequential"),
    cluster = inherits(plan_info, "cluster"),
    multisession = inherits(plan_info, "multisession"),
    local = TRUE
  )

  # if running in parallel
  if (plan_is$parallel) {

    # if it's a cluster, check there's no forking
    if (plan_is$cluster) {

      test_if_forked_cluster()

      f <- future::future(NULL, lazy = FALSE)

      workers <- f$workers

      if (inherits(workers, "cluster")) {
        worker <- workers[[1]]
        if (!is.null(worker$host)) {
          localhosts <- c("localhost", "127.0.0.1", Sys.info()[["nodename"]])
          plan_is$local <- worker$host %in% localhosts
        }
      }
    } else {

      # if multi*, check it's multisession
      if (!plan_is$multisession) {
        cli::cli_abort(
          "parallel mcmc samplers cannot be run with {.code plan(multicore)}"
        )
      }
    }
  }

  plan_is
}

# check a list of greta arrays and return a list with names scraped from call
check_greta_arrays <- function(greta_array_list, fun_name, hint = NULL) {

  # check they are greta arrays
  are_greta_arrays <- vapply(greta_array_list,
                             inherits, "greta_array",
                             FUN.VALUE = FALSE
  )


  msg <- NULL

  if (length(greta_array_list) == 0) {
    msg <- cli::format_error(
      c(
        "could not find any non-data {.cls greta_array}s"
      )
    )
  }

  if (!all(are_greta_arrays)) {
    unexpected_items <- names(greta_array_list)[!are_greta_arrays]

    msg <- cli::format_error(
      c(
        "{.fun {fun_name}} arguments must be {.cls greta_array}s",
        "The following {cli::qty(length(unexpected_items))} object{?s} passed \\
        to {.fun {fun_name}} {cli::qty(length(unexpected_items))} \\
        {?is not a/are not} {.cls greta array}{?s}:",
        "{.val {unexpected_items}}",
        "{hint}"
      )
    )
  }

  if (!is.null(msg)) {
    stop(
      msg,
      call. = FALSE
    )
  }

  greta_array_list
}

# check the provided list of greta array fixed values (as used in calculate and
# simulate) is valid
check_values_list <- function(values, env) {

  # get the values and their names
  names <- names(values)
  stopifnot(length(names) == length(values))

  # get the corresponding greta arrays
  fixed_greta_arrays <- lapply(names, get, envir = env)

  # make sure that's what they are
  are_greta_arrays <- vapply(fixed_greta_arrays,
                             inherits,
                             "greta_array",
                             FUN.VALUE = FALSE
  )

  if (!all(are_greta_arrays)) {
    cli::cli_abort(
      "the names of arguments to values must all correspond to named \\
      {.cls greta_array}s"
    )
  }

  # coerce value to have the correct dimensions
  assign_dim <- function(value, greta_array) {
    array <- unclass(get_node(greta_array)$value())
    if (length(array) != length(value)) {
      cli::cli_abort(
        "a provided value has different number of elements than the \\
        {.cls greta_array}"
      )
    }
    array[] <- value
    array
  }

  # make sure the values have the correct dimensions
  values <- mapply(assign_dim,
                   values,
                   fixed_greta_arrays,
                   SIMPLIFY = FALSE
  )

  list(
    fixed_greta_arrays = fixed_greta_arrays,
    values = values
  )
}

# check that all the variable greta arrays on which the target greta array
# depends are in the list fixed_greta_arrays (for use in calculate_list)
check_dependencies_satisfied <- function(target, fixed_greta_arrays, dag, env) {
  dependency_names <- function(x) {
    get_node(x)$parent_names(recursive = TRUE)
  }

  # find all the nodes depended on by this one
  dependencies <- dependency_names(target)

  # find all the nodes depended on by the new values, and remove them from the
  # list
  complete_dependencies <- unlist(
    lapply(
      fixed_greta_arrays,
      dependency_names
    )
  )

  unmet <- !dependencies %in% complete_dependencies
  unmet_dependencies <- dependencies[unmet]

  # find all of the remaining nodes that are variables
  unmet_nodes <- dag$node_list[unmet_dependencies]
  unmet_node_types <- vapply(unmet_nodes, node_type, FUN.VALUE = "")
  is_variable <- unmet_node_types == "variable"

  # if there are any undefined variables
  if (any(is_variable)) {

    # try to find the associated greta arrays to provide a more informative
    # error message
    greta_arrays <- all_greta_arrays(env, include_data = FALSE)

    greta_array_node_names <- vapply(greta_arrays,
                                     function(x) get_node(x)$unique_name,
                                     FUN.VALUE = ""
    )

    unmet_variables <- unmet_nodes[is_variable]

    matches <- names(unmet_variables) %in% greta_array_node_names

    unmet_names_idx <- greta_array_node_names %in% names(unmet_variables)
    unmet_names <- names(greta_array_node_names)[unmet_names_idx]

    # build the message
    if (any(matches)) {
      names_text <- paste(unmet_names, collapse = ", ")
      msg <- cli::format_error(
        c(
          "Please provide values for the following {length(names_text)} \\
          {.cls greta_array}{?s}:",
          "{.var {names_text}}"
        )
      )
    } else {
      msg <- cli::format_error(
        "The names of the missing {.cls greta_array}s could not be detected"
      )
    }

    final_msg <- cli::format_error(
      c(
        "greta array(s) do not have values",
        "values have not been provided for all {.cls greta_array}s on which the \\
        target depends, and {.var nsim} has not been set.",
        "{msg}"
      )
    )

    stop(
      msg,
      call. = FALSE
    )

  }
}

check_cum_op <- function(x) {
  dims <- dim(x)
  if (length(dims) > 2 | dims[2] != 1) {
    cli::cli_abort(
      c(
        "{.var x} must be a column vector",
        "but {.var x} has dimensions {paste(dims, collapse = 'x')}"
      )
    )
  }
}


#' @importFrom future availableCores
check_n_cores <- function(n_cores, samplers, plan_is) {

  # if the plan is remote, and the user hasn't specificed the number of cores,
  # leave it as all of them
  if (is.null(n_cores) & !plan_is$local) {
    return(NULL)
  }

  n_cores_detected <- future::availableCores()
  allowed_n_cores <- seq_len(n_cores_detected)

  # check user-provided cores
  if (!is.null(n_cores) && !n_cores %in% allowed_n_cores) {
    check_positive_integer(n_cores, "n_cores")

    cli::cli_warn(
      "{n_cores} cores were requested, but only {n_cores_detected} \\
      are available."
    )

    n_cores <- NULL
  }

  # if n_cores isn't user-specified, set it so there's no clash between samplers
  n_cores <- n_cores %||% floor(n_cores_detected / samplers)

  # make sure there's at least 1
  n_cores <- max(n_cores, 1)

  as.integer(n_cores)
}

check_positive_integer <- function(x, name = "") {
  suppressWarnings(x <- as.integer(x))

  if (length(x) != 1 | is.na(x) | x < 1) {
    cli::cli_abort(
      c(
        "{name} must be a positive integer",
        "However the value provided was: {.val {x}}"
      )
    )
  }

  x
}

# batch sizes must be positive numerics, rounded off to integers
check_trace_batch_size <- function(x) {
  valid <- is.numeric(x) && length(x) == 1 && x >= 1
  if (!valid) {
    cli::cli_abort(
      "{.var trace_batch_size} must be a single numeric value greater than or \\
      equal to 1"
    )
  }
  x
}

check_if_greta_array_in_mcmc <- function(x){
  if (!inherits(x, "greta_model") && inherits(x, "greta_array")) {
    cli::cli_abort(
      c( "MCMC requires input to be a {.cls greta_model} not a {.cls greta_array}",
        "x" = "{.var x} is a {.cls greta_array} not a {.cls greta_model}",
        "i" = "You can convert {.var x} into a {.cls greta_model} by running:",
        "{.code model(x)}"
      )
    )
  }
}

check_if_greta_model <- function(x) {
  if (!inherits(x, "greta_model")) {
    cli::cli_abort(
      c(
        "{.var x} must be a {.cls greta_model}",
        "But {.var x} is {.cls {class(x)}}"
      )
    )
  }
}




complex_error <- function(z) {
  cli::cli_abort(
    "{.pkg greta} does not yet support complex numbers"
  )
}

#' @export
Im.greta_array <- complex_error

#' @export
Re.greta_array <- complex_error

#' @export
Arg.greta_array <- complex_error

#' @export
Conj.greta_array <- complex_error

#' @export
Mod.greta_array <- complex_error

check_if_unsampleable_and_unfixed <- function(fixed_greta_arrays, dag) {
  # check there are no variables without distributions (or whose children have
  # distributions - for lkj & wishart) that aren't given fixed values
  variables <- dag$node_list[dag$node_types == "variable"]
  have_distributions <- vapply(
    variables,
    has_distribution,
    FUN.VALUE = logical(1)
  )
  any_child_has_distribution <- function(variable) {
    have_distributions <- vapply(
      variable$children,
      has_distribution,
      FUN.VALUE = logical(1)
    )
    any(have_distributions)
  }
  children_have_distributions <- vapply(
    variables,
    any_child_has_distribution,
    FUN.VALUE = logical(1)
  )

  unsampleable <- !have_distributions & !children_have_distributions

  fixed_nodes <- lapply(fixed_greta_arrays, get_node)
  fixed_node_names <- vapply(
    fixed_nodes,
    member,
    "unique_name",
    FUN.VALUE = character(1)
  )

  unfixed <- !(names(variables) %in% fixed_node_names)

  if (any(unsampleable & unfixed)) {
    cli::cli_abort(
      # NOTE:
      # is it possible to identify the names of these arrays or variables?
      "the target {.cls greta_array}s are related to variables that do not \\
        have distributions so cannot be sampled"
    )
  }
}

check_if_array_is_empty_list <- function(target){
  if (identical(target, list())) {
    cli::cli_abort(
      c(
        "{.fun calculate} requires {.cls greta array}s",
        "no {.cls greta array}s were provided to {.fun calculate}"
      )
    )
  }
}

check_if_lower_upper_numeric <- function(lower, upper) {
  if (!is.numeric(lower) | !is.numeric(upper)) {
    cli::cli_abort(
      c(
        "lower and upper must be numeric",
        "lower has class: {class(lower)}",
        "lower has length: {length(lower)}",
        "upper has class: {class(upper)}",
        "upper has length: {length(upper)}"
      )
    )
  }
}

check_if_lower_upper_has_bad_limits <- function(bad_limits) {
  if (bad_limits) {
    cli::cli_abort(
      "lower and upper must either be -Inf (lower only), Inf (upper only) \\
          or finite"
    )
  }
}

check_if_upper_gt_lower <- function(lower, upper) {
  if (any(lower >= upper)) {
    cli::cli_abort(
      c(
        "upper bounds must be greater than lower bounds",
        "lower is: {.val {lower}}",
        "upper is: {.val {upper}}"
      )
    )
  }
}

checks_module <- module(
  check_tf_version,
  check_dims,
  check_unit,
  check_positive,
  check_in_family,
  check_future_plan,
  check_greta_arrays,
  check_values_list,
  check_dependencies_satisfied,
  check_cum_op,
  check_future_plan,
  check_n_cores,
  check_positive_integer,
  check_if_array_is_empty_list,
  complex_error
)

