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
      "x" = "The expected python packages are not available",
      "i" = "We recommend installing them (in a fresh R session) with:",
      "{.code install_greta_deps()}",
      "or",
      "{.code reinstall_greta_deps()}",
      "({.strong Note}: Your R session should not have initialised \\
          Tensorflow yet.)",
      "i" = "For more information, see {.code ?install_greta_deps}"
    )

    # if there was a problem, append the solution
    message_text <- cli::format_message(cli_msg)
    warning_text <- cli::format_warning(cli_msg)
    error_text <- cli::format_error(cli_msg)

    switch(
      alert,
      error = rlang::abort(error_text),
      warn = rlang::warn(warning_text),
      message = rlang::inform(message_text),
      startup = packageStartupMessage(message_text),
      none = NULL
    )
  }

  invisible(all(requirements_valid))

}


# check dimensions of arguments to ops, and return the maximum dimension
check_dims <- function(...,
                       target_dim = NULL,
                       call = rlang::caller_env()) {

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
  more_than_one_is_non_scalar <- sum(!scalars) > 1
  if (more_than_one_is_non_scalar) {
    match_first <- are_identical(dim_list[!scalars], dim_list[!scalars][[1]])

    # if they're non-scalar, but have the same dimensions, that's fine too
    if (!all(match_first)) {

      # otherwise it's not fine
      cli::cli_abort(
        message = "incompatible dimensions: {dims_text}",
        call = call
      )

    }
  }

  # if there's a target dimension, make sure they all match it
  if (!is.null(target_dim)) {

    # make sure it's 2D
    is_1d <- length(target_dim) == 1
    if (is_1d) {
      target_dim <- c(target_dim, 1)
    }

    target_dim <- as.integer(target_dim)

    # if they are all scalars, that's fine too
    if (!all(scalars)) {

      # check all arguments against this
      matches_target <- are_identical(dim_list[!scalars], target_dim)

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
check_2d_multivariate <- function(x,
                                  call = rlang::caller_env()) {
  if (!is_2d(x)) {
    cli::cli_abort(
      message = c(
        "Dimensions of parameters not compatible with multivariate \\
        distribution parameters of multivariate distributions cannot have \\
        more than two dimensions",
        "object {.var x} has dimensions: {paste(dim(x), collapse = 'x')}"
      ),
      call = call
    )
  }
}

check_square <- function(x = NULL,
                         dim = NULL,
                         call = rlang::caller_env()) {

  # allows for specifying x or named dim = dim
  dim <- dim %||% dim(x)
  ndim <- length(dim)
  not_square <- dim[1] != dim[2]
  if (ndim == 2 && not_square) {
    cli::cli_abort(
      message = c(
        "Object must be 2D square array",
        "x" = "But it had dimension: \\
        {.val {paste(dim, collapse = 'x')}}"
      ),
      call = call
    )
  }
}

check_sigma_square_2d_greta_array <- function(sigma,
                                              call = rlang::caller_env()){
  # check dimensions of Sigma
  not_square <- nrow(sigma) != ncol(sigma)
  not_2d <- n_dim(sigma) != 2
  not_square_or_2d <- not_square | not_2d
  if (not_square_or_2d) {
    cli::cli_abort(
      message = c(
        "{.arg Sigma} must be a square 2D greta array",
        "However, {.arg Sigma} has dimensions \\
            {.val {paste(dim(sigma), collapse = 'x')}}"
      ),
      call = call
    )
  }
}

check_mean_sigma_have_same_dimensions <- function(mean,
                                                  sigma,
                                                  call = rlang::caller_env()) {
  dim_mean <- ncol(mean)
  dim_sigma <- nrow(sigma)

  if (dim_mean != dim_sigma) {
    cli::cli_abort(
      message = c(
        "{.arg mean} and {.arg Sigma} must have the same dimensions",
        "However they are different: {dim_mean} vs {dim_sigma}"
      ),
      call = call
    )
  }
}

check_chol2symm_square_symmetric_upper_tri_matrix <- function(
    x,
    call = rlang::caller_env()
) {
  dim <- dim(x)
  is_square <- dim[1] == dim[2]
  if (!is_2d(x) || !is_square) {
    cli::cli_abort(
      message = c(
        "{.fun chol2symm} must have square symmetric matrix, assumed to be \\
        upper triangular",
        "{.code dim(x)} returns: {dim(x)}"
      ),
      call = call
    )
  }
}

check_chol2symm_2d_square_upper_tri_greta_array <- function(
    x,
    call = rlang::caller_env()
) {
  dim <- dim(x)
  is_square <- dim[1] == dim[2]
  if (!is_2d(x) || !is_square) {
    cli::cli_abort(
      message = c(
        "{.fun chol2symm} must have two-dimensional, square, upper-triangular \\
        {.cls greta_array}s",
        "{.code dim(x)} returns: {dim(x)}"
      ),
      call = call
    )
  }
}

# given lists of greta arrays for the vector and scalar parameters (can be
# matrices and column vectors, respectively, where number of rows implies the
# number of realisations) and an optional target number of realisations, error
# if there's a mismatch, and otherwise return the output number of realisations
check_n_realisations <- function(vectors = list(),
                                 scalars = list(),
                                 target = NULL,
                                 call = rlang::caller_env()) {

  # get the number of rows in the vector and scalar objects
  nrows <- lapply(c(vectors, scalars), nrow)

  # which are single rows
  single_rows <- unlist(nrows) == 1

  # if more than one has multiple rows, need to check them
  if (sum(!single_rows) > 1) {
    match_first <- are_identical(nrows[!single_rows], nrows[!single_rows][[1]])

    # if they're non-scalar, but have the same dimensions, that's fine too
    if (!all(match_first)) {

      # otherwise it's not fine
      cli::cli_abort(
        message = c(
          "incompatible number of rows",
          x = "{paste(nrows, collapse = ' vs ')}"
        ),
        call = call
      )
    }
  }

  # if there's a target number of realisations, check it's valid and make sure
  # they all match it
  if (!is.null(target)) {

    # make sure it's a scalar
    not_scalar <- length(target) != 1 || target < 1
    if (not_scalar) {
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
      matches_target <- are_identical(nrows[!single_rows], target)

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
                            min_dimension = 2L,
                            call = rlang::caller_env()) {

  # get the number of columns in the vector and scalar objects
  ncols <- lapply(c(vectors, squares), ncol)

  # if there's a target dimension, check then use that:
  if (!is.null(target)) {

    # make sure it's a scalar
    positive_scalar <- length(target) != 1 || target < 1 || !is.finite(target)
    if (positive_scalar) {
      cli::cli_abort(
        message = c(
          "{.var dimension} must be a positive scalar integer giving the \\
          dimension of the distribution",
          "{.code dim(target)} returns: {dim(target)}"
        ),
        call = call
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
  match_dimension <- are_identical(ncols, dimension)

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
  lapply(c(vectors, scalars, squares), check_2d_multivariate)
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
check_positive <- function(truncation,
                           call = rlang::caller_env()) {
  bound_is_negative <- truncation[1] < 0
  if (bound_is_negative) {
    cli::cli_abort(
      message = c(
        "lower bound must be 0 or higher",
        "lower bound is: {.val {truncation[1]}}"
      ),
      call = call
    )
  }
}

check_unit <- function(truncation,
                       call = rlang::caller_env()) {
  bounds_not_btn_0_1 <- truncation[1] < 0 | truncation[2] > 1
  if (bounds_not_btn_0_1) {
    cli::cli_abort(
      message = c(
        "lower and upper bounds must be between 0 and 1",
        "lower bound is: {.val {truncation[1]}}",
        "upper bound is: {.val {truncation[2]}}"
      ),
      call = call
    )
  }
}

# check whether the function calling this is being used as the 'family' argument
# of another modelling function
check_in_family <- function(function_name,
                            arg,
                            call = rlang::caller_env()) {
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
      message = c(
        "Wrong function name provided in another model",
        "It looks like you're using {.pkg greta}'s {.fun {function_name}} \\
        function in the family argument of another model.",
        "Maybe you want to use {.code family = stats::{function_name}},instead?"
      ),
      call = call
    )
  }
}


# get & return information about the future plan, and error nicely if invalid

#' @importFrom future plan future
check_future_plan <- function(call = rlang::caller_env()) {

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
          message = "parallel mcmc samplers cannot be run with \\
          {.code plan(multicore)}",
          call = call
        )
      }
    }
  }

  plan_is
}

# check a list of greta arrays and return a list with names scraped from call
check_greta_arrays <- function(greta_array_list,
                               fun_name,
                               hint = NULL,
                               call = rlang::caller_env()) {

  # check they are greta arrays
  are_greta_arrays <- are_greta_array(greta_array_list)

  msg <- NULL

  if (length(greta_array_list) == 0) {
    msg <- cli::format_error(
      message = c(
        "could not find any non-data {.cls greta_array}s"
      )
    )
  }

  if (!all(are_greta_arrays)) {
    unexpected_items <- names(greta_array_list)[!are_greta_arrays]

    msg <- cli::format_error(
      message = c(
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
    rlang::abort(
      msg,
      call = call
    )
  }

  greta_array_list
}

# check the provided list of greta array fixed values (as used in calculate and
# simulate) is valid
check_values_list <- function(values,
                              env,
                              call = rlang::caller_env()) {

  # get the values and their names
  names <- names(values)
  stopifnot(length(names) == length(values))

  # get the corresponding greta arrays
  fixed_greta_arrays <- lapply(names, get, envir = env)

  # make sure that's what they are
  are_greta_arrays <- are_greta_array(fixed_greta_arrays)

  if (!all(are_greta_arrays)) {
    cli::cli_abort(
      message = "the names of arguments to values must all correspond to \\
      named {.cls greta_array}s",
      call = call
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
check_dependencies_satisfied <- function(target,
                                         fixed_greta_arrays,
                                         dag,
                                         env,
                                         call = rlang::caller_env()) {
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
        message = c(
          "Please provide values for the following {length(names_text)} \\
          {.cls greta_array}{?s}:",
          "{.var {names_text}}"
        )
      )
    } else {
      msg <- cli::format_error(
        message = "The names of the missing {.cls greta_array}s could not \\
        be detected"
      )
    }

    final_msg <- cli::format_error(
      message = c(
        "greta array(s) do not have values",
        "values have not been provided for all {.cls greta_array}s on which \\
        the target depends, and {.var nsim} has not been set.",
        "{msg}"
      )
    )

    rlang::abort(
      message = msg,
      call = call
    )

  }
}

check_cum_op <- function(x,
                         call = rlang::caller_env()) {
  dims <- dim(x)
  x_not_column_vector <- length(dims) > 2 | dims[2] != 1
  if (x_not_column_vector) {
    cli::cli_abort(
      message = c(
        "{.var x} must be a column vector",
        "but {.var x} has dimensions {paste(dims, collapse = 'x')}"
      ),
      call = call
    )
  }
}


#' @importFrom future availableCores
check_n_cores <- function(n_cores,
                          samplers,
                          plan_is) {

  # if the plan is remote, and the user hasn't specificed the number of cores,
  # leave it as all of them
  if (is.null(n_cores) & !plan_is$local) {
    return(NULL)
  }

  n_cores_detected <- future::availableCores()
  allowed_n_cores <- seq_len(n_cores_detected)

  cores_exceed_available <- !is.null(n_cores) && !n_cores %in% allowed_n_cores
  if (cores_exceed_available) {
    check_positive_integer(n_cores, "n_cores")

    cli::cli_warn(
      message = "{n_cores} cores were requested, but only {n_cores_detected} \\
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

check_positive_integer <- function(x,
                                   name = "",
                                   call = rlang::caller_env()) {
  suppressWarnings(x <- as.integer(x))

  not_positive_integer <- length(x) != 1 | is.na(x) | x < 1
  if (not_positive_integer) {
    cli::cli_abort(
      message = c(
        "{name} must be a positive integer",
        "However the value provided was: {.val {x}}"
      ),
      call = call
    )
  }

  x
}

# batch sizes must be positive numerics, rounded off to integers
check_trace_batch_size <- function(x,
                                   call = rlang::caller_env()) {
  valid <- is.numeric(x) && length(x) == 1 && x >= 1
  if (!valid) {
    cli::cli_abort(
      message = "{.var trace_batch_size} must be a single numeric value \\
      greater than or equal to 1",
      call = call
    )
  }
  x
}

check_if_greta_array_in_mcmc <- function(x,
                                         call = rlang::caller_env()){
  if (!is.greta_model(x) && is.greta_array(x)) {
    cli::cli_abort(
      message = c(
        "MCMC requires input to be a {.cls greta_model} not a {.cls greta_array}",
        "x" = "{.var x} is a {.cls greta_array} not a {.cls greta_model}",
        "i" = "You can convert {.var x} into a {.cls greta_model} by running:",
        "{.code model(x)}"
      ),
      call = call
    )
  }
}

check_if_greta_model <- function(x,
                                 call = rlang::caller_env()) {
  if (!is.greta_model(x)) {
    cli::cli_abort(
      message = c(
        "{.var x} must be a {.cls greta_model}",
        "But {.var x} is {.cls {class(x)}}"
      ),
      call = call
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

check_if_unsampleable_and_unfixed <- function(fixed_greta_arrays,
                                              dag,
                                              call = rlang::caller_env()) {
  # check there are no variables without distributions (or whose children have
  # distributions - for lkj & wishart) that aren't given fixed values
  variables <- dag$node_list[dag$node_types == "variable"]
  have_distributions <- have_distribution(variables)
  any_child_has_distribution <- function(variable) {
    have_distributions <- have_distribution(variable$children)
    any(have_distributions)
  }
  children_have_distributions <- vapply(
    variables,
    any_child_has_distribution,
    FUN.VALUE = logical(1)
  )

  unsampleable <- !have_distributions & !children_have_distributions

  fixed_nodes <- lapply(fixed_greta_arrays, get_node)
  fixed_node_names <- extract_unique_names(fixed_nodes)

  unfixed <- !(names(variables) %in% fixed_node_names)

  if (any(unsampleable & unfixed)) {
    cli::cli_abort(
      # NOTE:
      # is it possible to identify the names of these arrays or variables?
      message = "the target {.cls greta_array}s are related to variables \\
      that do not have distributions so cannot be sampled",
      call = call
    )
  }
}

check_if_array_is_empty_list <- function(target, call = rlang::caller_env()){
  no_greta_arrays_provided <- identical(target, list())
  if (no_greta_arrays_provided) {
    cli::cli_abort(
      message = c(
        "{.fun calculate} requires {.cls greta array}s",
        "no {.cls greta array}s were provided to {.fun calculate}"
      ),
      call = call
    )
  }
}

check_if_lower_upper_numeric <- function(lower,
                                         upper,
                                         call = rlang::caller_env()) {
  if (!is.numeric(lower) | !is.numeric(upper)) {
    cli::cli_abort(
      message = c(
        "lower and upper must be numeric",
        "lower has class: {class(lower)}",
        "lower has length: {length(lower)}",
        "upper has class: {class(upper)}",
        "upper has length: {length(upper)}"
      ),
      call = call
    )
  }
}

check_if_lower_upper_has_bad_limits <- function(bad_limits,
                                                call = rlang::caller_env()) {
  if (bad_limits) {
    cli::cli_abort(
      message = "lower and upper must either be -Inf (lower only), \\
      Inf (upper only) or finite",
      call = call
    )
  }
}

check_if_upper_gt_lower <- function(lower,
                                    upper,
                                    call = rlang::caller_env()) {
  if (any(lower >= upper)) {
    cli::cli_abort(
      message = c(
        "upper bounds must be greater than lower bounds",
        "lower is: {.val {lower}}",
        "upper is: {.val {upper}}"
      ),
      call = call
    )
  }
}


check_targets_stochastic_and_not_sampled <- function(
    target,
    mcmc_dag_variables,
    call = rlang::caller_env()
) {
  target_nodes <- lapply(target, get_node)
  target_node_names <- extract_unique_names(target_nodes)
  existing_variables <- target_node_names %in% names(mcmc_dag_variables)
  have_distributions <- have_distribution(target_nodes)
  new_stochastics <- have_distributions & !existing_variables
  if (any(new_stochastics)) {
    n_stoch <- sum(new_stochastics)
    cli::cli_abort(
      message = c(
        "{.arg nsim} must be set to sample {.cls greta array}s not in MCMC \\
          samples",
        "the greta {cli::qty(n_stoch)} arra{?ys/y} \\
          {.var {names(target)[new_stochastics]}} {cli::qty(n_stoch)} \\
          {?have distributions and are/has a distribution and is} not in the \\
          MCMC samples, so cannot be calculated from the samples alone.",
        "Set {.arg nsim} if you want to sample them conditionally on the \\
          MCMC samples"
      ),
      call = call
    )
  }
}

# see if the new dag introduces any new variables
check_dag_introduces_new_variables <- function(dag,
                                               mcmc_dag,
                                               call = rlang::caller_env()) {
  new_types <- dag$node_types[!connected_to_draws(dag, mcmc_dag)]
  any_new_variables <- any(new_types == "variable")
  if (any_new_variables) {
    cli::cli_abort(
      message = c(
        "{.arg nsim} must be set to sample {.cls greta array}s not in MCMC \\
          samples",
        "the target {.cls greta array}s are related to new variables that \\
          are not in the MCMC samples, so cannot be calculated from the \\
          samples alone.",
        "Set {.arg nsim} if you want to sample them conditionally on the \\
          MCMC samples"
      ),
      call = call
    )
  }
}

check_commanality_btn_dags <- function(dag,
                                       mcmc_dag,
                                       call = rlang::caller_env()) {
  target_not_connected_to_mcmc <- !any(connected_to_draws(dag, mcmc_dag))
  if (target_not_connected_to_mcmc) {
    cli::cli_abort(
      message = "the target {.cls greta array}s do not appear to be \\
      connected to those in the {.cls greta_mcmc_list} object",
      call = call
    )
  }
}

check_finite_positive_scalar_integer <- function(x,
                                                 arg = rlang::caller_arg(x),
                                                 call = rlang::caller_env()){
  is_not_finite_positive_scalar_integer <- !is.numeric(x) ||
    length(x) != 1 ||
    !is.finite(x) ||
    x <= 0

  if (is_not_finite_positive_scalar_integer) {

    cli::cli_abort(
      message = c(
        "{.arg {arg}} must be a finite, positive, scalar integer",
        "x" = "We see {.arg {arg}} = {.arg {x}}",
        "x" = "having class: {.cls {class(x)}}",
        "x" = "and length {.var {length(x)}}"
      ),
      call = call
    )
  }

}

check_if_greta_mcmc_list <- function(x,
                                     arg = rlang::caller_arg(x),
                                     call = rlang::caller_env()){
  if (!is.greta_mcmc_list(x)) {
    cli::cli_abort(
      message = c(
        "{.arg {arg}} must be a {.cls greta_mcmc_list} object",
        "created by {.fun greta::mcmc}, {.fun greta::stashed_samples}, or \\
        {.fun greta::extra_samples}"
      ),
      call = call
    )
  }
}

check_if_model_info <- function(x,
                                arg = rlang::caller_arg(x),
                                call = rlang::caller_env()){
  valid <- !is.null(x)

  if (!valid) {
    cli::cli_abort(
      message = c(
        "{.arg {arg}} is an {.cls mcmc.list} object, but is not associated \\
        with any model information",
        "Perhaps it wasn't created by {.fun greta::mcmc}, \\
        {.fun greta::stashed_samples}, or {.fun greta::extra_samples}?"
      ),
      call = call
    )
  }

}

check_2d <- function(x,
                     arg = rlang::caller_arg(x),
                     call = rlang::caller_env()){
  if (!is_2d(x)){
    cli::cli_abort(
      message = c(
        "{.arg {arg} must be two dimensional}",
        "However, {.arg {arg}} has dimensions: {paste(dim(x), collapse = 'x')}"
      ),
      call = call
    )
  }
}

check_positive_scalar <- function(x,
                                  arg = rlang::caller_arg(x),
                                  call = rlang::caller_env()){

  not_positive_scalar <- !is.numeric(x) || !length(x) == 1 || x <= 0
  if (not_positive_scalar){
    cli::cli_abort(
      message = c(
        "{.arg {arg}} must be a positive scalar value, or a scalar \\
            {.cls greta_array}"
      ),
      call = call
    )
  }
}

check_scalar <- function(x,
                         arg = rlang::caller_arg(x),
                         call = rlang::caller_env()){
  scalar <- is_scalar(x)
  if (!scalar){
    cli::cli_abort(
      message = c(
        "{.arg {arg}} must be a scalar",
        "However {.arg {arg}} had dimensions: \\
            {paste0(dim({x}), collapse = 'x')}"
      ),
      call = call
    )
  }
}

check_finite <- function(x,
                         arg = rlang::caller_arg(x),
                         call = rlang::caller_env()){
  not_finite <- !is.finite(x)
  if (not_finite){
    cli::cli_abort(
      message =   c(
        "{.arg {x}} must be a finite scalar",
        "But their values are:",
        "{.arg {x}}: {x}"
      ),
      call = call
    )
  }
}

check_x_gte_y <- function(x,
                          y,
                          x_arg = rlang::caller_arg(x),
                          y_arg = rlang::caller_arg(y),
                          call = rlang::caller_env()){

  x_gte_y <- x >= y

  if (x_gte_y) {
    cli::cli_abort(
      message = c(
        "{.arg {y_arg}} must be greater than {.arg {x_arg}}",
        "Their values are:",
        "{.arg {x_arg}}: {x}",
        "{.arg {y_arg}}: {y}"
      ),
      call = call
    )
  }

}


check_numeric_length_1 <- function(x,
                                   arg = rlang::caller_arg(x),
                                   call = rlang::caller_env()){
  good_type <- is.numeric(x) && length(x) == 1

  if (!good_type) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be a numeric vector of length 1",
        "However its class, and length are:",
        "{.arg {arg}}:",
        "*" = "(class: {.cls {class({x})}})",
        "*" = "(length: {.val {length({x})}})"
      ),
      call = call
    )
  }

}

check_both_2d <- function(x,
                          y,
                          x_arg = rlang::caller_arg(x),
                          y_arg = rlang::caller_arg(y),
                          call = rlang::caller_env()){
  if (!is_2d(x) | !is_2d(y)) {
    cli::cli_abort(
      message = c(
        "Only two-dimensional {.cls greta_array}s can be matrix-multiplied",
        "Dimensions for each are:",
        "{.arg {x_arg}}: {.val {pretty_dim(x)}}",
        "{.arg {y_arg}}: {.val {pretty_dim(y)}}"
      ),
      call = call
    )
  }
}

check_compatible_dimensions <- function(x,
                                        y,
                                        call = rlang::caller_env()){

  incompatible_dimensions <- dim(x)[2] != dim(y)[1]
  if (incompatible_dimensions) {
    cli::cli_abort(
      message = c(
        "Incompatible dimensions: \\
        {.val {paste0(dim(x), collapse = 'x')}} vs \\
        {.val {paste0(dim(y), collapse = 'x')}}"
      ),
      call = call
    )
  }
}

check_distribution_support <- function(x,
                                       arg = rlang::caller_arg(x),
                                       call = rlang::caller_env()){
  n_supports <- length(unique(x))
  if (n_supports != 1) {
    supports_text <- vapply(
      X = unique(x),
      FUN = paste,
      collapse = " to ",
      FUN.VALUE = character(1)
    )

    cli::cli_abort(
      message = c(
        "Component distributions must have the same support",
        "However the component distributions have different support:",
        "{.val {paste(supports_text, collapse = ' vs. ')}}"
      ),
      call = call
    )
  }

}

check_not_multivariate_univariate <- function(x,
                                              arg = rlang::caller_arg(x),
                                              call = rlang::caller_env()){
  is_multivariate_and_univariate <- !all(x) & !all(!x)
  if (is_multivariate_and_univariate) {
    cli::cli_abort(
      message = c(
        "Cannot construct a mixture from a combination of multivariate and \\
      univariate distributions"
      ),
      call = call
    )
  }
}

check_not_discrete_continuous <- function(x,
                                          name,
                                          arg = rlang::caller_arg(x),
                                          call = rlang::caller_env()){

  is_discrete_and_continuous <- !all(x) & !all(!x)
  if (is_discrete_and_continuous) {
    cli::cli_abort(
      message = c(
        "Cannot construct a {name} distribution from a combination of discrete and \\
    continuous distributions"
      ),
      call = call
    )
  }
}

check_num_distributions <- function(n_distributions,
                                    at_least,
                                    name,
                                    call = rlang::caller_env()){
  if (n_distributions < at_least) {
    cli::cli_abort(
      message = c(
        "{.fun {name}} must be passed at least {.val {at_least}} \\
        distributions",
        "The number of distributions passed was: {.val {n_distributions}}"
      ),
      call = call
    )
  }

}

check_weights_dim <- function(weights_dim,
                              dim,
                              n_distributions,
                              arg = rlang::caller_arg(weights_dim),
                              call = rlang::caller_env()){

  # weights should have n_distributions as the first dimension
  if (weights_dim[1] != n_distributions) {
    cli::cli_abort(
      message = c(
        "The first dimension of weights must be the number of \\
            distributions in the mixture ({.val {n_distributions}})",
        "However it was {.val {weights_dim[1]}}"
      ),
      call = call
    )
  }

  weights_extra_dim <- dim
  n_extra_dim <- length(weights_extra_dim)
  weights_last_dim_is_1 <- weights_extra_dim[n_extra_dim] == 1
  if (weights_last_dim_is_1) {
    weights_extra_dim <- weights_extra_dim[-n_extra_dim]
  }

  # remainder should be 1 or match weights_extra_dim
  w_dim <- weights_dim[-1]
  dim_1 <- length(w_dim) == 1 && w_dim == 1
  dim_same <- all(w_dim == weights_extra_dim)
  incompatible_dims <- !(dim_1 | dim_same)
  if (incompatible_dims) {
    cli::cli_abort(
      message = c(
        "The dimension of weights must be either \\
            {.val {n_distributions}x1} or \\
            {.val {n_distributions}x{pretty_dim(dim)}}",
        " but was {.val {pretty_dim(weights_dim)}}"
      ),
      call = call
    )
  }

}

check_initials_are_named <- function(values,
                                     call = rlang::caller_env()){
  names <- names(values)
  initials_not_all_named <- length(names) != length(values)
  if (initials_not_all_named) {
    cli::cli_abort(
      message = "All initial values must be named",
      call = call
    )
  }
}

check_initials_are_numeric <- function(values,
                                       call = rlang::caller_env()){
  are_numeric <- vapply(values, is.numeric, FUN.VALUE = FALSE)
  if (!all(are_numeric)) {
    cli::cli_abort(
      message = "initial values must be numeric",
      call = call
    )
  }
}

check_initial_values_match_chains <- function(initial_values,
                                              n_chains,
                                              call = rlang::caller_env()){

  initials <- initial_values
  not_initials_but_list <- !is.initials(initials) && is.list(initials)
  if (not_initials_but_list) {
    # if the user provided a list of initial values, check elements and length
    all_initials <- all(are_initials(initials))

    n_sets <- length(initials)

    initial_values_do_not_match_chains <- n_sets != n_chains
    if (initial_values_do_not_match_chains && all_initials) {
      cli::cli_abort(
        message = c(
          "The number of provided initial values does not match chains",
          "{n_sets} set{?s} of initial values were provided, but there \\
            {cli::qty(n_chains)} {?is only/are} {n_chains} \\
            {cli::qty(n_chains)} chain{?s}"
        ),
        call = call
      )
    }
  }

}

check_initial_values_correct_dim <- function(target_dims,
                                             replacement_dims,
                                             call = rlang::caller_env()){

  same_dims <- mapply(identical, target_dims, replacement_dims)

  if (!all(same_dims)) {
    cli::cli_abort(
      message = "The initial values provided have different dimensions than \\
      the named {.cls greta_array}s",
      call = call
    )
  }

}

check_initial_values_correct_class <- function(initial_values,
                                               call = rlang::caller_env()){

  initials <- initial_values
  not_initials_but_list <- !is.initials(initials) && is.list(initials)
  not_initials_not_list <- !is.initials(initials) && !is.list(initials)
  # if the user provided a list of initial values, check elements and the
  # length
  all_initials <- all(are_initials(initials))
  not_all_initials <- !all_initials

  if (not_initials_but_list && not_all_initials || not_initials_not_list) {
    cli::cli_abort(
      message = c(
        "{.arg initial_values} must be an initials object created with \\
        {.fun initials}, or a simple list of initials objects"
      ),
      call = call
    )
  }

}

check_nodes_all_variable <- function(nodes,
                                     call = rlang::caller_env()){
  types <- lapply(nodes, node_type)
  are_variables <- are_identical(types, "variable")

  if (!all(are_variables)) {
    cli::cli_abort(
      "Initial values can only be set for variable {.cls greta_array}s"
    )
  }

}

check_greta_arrays_associated_with_model <- function(tf_names,
                                                     call = rlang::caller_env()){
  missing_names <- is.na(tf_names)
  if (any(missing_names)) {
    bad <- names(tf_names)[missing_names]
    cli::cli_abort(
      c(
        "Some {.cls greta_array}s passed to {.fun initials} are not \\
        associated with the model:",
        "{.var {bad}}"
      )
    )
  }
}

check_not_data_greta_arrays <- function(model,
                                        call = rlang::caller_env()){

  # find variable names to label samples
  target_greta_arrays <- model$target_greta_arrays
  names <- names(target_greta_arrays)

  # check they're not data nodes, provide a useful error message if they are
  are_data <- vapply(
    target_greta_arrays,
    function(x) is.data_node(get_node(x)),
    FUN.VALUE = FALSE
  )

  if (any(are_data)) {
    cli::cli_abort(
      message = c(
        "Data {.cls greta_array}s cannot be sampled",
        "{.var {names[are_data]}} \\
        {?is a data/are data} {.cls greta_array}(s)"
      ),
      call = call
    )
  }
}

check_diagrammer_installed <- function(call = rlang::caller_env()){
  if (!is_DiagrammeR_installed()) {
    cli::cli_abort(
      message = c(
        "The {.pkg DiagrammeR} package must be installed to plot \\
        {.pkg greta} models",
        "Install {.pkg DiagrammeR} with:",
        "{.code install.packages('DiagrammeR')}"
      ),
      call = call
    )
  }
}

check_unfixed_discrete_distributions <- function(dag,
                                                 call = rlang::caller_env()){

  # check for unfixed discrete distributions
  distributions <- dag$node_list[dag$node_types == "distribution"]
  bad_nodes <- vapply(
    X = distributions,
    FUN = function(x) {
      valid_target <- is.null(x$target) || is.data_node(x$target)
      x$discrete && !valid_target
    },
    FUN.VALUE = FALSE
  )

  if (any(bad_nodes)) {
    cli::cli_abort(
      "Model contains a discrete random variable that doesn't have a fixed \\
      value, so inference cannot be carried out."
    )
  }
}

check_greta_array_type <- function(x,
                                   optional,
                                   call = rlang::caller_env()){

  if (!is.numeric(x) && !is.logical(x) && !optional){
    cli::cli_abort(
      message = c(
        "{.cls greta_array} must contain the same type",
        "Cannot coerce {.cls matrix} to a {.cls greta_array} unless it is \\
          {.cls numeric}, {.cls integer} or {.cls logical}.",
        "This {.cls matrix} had type: {.cls {class(as.vector(x))}}"
      ),
      call = call
    )
  }
}

check_greta_data_frame <- function(x,
                                   optional,
                                   arg = rlang::caller_arg(x),
                                   call = rlang::caller_env()){
  classes <- vapply(x, class, "")
  valid <- classes %in% c("numeric", "integer", "logical")

  array_has_different_types <- !optional & !all(valid)
  if (array_has_different_types) {
    invalid_types <- unique(classes[!valid])
    cli::cli_abort(
      message = c(
        "{.cls greta_array} must contain the same type",
        "Cannot coerce a {.cls data.frame} to a {.cls greta_array} unless \\
        all columns are {.cls numeric, integer} or {.cls logical}.",
        "This dataframe had columns of type: {.cls {invalid_types}}"
      ),
      call = call
    )
  }
}

check_ncols_match <- function(x1,
                              x2,
                              call = rlang::caller_env()){
  if (ncol(x1) != ncol(x2)) {
    cli::cli_abort(
      message = c(
        "{.var x1} and {.var x2} must have the same number of columns",
        "However {.code ncol(x1)} = {ncol(x1)} and \\
          {.code ncol(x2)} = {ncol(x2)}"
      ),
      call = call
    )
  }
}

check_fields_installed <- function(){
  fields_installed <- requireNamespace("fields", quietly = TRUE)
  if (!fields_installed) {
    cli::cli_abort(
      c(
        "{.pkg fields} package must be installed to use {.fun rdist} on greta \\
        arrays",
        "Install {.pkg fields} with:",
        "{.code install.packages('fields')}"
      )
    )
  }
}

check_2_by_1 <- function(x,
                         call = rlang::caller_env()){
  dim_x <- dim(x)
  is_2_by_1 <- is_2d(x) && dim_x[2] == 1L
  if (!is_2_by_1) {
    cli::cli_abort(
      message = c(
        "{.var x} must be 2D {.cls greta_array} with one column",
        "However {.var x} has dimensions {paste(dim_x, collapse = 'x')}"
      ),
      call = call
    )
  }
}


check_transpose <- function(x,
                            call = rlang::caller_env()){
  if (x) {
    cli::cli_abort(
      message = "{.arg transpose} must be FALSE for {.cls greta_array}s",
      call = call
    )
  }
}

check_x_matches_ncol <- function(x,
                                 ncol_of,
                                 x_arg = rlang::caller_arg(x),
                                 ncol_of_arg = rlang::caller_arg(ncol_of),
                                 call = rlang::caller_env()){

  if (x != ncol(ncol_of)) {
    cli::cli_abort(
      message = "{.arg {x}} must equal {.code ncol({ncol_of_arg})} for \\
      {.cls greta_array}s",
      call = call
    )
  }
}

check_stats_dim_matches_x_dim <- function(x,
                                          margin,
                                          stats,
                                          call = rlang::caller_env()){
  stats_dim_matches_x_dim <- dim(x)[margin] == dim(stats)[1]
  if (!stats_dim_matches_x_dim) {
    cli::cli_abort(
      message = c(
        "The number of elements of {.var stats} does not match \\
        {.code dim(x)[MARGIN]}"
      ),
      call = call
    )
  }
}

# STATS must be a column array
check_is_column_array <- function(x,
                                  arg = rlang::caller_arg(x),
                                  call = rlang::caller_env()){

  is_column_array <- is_2d(x) && dim(x)[2] == 1
  if (!is_column_array) {
    cli::cli_abort(
      message = c(
        "{.arg {arg}} not a column vector array",
        "{.arg {arg}} must be a column vector array",
        "x" = "{.arg {arg}} has dimensions:",
        "{.val {pretty_dim(x)}}"
      ),
      call = call
    )
  }
}

check_rows_equal <- function(a,
                             b,
                             a_arg = rlang::caller_arg(a),
                             b_arg = rlang::caller_arg(b),
                             call = rlang::caller_env()){

  # b must have the right number of rows
  rows_not_equal <- dim(b)[1] != dim(a)[1]
  if (rows_not_equal) {
    cli::cli_abort(
      message = c(
        "Number of rows not equal",
        "x" = "{.arg {b_arg}} must have the same number of rows as \\
        {.arg {a_arg}} ({.val {dim(a)[1]}}), but has \\
        {.val {dim(b)[1]}} rows instead"
      ),
      call = call
    )
  }
}

check_final_dim <- function(dim,
                            thing,
                            call = rlang::caller_env()){
  # dimension of the free state version
  n_dim <- length(dim)
  last_dim <- dim[n_dim]
  n_last_dim <- length(last_dim)
  last_dim_gt_1 <- !last_dim > 1
  if (last_dim_gt_1) {
    cli::cli_abort(
      message = c(
        "The final dimension of a {thing} must have more than \\
      one element",
        "The final dimension has: {.val {n_last_dim} element{?s}}"
      ),
      call = call
    )
  }

}

check_param_greta_array <- function(x,
                                    arg = rlang::caller_arg(x),
                                    call = rlang::caller_env()){
  if (is.greta_array(x)) {
    cli::cli_abort(
      message = "{.arg {arg}} must be fixed, they cannot be another \\
      {.cls greta_array}",
      call = call
    )
  }
}

check_not_greta_array <- function(x,
                                  arg = rlang::caller_arg(x),
                                  call = rlang::caller_env()){
  if (is.greta_array(x)) {
    cli::cli_abort(
      "{.arg {arg}} cannot be a {.cls greta_array}"
    )
  }
}

# if it errored
check_for_errors <- function(res,
                             call = rlang::caller_env()){

  if (inherits(res, "error")) {

    # check for known numerical errors
    numerical_errors <- vapply(greta_stash$numerical_messages,
                               grepl,
                               res$message,
                               FUN.VALUE = 0
    ) == 1

    # if it was just a numerical error, quietly return a bad value
    if (!any(numerical_errors)) {
      cli::cli_abort(
        message = c(
          "{.pkg greta} hit a tensorflow error:",
          "{res}"
        ),
        call = call
      )
    }
  }

}

check_dim_length <- function(dim,
                             call = rlang::caller_env()){

  ndim <- length(dim)
  ndim_gt2 <- ndim > 2
  if (ndim_gt2) {
    cli::cli_abort(
      message = c(
        "{.arg dim} can either be a scalar or a vector of length 2",
        "However {.arg dim} has length {.val {ndim}}, and contains:",
        "{.val {dim}}"
      ),
      call = call
    )
  }
}

check_is_distribution_node <- function(distribution,
                                       call = rlang::caller_env()){
  if (!is.distribution_node(distribution)) {
    cli::cli_abort(
      message = c("Invalid distribution"),
      call = call
    )
  }

}

check_values_dim <- function(value,
                             dim,
                             call = rlang::caller_env()){
  values_have_wrong_dim <- !is.null(value) && !all.equal(dim(value), dim)
  if (values_have_wrong_dim) {
    cli::cli_abort(
      message = "Values have the wrong dimension so cannot be used",
      call = call
    )
  }

}

# check they are all scalar
check_dot_nodes_scalar <- function(dot_nodes,
                                   call = rlang::caller_env()){
  are_scalar <- vapply(dot_nodes, is_scalar, logical(1))
  if (!all(are_scalar)) {
    cli::cli_abort(
      message = "{.fun joint} only accepts probability distributions over \\
      scalars",
      call = call
    )
  }

}

inform_if_one_set_of_initials <- function(initial_values,
                                          n_chains,
                                          call = rlang::caller_env()){

  is_blank <- identical(initial_values, initials())

  one_set_of_initials <- !is_blank & n_chains > 1
  if (one_set_of_initials) {
    cli::cli_inform(
      message = "Only one set of initial values was provided, and was used \\
        for all chains"
    )
  }
}

# the user might pass greta arrays with groups of nodes that are unconnected
# to one another. Need to check there are densities in each graph
check_subgraphs <- function(dag,
                            call = rlang::caller_env()){
  # get and check the types
  types <- dag$node_types

  # the user might pass greta arrays with groups of nodes that are unconnected
  # to one another. Need to check there are densities in each graph

  # so find the subgraph to which each node belongs
  graph_id <- dag$subgraph_membership()

  graphs <- unique(graph_id)
  n_graphs <- length(graphs)

  # separate messages to avoid the subgraphs issue for beginners

  if (n_graphs == 1) {
    density_message <- cli::format_error(
      c(
        "none of the {.cls greta_array}s in the model are associated with a \\
        probability density, so a model cannot be defined"
      )
    )
    variable_message <- cli::format_error(
      c(
        "none of the {.cls greta_array}s in the model are unknown, so a model \\
        cannot be defined"
      )
    )
  } else {
    density_message <- cli::format_error(
      c(
        "the model contains {n_graphs} disjoint graphs",
        "one or more of these sub-graphs does not contain any \\
        {.cls greta_array}s that are associated with a probability density, \\
        so a model cannot be defined"
      )
    )
    variable_message <- cli::format_error(
      c(
        "the model contains {n_graphs} disjoint graphs",
        "one or more of these sub-graphs does not contain any \\
          {.cls greta_array}s that are unknown, so a model cannot be defined"
      )
    )
  }

  for (graph in graphs) {
    types_sub <- types[graph_id == graph]

    # check they have a density among them
    no_distribution <- !("distribution" %in% types_sub)
    if (no_distribution) {
      cli::cli_abort(
        message = density_message,
        call = call
      )
    }

    # check they have a variable node among them
    no_variable_node <- !("variable" %in% types_sub)
    if (no_variable_node) {
      cli::cli_abort(
        message = variable_message,
        call = call
      )
    }
  }

}

check_has_representation <- function(repr,
                                     name,
                                     error,
                                     call = rlang::caller_env()){
  not_represented <- error && is.null(repr)
  if (not_represented) {
    cli::cli_abort(
      message = "{.cls greta_array} has no representation {.var {name}}",
      call = call
    )
  }
}

check_has_anti_representation <- function(repr,
                                     name,
                                     error,
                                     call = rlang::caller_env()){
  not_anti_represented <- error && is.null(repr)
  if (not_anti_represented) {
    cli::cli_abort(
      message = "{.cls greta_array} has no anti representation {.var {name}}",
      call = call
    )
  }
}

check_is_greta_array <- function(x,
                                 arg = rlang::caller_arg(x),
                                 call = rlang::caller_env()){
  if (!is.greta_array(x)) {
    cli::cli_abort(
      message = c(
        "{.arg {arg}} must be {.cls greta_array}",
        "{.arg {arg}} is: {.cls {class(x)}}"
      ),
      call = call
    )
  }
}

check_missing_infinite_values <- function(x,
                                          optional,
                                          call = rlang::caller_env()){
  contains_missing_or_inf <- !optional & any(!is.finite(x))
  if (contains_missing_or_inf) {
    cli::cli_abort(
      message = c(
        "{.cls greta_array} must not contain missing or infinite values"
      ),
      call = call
    )
  }
}

check_truncation_implemented <- function(tfp_distribution,
                                         distribution_node,
                                         call = rlang::caller_env()){

  cdf <- tfp_distribution$cdf
  quantile <- tfp_distribution$quantile

  is_truncated <- is.null(cdf) | is.null(quantile)
  if (is_truncated) {
    cli::cli_abort(
      message = c(
        "Sampling is not yet implemented for truncated \\
            {.val {distribution_node$distribution_name}} distributions"
      ),
      call = call
    )
  }

}

check_sampling_implemented <- function(sample,
                                       distribution_node,
                                       call = rlang::caller_env()){
  if (is.null(sample)) {
    cli::cli_abort(
      "Sampling is not yet implemented for \\
            {.val {distribution_node$distribution_name}} distributions"
    )
  }
}

check_timeout <- function(it,
                          maxit,
                          call = rlang::caller_env()){
  # check we didn't time out
  if (it == maxit) {
    cli::cli_abort(
      message = c(
      "Could not determine the number of independent models in a reasonable \\
      amount of time",
      "Iterations = {.val {it}}",
      "Maximum iterations = {.cal {maxit}}"
      ),
      call = call
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
  complex_error,
  check_targets_stochastic_and_not_sampled,
  check_dag_introduces_new_variables,
  check_commanality_btn_dags,
  check_finite_positive_scalar_integer,
  check_if_greta_mcmc_list,
  check_2d_multivariate
)

