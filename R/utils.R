# utility functions

# create a named list
module <- function(..., sort = TRUE) {
  dots <- list(...)
  names <- names(dots)

  # guess names from call
  cl <- match.call()
  nm <- as.character(as.list(cl)[-1])

  if (is.null(names)) {
    names(dots) <- nm
  } else {
    blank_names <- names == ""
    names[blank_names] <- nm[blank_names]
    names(dots) <- names
  }

  if (sort) {
    dots <- dots[order(names(dots))]
  }

  dots
}

# find out whether the usr has conda installed and visible
#' @importFrom reticulate conda_binary
have_conda <- function() {
  conda_bin <- tryCatch(reticulate::conda_binary("auto"),
    error = function(e) NULL
  )
  !is.null(conda_bin)
}

#' @importFrom reticulate py_available
have_python <- function() {
  tryCatch(
    expr = reticulate::py_available(initialize = TRUE),
    error = function(e) FALSE
  )
}

#' @importFrom reticulate py_module_available
have_tfp <- function() {
  is_tfp_available <- reticulate::py_module_available("tensorflow_probability")

  if (is_tfp_available) {

    pkg <- reticulate::import("pkg_resources")
    tfp_version <- pkg$get_distribution("tensorflow_probability")$version
    is_tfp_available <- utils::compareVersion("0.7.0", tfp_version) == 0

  }

  return(is_tfp_available)

}

#' @importFrom reticulate py_module_available
have_tf <- function() {

  is_tf_available <- reticulate::py_module_available("tensorflow")

  if (is_tf_available) {

    tf_version <- tf$`__version__`
    is_tf_available <- utils::compareVersion("1.14.0", tf_version) == 0

  }

  return(is_tf_available)

}

version_tf <- function(){
  if (have_tf()) {
    tf$`__version__`
  } else {
    msg <- cli::format_error(
      "{.pkg tensorflow} not found"
    )
    stop(
      msg,
      call. = FALSE
      )
  }
}

version_tfp <- function(){
  if (have_tfp()) {
    tfp$`__version__`
  } else {
    msg <- cli::format_message("{.pkg tensorflow-probability} not found")
    message(msg)
  }
}

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

    # if there was a problem, append the solution
      message_text <- cli::format_message(
        c(
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
      )

      warning_text <- cli::format_warning(
        c(
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
      )

      error_text <- cli::format_error(
        c(
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
      )

    switch(alert,
      error = stop(error_text, call. = FALSE),
      warn = warning(warning_text, call. = FALSE),
      message = message(message_text),
      startup = packageStartupMessage(message_text),
      none = NULL
    )
  }

  invisible(all(requirements_valid))

}

# helper for *apply statements on R6 objects
member <- function(x, method) {
  eval(parse(text = paste0("x$", method)))
}

node_type <- function(node) {
  classes <- class(node)
  type <- grep("*_node", classes, value = TRUE)
  gsub("_node", "", type)
}

# access the float type option
tf_float <- function() {
  float_name <- options()$greta_tf_float
  tf[[float_name]]
}

# cast an R scalar as a float of the correct type in TF code
fl <- function(x) {
  tf$constant(x, dtype = tf_float())
}

# get the tensor for the batch size in the dag currently defining (since it's
# not alway possible to pass the dag in)
get_batch_size <- function() {
  options()$greta_batch_size
}

# coerce an integer(ish) vector to a list as expected in tensorflow shape
# arguments
#' @noRd
#' @importFrom tensorflow shape
to_shape <- function(dim) {
  do.call(shape, as.list(dim))
}

# is this greta_array actually a scalar?
is_scalar <- function(x) {
  identical(dim(x), c(1L, 1L))
}

# is it a row vector?
is_row <- function(x) {
  length(dim(x) == 1) && dim(x)[1] == 1L
}

# flatten a greta array into a column vector in column-major order
flatten <- function(x) {
  x[seq_along(x)]
}

# return an integer to pass on as an RNG seed
get_seed <- function() {
  sample.int(1e12, 1)
}

# does a pointer exist (as a named object) and is it from the current session
# use like: live_pointer("joint_density", dag$tf_environment)
live_pointer <- function(tensor_name, environment = parent.frame()) {
  exists(tensor_name, envir = environment) &&
    !is.null(environment[[tensor_name]]$name)
}

# nolint start
# get the next seed as a L'Ecuyer
future_seed <- function() {
  okind <- RNGkind()[1]
  on.exit(RNGkind(okind), add = TRUE)
  RNGkind("L'Ecuyer-CMRG")
  .GlobalEnv$.Random.seed
}
# nolint end

create_log_file <- function(create = FALSE) {
  filename <- tempfile(pattern = "greta_log_")
  if (create) {
    file.create(filename)
  }
  filename
}

# given a number of bars to be printed at the same time, determine the width of
# sub process bars, so they all fit on the same line
bar_width <- function(n_bars) {
  terminal_width <- options()$width

  # a space between each bar, divide up the remainder and add 2 spaces to each
  total_width <- terminal_width - (n_bars - 1)
  bar_width <- total_width %/% n_bars
  bar_width - 2
}

# record the messages produced by the expression in the file
#' @importFrom utils capture.output
record <- function(expr, file) {
  if (!is.null(file)) {
    msg <- capture.output(out <- eval(expr), type = "message")
    writeLines(msg, file)
  } else {
    out <- eval(expr)
  }
  invisible(out)
}

# convert an assumed numeric to an array with at least 2 dimensions
as_2d_array <- function(x) {

  # coerce data from common formats to an array here
  x <- as.array(x)

  # coerce 1D arrays to column vectors
  if (length(dim(x)) == 1) {
    dim(x) <- c(dim(x), 1)
  }

  x
}

# add an additional dimension at the beginning of an array
add_first_dim <- function(x) {
  x <- as.array(x)
  array(x, dim = c(1, dim(x)))
}

# drop the additional dimension at the beginning of an array
drop_first_dim <- function(x) {
  x <- as.array(x)
  if (length(dim(x)) > 1) {
    x <- array(x, dim = dim(x)[-1])
  }
  x
}

# given an R array with first dimension of size 1, tile it to have size 'times'
# on that dimension
tile_first_dim <- function(x, times) {
  x_list <- replicate(times, x, simplify = FALSE)
  do.call(abind::abind, c(x_list, list(along = 1)))
}

# if x is an R matrix representing a column vector, make it a plain R vector
drop_column_dim <- function(x) {
  dims <- dim(x)
  if (length(dims) == 2 && dims[2] == 1L) {
    x <- as.vector(x)
  }
  x
}

# where x is a tensor with no batch dimension, and y is a tensor with a batch
# dimension, tile x to have first dimension matching y (dimension determined at
# run time)
expand_to_batch <- function(x, y) {
  batch_size <- tf$shape(y)[[0]]
  ndim <- length(dim(x))
  tf$tile(x, c(batch_size, rep(1L, ndim - 1)))
}

# does this tensor have a batch dimension (of unknown size) as its first
# dimension?
has_batch <- function(x) is.null(dim(x)[[1]])

# given a list of tensors, if none or all of them have a batch dimension, return
# the list. If any (but not all) of them has a batch dimension, tile the
# unbatched ones (which are assumed to have first dimension 1) to match the
# dimension of the batched ones dimension
match_batches <- function(values) {
  is_tensor <- vapply(values, inherits, "tensorflow.tensor", FUN.VALUE = FALSE)

  values_mutable <- values[is_tensor]

  have_batches <- vapply(values_mutable, has_batch, FUN.VALUE = TRUE)

  # if any, but not all, have a batch, dimension, tile the others to match the
  # batch
  if (!all(have_batches) & any(have_batches)) {
    target_id <- which(have_batches)[1]
    target <- values_mutable[[target_id]]

    for (i in which(!have_batches)) {
      values_mutable[[i]] <- expand_to_batch(values_mutable[[i]], target)
    }
  }

  values[is_tensor] <- values_mutable

  values
}

# split a 3D array of n_samples * n_chains * n_parameters posterior samples into
# a list of n_chains 2D arrays of dimension n_samples * n_parameters
split_chains <- function(samples_array) {
  dims_in <- dim(samples_array)
  dims_out <- dims_in[-2]
  n_chains <- dims_in[2]

  lapply(
    seq_len(n_chains),
    function(i) {
      x <- samples_array[, i, , drop = FALSE]
      dim(x) <- dims_out
      x
    }
  )
}

# take a greta array dimension and return the dimension of the hessian to return
# to the user
hessian_dims <- function(dim) {
  if (length(dim) == 2 && dim[2] == 1L) {
    dim <- dim[1]
  }
  rep(dim, 2)
}

# generate a random 8-digit hexadecimal string
rhex <- function() {
  paste(as.raw(sample.int(256L, 4, TRUE) - 1L), collapse = "")
}

# stop TensorFlow messaging about deprecations etc.
#' @importFrom reticulate py_set_attr import
disable_tensorflow_logging <- function(disable = TRUE) {
  logging <- reticulate::import("logging")
  # nolint start
  logger <- logging$getLogger("tensorflow")
  # nolint end
  reticulate::py_set_attr(logger, "disabled", disable)
}


pad_vector <- function(x, to_length, with = 1) {
  pad_by <- to_length - length(x)
  if (pad_by > 0) {
    x <- c(x, rep(with, pad_by))
  }
  x
}

has_distribution <- function(node) {
  !is.null(node$distribution)
}

misc_module <- module(
  module,
  check_tf_version,
  member,
  node_type,
  tf_float,
  fl,
  to_shape,
  is_scalar,
  flatten,
  get_seed,
  live_pointer,
  future_seed,
  create_log_file,
  bar_width,
  record,
  as_2d_array,
  add_first_dim,
  drop_first_dim,
  tile_first_dim,
  drop_column_dim,
  expand_to_batch,
  has_batch,
  match_batches,
  split_chains,
  hessian_dims,
  rhex,
  disable_tensorflow_logging,
  pad_vector
)

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
      msg <- cli::format_error(
        "incompatible dimensions: {dims_text}"
        )

      stop(msg, call. = FALSE)
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

        msg <- cli::format_error(
          c(
            "incorrect array dimensions",
            "x" = "array dimensions should be \\
              {paste(target_dim, collapse = 'x')},",
            "but input dimensions were {dims_text}."
          )
        )

        stop(
          msg,
          call. = FALSE
          )

      }

    }

    output_dim <- target_dim
  } else {

    # otherwise, find the correct output dimension
    dim_lengths <- vapply(dim_list, length, numeric(1))
    dim_list <- lapply(dim_list, pad_vector, to_length = max(dim_lengths))
    output_dim <- do.call(pmax, dim_list)
  }

  output_dim
}

# make sure a greta array is 2D
check_2d <- function(x) {
  if (length(dim(x)) != 2L) {
    msg <- cli::format_error(
      c(
        "Dimensions of parameters not compatible with multivariate \\
        distribution parameters of multivariate distributions cannot have \\
        more than two dimensions",
        "object {.var x} has dimensions: {paste(dim(x), collapse = 'x')}"
      )
    )
    stop(
      msg,
      call. = FALSE
    )
  }
}

check_square <- function(x) {
  dim <- dim(x)
  ndim <- length(dim)
  is_square <- ndim == 2 && dim[1] == dim[2]
  if (!is_square) {
    msg <- cli::format_error(
      c(
        "Not 2D square greta array",
        "x" = "expected a 2D square greta array, but object {.var x} had \\
        dimension: {paste(dim, collapse = 'x')}"
      )
    )
    stop(
      msg,
      call. = FALSE
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
      msg <- cli::format_error(
        c(
          "incompatible number of rows",
          x = "{paste(nrows, collapse = ' vs ')}"
          )
        )
      stop(
        msg,
        call. = FALSE
        )
    }
  }

  # if there's a target number of realisations, check it's valid and make sure
  # they all match it
  if (!is.null(target)) {

    # make sure it's a scalar
    if (length(target) != 1 || target < 1) {
      msg <- cli::format_error(
        c(
          "{.code n_realisations is not a positive scalar interger}",
          "{.code n_realisations} must be a positive scalar integer giving \\
            the number of rows of the output",
          "x" = "We see {.code n_realisations} = {.code {n_realisations}} \\
            having class: \\
            {.cls {class(n_realisations)}} and length \\
            {.var {length(n_realisations)}}"
        )
      )
      stop(
        msg,
        call. = FALSE
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
        msg <- cli::format_error(
          c(
            "Realisations do not match rows",
            "number of realisations should be {target},",
            "but arguments had {paste(nrows, collapse = ', ')} rows"
            )
          )
        stop(
          msg,
          call. = FALSE
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
      msg <- cli::format_error(
        c(
          "{.var dimension} must be a positive scalar integer giving the \\
          dimension of the distribution",
          "{.code dim(target)} returns: {dim(target)}"
        )
      )
      stop(
        msg,
        call. = FALSE
      )
    }

    dimension <- as.integer(target)
  } else {

    # otherwise, get it from the first parameter
    dimension <- ncols[[1]]
  }

  # check it's big enough
  if (dimension < min_dimension) {
    msg <- cli::format_error(
      c(
        "the dimension of this distribution must be at least \\
        {min_dimension}, but was {dimension}",
        "multivariate distributions treat each {.emph row} as a separate \\
        realisation - perhaps you need to transpose something?"
        )
    )
    stop(
      msg,
      call. = FALSE
    )
  }

  # make sure all the parameters match this dimension
  match_dimension <- vapply(ncols, identical, dimension,
    FUN.VALUE = FALSE
  )

  # otherwise it's not fine
  if (!all(match_dimension)) {
    msg <- cli::format_error(
      c(
        "distribution dimensions do not match implied dimensions",
        "The distribution dimension should be {dimension}, but parameters \\
        implied dimensions: {paste(ncols, collapse = ' vs ')}",
        "Multivariate distributions treat each {.emph row} as a separate \\
        realisation - perhaps you need to transpose something?"
      )
    )
    stop(
      msg,
      call. = FALSE)
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
    msg <- cli::format_error(
      c(
        "lower bound must be 0 or higher",
        "lower bound is: {.val {truncation[1]}}"
      )
      )
    stop(
      msg,
      call. = FALSE
    )
  }
}

check_unit <- function(truncation) {
  if (truncation[1] < 0 | truncation[2] > 1) {
    msg <- cli::format_error(
      c(
        "lower and upper bounds must be between 0 and 1",
        "lower bound is: {.val {truncation[1]}}",
        "upper bound is: {.val {truncation[2]}}"
        )
      )
    stop(
      msg,
      call. = FALSE
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
    msg <- cli::format_error(
      c(
        "Wrong function name provided in another model",
        "It looks like you're using {.pkg greta}'s {.fun {function_name}} \\
        function in the family argument of another model.",
        "Maybe you want to use {.code family = stats::{function_name}},instead?"
      )
    )
    stop(msg, call. = FALSE)
  }
}

# get & return information about the future plan, and error nicely if invalid

#' @importFrom future plan future
check_future_plan <- function() {
  plan_info <- future::plan()

  plan_is <- list(
    parallel = !inherits(plan_info, "sequential"),
    cluster = inherits(plan_info, "cluster"),
    multiprocess = inherits(plan_info, "multiprocess"),
    multisession = inherits(plan_info, "multisession"),
    local = TRUE
  )

  # if running in parallel
  if (plan_is$parallel) {

    # if it's a cluster, check there's no forking
    if (plan_is$cluster) {

      # This stopgap trick from Henrik on github:
      f <- future::future(NULL, lazy = TRUE)
      workers <- f$workers
      if (inherits(workers, "cluster")) {
        worker <- workers[[1]]
        if (inherits(worker, "forknode")) {
          msg <- cli::format_error(
            "parallel mcmc samplers cannot be run with a fork cluster"
            )
          stop(
            msg,
            call. = FALSE
          )
        }
      }

      # check whether the cluster is local
      if (!is.null(worker$host)) {
        localhosts <- c("localhost", "127.0.0.1", Sys.info()[["nodename"]])
        plan_is$local <- worker$host %in% localhosts
      }
    } else {

      # if multi*, check it's multisession
      if (plan_is$multiprocess && !plan_is$multisession) {
        msg <- cli::format_error(
          "parallel mcmc samplers cannot be run with \\
          {.code plan(multiprocess)} or {.code plan(multicore)}"
        )
        stop(
          msg,
          call. = FALSE
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
    stop(msg, hint, call. = FALSE)
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
    msg <- cli::format_error(
      "the names of arguments to values must all correspond to named \\
      {.cls greta_array}s"
      )
    stop(
      msg,
      call. = FALSE
    )
  }

  # coerce value to have the correct dimensions
  assign_dim <- function(value, greta_array) {
    array <- unclass(get_node(greta_array)$value())
    if (length(array) != length(value)) {
      msg <- cli::format_error(
        "a provided value has different number of elements than the \\
        {.cls greta_array}"
      )
      stop(
        msg,
        call. = FALSE
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
    msg <- cli::format_error(
      c(
        "{.var x} must be a column vector",
        "but {.var x} has dimensions {paste(dims, collapse = 'x')}"
      )
    )
    stop(
      msg,
      call. = FALSE
    )
  }
}

complex_error <- function(z) {
  msg <- cli::format_error(
    "{.pkg greta} does not yet support complex numbers"
    )
  stop(
    msg,
    call. = FALSE
  )
}

checks_module <- module(
  check_dims,
  check_unit,
  check_positive,
  check_in_family,
  check_future_plan,
  check_greta_arrays,
  check_values_list,
  check_dependencies_satisfied,
  check_cum_op,
  complex_error
)

# convert an array to a vector row-wise
flatten_rowwise <- function(array) {
  dim <- dim(array)
  array <- aperm(array, rev(seq_along(dim)))
  dim(array) <- NULL
  array
}

# convert an vector to an array row-wise
unflatten_rowwise <- function(array, dim) {
  array <- as.array(array)
  # if any dim has length 1, make it a column vector
  if (length(dim) == 1) {
    dim <- c(dim, 1)
  }

  dim(array) <- rev(dim)
  array <- aperm(array, rev(seq_along(dim)))
  dim(array) <- dim
  array
}

# create an array with the same dimensions as tensor and fill it with
# consecutive increasing integers in python order
dummy <- function(dims) {
  vec <- seq_len(prod(dims)) - 1
  unflatten_rowwise(vec, dims)
}

# create a greta array of zeros with the correct dimensions
dummy_greta_array <- function(x) {
  do.call(zeros, list(dim(x)))
}

dummy_array_module <- module(
  flatten_rowwise,
  unflatten_rowwise,
  dummy,
  dummy_greta_array
)

# given a base colour, return a function taking a value between 0 and 1 and
# returning a colour linearly interpolated between black, the colour and white,
# so that values close to 0.5 match the base colour, values close to 0 are
# nearer black, and values close to 1 are nearer white
#' @importFrom grDevices colorRampPalette
palettize <- function(base_colour) {
  pal <- colorRampPalette(c("#000000", base_colour, "#ffffff"))
  function(val) {
    stopifnot(val > 0 & val < 1)
    cols <- pal(1001)
    cols[round(val * 1000 + 1)]
  }
}

# colour scheme for plotting
#' @importFrom grDevices col2rgb
greta_col <- function(which = c(
                        "main",
                        "dark",
                        "light",
                        "lighter",
                        "super_light"
                      ),
                      colour = "#996bc7") {

  # tests if a color encoded as string can be converted to RGB
  tryCatch(
    is.matrix(grDevices::col2rgb(colour)),
    error = function(e) {
      msg <- cli::format_error(
          "Invalid colour: {colour}"
      )
      stop(
        msg,
        call. = FALSE
        )
    }
  )

  which <- match.arg(which)
  pal <- palettize(colour)
  switch(which,
    dark = pal(0.45), # 45%
    main = pal(0.55), # 55%
    light = pal(0.65), # 65%ish
    lighter = pal(0.85), # 85%ish
    super_light = pal(0.95)
  ) # 95%ish
}

colour_module <- module(
  palettize,
  greta_col
)

# look in the environment specified by env, and return a named list of all greta
# arrays in that environment
all_greta_arrays <- function(env = parent.frame(),
                             include_data = TRUE) {

  # all objects in that environment as a named list
  all_object_names <- ls(envir = env)

  # loop carefully in case there are unfulfilled promises
  all_objects <- list()
  for (name in all_object_names) {
    all_objects[[name]] <- tryCatch(get(name, envir = env),
      error = function(e) NULL
    )
  }

  # find the greta arrays
  is_greta_array <- vapply(all_objects,
    inherits,
    "greta_array",
    FUN.VALUE = FALSE
  )
  all_arrays <- all_objects[is_greta_array]

  # optionally strip out the data arrays
  if (!include_data) {
    is_data <- vapply(all_arrays,
      function(x) inherits(get_node(x), "data_node"),
      FUN.VALUE = FALSE
    )
    all_arrays <- all_arrays[!is_data]
  }

  all_arrays
}

# suppress the R or python output of R expressions
quietly <- function(expr) {
  py_out <- reticulate::py_capture_output(
    r_out <- capture.output(expr)
  )
  out <- c(py_out, r_out)
  invisible(out)
}

# evaluate expressions (dag density or gradient), capturing numerical errors
# like matrix inversions as bad samples, and erroring otherwise
cleanly <- function(expr) {
  res <- tryCatch(expr, error = function(e) e)

  # if it errored
  if (inherits(res, "error")) {

    # check for known numerical errors
    numerical_errors <- vapply(greta_stash$numerical_messages,
      grepl,
      res$message,
      FUN.VALUE = 0
    ) == 1

    # if it was just a numerical error, quietly return a bad value
    if (!any(numerical_errors)) {
      msg <- cli::format_error(
        c(
          "{.pkg greta} hit a tensorflow error:",
          "{res}"
        )
      )
      stop(
        msg,
        call. = FALSE
        )
    }
  }

  res
}

# prepare a matrix of draws and return as an mcmc object
#' @noRd
#' @importFrom coda mcmc
prepare_draws <- function(draws, thin = 1) {
  draws_df <- data.frame(draws, check.names = FALSE)
  draws_df <- na.omit(draws_df)
  coda::mcmc(draws_df, thin = thin)
}

build_sampler <- function(initial_values, sampler, model, seed = get_seed()) {
  sampler$class$new(initial_values,
    model,
    sampler$parameters,
    seed = seed
  )
}

# unlist and flatten a list of arrays to a vector row-wise
unlist_tf <- function(x) {
  # flatten each element row-wise and concatenate
  x <- lapply(x, flatten_rowwise)
  do.call(c, x)
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

    msg <- cli::format_warning(
      "{n_cores} cores were requested, but only {n_cores_detected} \\
      are available."
    )

    warning(
      msg,
      call. = FALSE
      )

    n_cores <- NULL
  }

  # if n_cores isn't user-specified, set it so
  # there's no clash between samplers
  if (is.null(n_cores)) {
    n_cores <- floor(n_cores_detected / samplers)
  }

  # make sure there's at least 1
  n_cores <- max(n_cores, 1)

  as.integer(n_cores)
}

check_positive_integer <- function(x, name = "") {
  suppressWarnings(x <- as.integer(x))

  if (length(x) != 1 | is.na(x) | x < 1) {
    msg <- cli::format_error(
      c(
        "{name} must be a positive integer",
        "However the value provided was: {.val {x}}"
      )
    )
    stop(
      msg,
      call. = FALSE
    )
  }

  x
}

# batch sizes must be positive numerics, rounded off to integers
check_trace_batch_size <- function(x) {
  valid <- is.numeric(x) && length(x) == 1 && x >= 1
  if (!valid) {
    msg <- cli::format_error(
      "{.var trace_batch_size} must be a single numeric value greater than or \\
      equal to 1"
    )
    stop(
      msg,
      call. = FALSE
    )
  }
  x
}

# get better names for the scalar elements of a greta array, for labelling mcmc
# samples
get_indices_text <- function(dims, name) {
  ndim <- prod(dims)
  if (ndim > 1) {
    vec <- seq_len(ndim)
    if (length(vec)) {
      indices <- arrayInd(vec, dims)
    }
    mid_text <- apply(indices, 1, paste, collapse = ",")
    name <- paste0(name, "[", mid_text, "]")
  }
  name
}

# given a list 'trace_list' of arrays giving the values of the target greta
# arrays (with their true dimensions), return the ith element, flattened to a
# vector and with elements given informative names
flatten_trace <- function(i, trace_list) {
  object <- names(trace_list)[i]
  values <- trace_list[[i]]
  dim_in <- dim(values)
  dim_slice <- dim_in[-1]
  dim_out <- c(dim_in[1], prod(dim_slice))
  dim(values) <- dim_out
  names <- get_indices_text(dim_slice, object)
  colnames(values) <- names
  values
}

# extract the model information object from mcmc samples returned by
# stashed_samples, and error nicely if there's something fishy
get_model_info <- function(draws, name = "value") {
  if (!inherits(draws, "greta_mcmc_list")) {
    msg <- cli::format_error(
      c(
        "{name} must be an {.cls greta_mcmc_list} object",
        "created by {.fun greta::mcmc}, {.fun greta::stashed_samples}, or \\
        {.fun greta::extra_samples}"
      )
    )
    stop(
      msg,
      call. = FALSE
    )
  }

  model_info <- attr(draws, "model_info")
  valid <- !is.null(model_info)

  if (!valid) {
    msg <- cli::format_error(
      c(
        "{name} is an {.cls mcmc.list} object, but is not associated with any\\
        model information",
        "perhaps it wasn't created by {.fun greta::mcmc}, \\
        {.fun greta::stashed_samples}, or {.fun greta::extra_samples}?"
      )
    )
    stop(
      msg,
      call. = FALSE
    )
  }

  model_info
}

sampler_utils_module <- module(
  all_greta_arrays,
  cleanly,
  build_sampler,
  prepare_draws,
  unlist_tf,
  check_future_plan,
  check_n_cores,
  check_positive_integer,
  get_indices_text,
  flatten_trace,
  get_model_info
)

# convert a function on greta arrays into a function on corresponding tensors,
# given the greta arrays for inputs. When executed, this needs to be wrapped in
# dag$on_graph() to get the tensors connected up with the rest of the graph
as_tf_function <- function(r_fun, ...) {

  # run the operation on isolated greta arrays, so nothing gets attached to the
  # model real greta arrays in dots
  ga_dummies <- lapply(list(...), dummy_greta_array)
  ga_out <- do.call(r_fun, ga_dummies)
  ga_out

  function(...) {
    tensor_inputs <- list(...)

    # if any of these are shapeless, make them into greta scalars (3D)
    tensor_inputs <- lapply(
      tensor_inputs,
      function(x) {
        if (identical(dim(x), list())) {
          x <- tf$reshape(x, shape(1, 1, 1))
        }
        x
      }
    )

    # create a sub-dag for these operations, from ga_dummies to ga_out
    if (!is.list(ga_out)) {
      ga_out <- list(ga_out)
    }
    targets <- c(ga_out, ga_dummies)
    sub_dag <- dag_class$new(targets)

    # use the default graph, so that it can be overwritten when this is called?
    # alternatively fetch from above, or put it in greta_stash?
    sub_dag$tf_graph <- tf$compat$v1$get_default_graph()
    sub_tfe <- sub_dag$tf_environment

    # pass on the batch size, used when defining data
    sub_tfe$batch_size <- get_batch_size()

    # set the input tensors as the values for the dummy greta arrays in the new
    # tf_environment
    node_dummies <- lapply(ga_dummies, get_node)
    tf_names <- lapply(node_dummies, sub_dag$tf_name)
    for (i in seq_along(tf_names)) {
      assign(tf_names[[i]], tensor_inputs[[i]], envir = sub_tfe)
    }

    # have output node define_tf in the new environment, with data defined as
    # constants
    greta_stash$data_as_constants <- TRUE
    on.exit(greta_stash$data_as_constants <- NULL)

    tf_out <- list()
    for (i in seq_along(ga_out)) {

      # define the output nodes
      node_out <- get_node(ga_out[[i]])
      node_out$define_tf(sub_dag)

      # get the tensors for the outputs
      tf_out[[i]] <- sub_tfe[[sub_dag$tf_name(node_out)]]
    }

    if (length(tf_out) == 1) {
      tf_out <- tf_out[[1]]
    }

    tf_out
  }
}

is_windows <- function() {
  identical(.Platform$OS.type, "windows")
}

greta_array_ops_module <- module(as_tf_function)

# utilities to export via .internals
utilities_module <- module(
  misc = misc_module,
  dummy_arrays = dummy_array_module,
  greta_array_operations = greta_array_ops_module,
  samplers = sampler_utils_module,
  checks = checks_module,
  colours = colour_module
)
