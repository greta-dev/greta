# utility functions

# create a named list
module <- function (..., sort = TRUE) {

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

  if (sort)
    dots <- dots[order(names(dots))]

  dots

}


# check tensorflow is installed and the version of tensorflow is valid. error,
# warn, or message if not and (if not an error) return an invisible logical
# saying whether it is valid
check_tf_version <- function (alert = c('none', 'error', 'warn', 'message', 'startup')) {

  alert <- match.arg(alert)
  text <- NULL
  available <- TRUE

  if (!reticulate::py_module_available('tensorflow')) {

    text <- "TensorFlow isn't installed."
    available <- FALSE

  } else {

    tf_version <- tf$`__version__`
    tf_version_split <- strsplit(tf_version, '.', fixed = TRUE)[[1]]
    tf_version_valid <- as.numeric(tf_version_split[1]) >= 1

    if (!tf_version_valid) {

      text <- paste0("you have version ", tf_version)
      available <- FALSE

    }

  }

  if (!is.null(text)) {

    text <- paste0("\n\n  greta requires TensorFlow version 1.0.0 or higher, ",
                   "but ", text, "\n  ",
                   "Use install_tensorflow() to install the latest version.",
                   "\n\n")
    switch(alert,
           error = stop (text, call. = FALSE),
           warn = warning (text, call. = FALSE),
           message = message(text),
           startup = packageStartupMessage(text),
           none = NULL)
  }

  invisible(available)

}

# helper for *apply statements on R6 objects
member <- function (x, method)
  eval(parse(text = paste0('x$', method)))

node_type <- function (node) {
  classes <- class(node)
  type <- grep('*_node', classes, value = TRUE)
  gsub('_node', '', type)
}

# access the float type option
tf_float <- function ()
  options()$greta_tf_float

# cast an R scalar as a float of the correct type in TF code
fl <- function (x)
  tf$constant(x, dtype = tf_float())

# coerce an integer(ish) vector to a list as expected in tensorflow shape arguments
to_shape <- function (dim)
  do.call(shape, as.list(dim))

# is this greta_array actually a scalar?
is_scalar <- function (x)
  identical(dim(x), c(1L, 1L))

# flatten a greta array into a column vector in column-major order
flatten <- function (x)
  x[seq_along(x)]

# helper function to loop through lists of R6 objects with lapply, executing the
# member function "name" on the arguments in dots. Using the syntax:
#   lapply(R6_objects, do("member_function"), args)
# which does R6_objects[[i]]$member_function(args) for each R6 object
do <- function(name, ...) {
  function (x, ...) {
    x[[name]](...)
  }
}

misc_module <- module(module,
                      check_tf_version,
                      member,
                      node_type,
                      tf_float,
                      fl,
                      to_shape,
                      is_scalar,
                      flatten,
                      do)

# check dimensions of arguments to ops, and return the maximum dimension
check_dims <- function (..., target_dim = NULL) {

  # coerce args to greta arrays
  elem_list <- list(...)
  elem_list <- lapply(elem_list, as.greta_array)

  # dimensions of each
  dim_list <- lapply(elem_list, dim)

  # as text, for printing
  dims_paste <- vapply(dim_list, paste, '', collapse= 'x')
  dims_text <- paste(dims_paste, collapse = ', ')

  # which are scalars
  scalars <- vapply(elem_list, is_scalar, FALSE)

  # if more than one is non-scalar, need to check them
  if (sum(!scalars) > 1) {

    match_first <- vapply(dim_list,
                          identical,
                          FUN.VALUE = FALSE,
                          dim_list[[1]])

    # if they're non-scalar, but have the same dimensions, that's fine too
    if (!all(match_first)) {

      # otherwise it's not fine
      msg <- sprintf('incompatible dimensions: %s',
                     dims_text)
      stop (msg, call. = FALSE)

    }
  }

  # if there's a target dimension, make sure they all match it
  if (!is.null(target_dim)) {

    # make sure it's 2D
    if (length(target_dim) == 1)
      target_dim <- c(target_dim, 1)

    target_dim <- as.integer(target_dim)

    # if they are all scalars, that's fine too
    if (!all(scalars)) {

      # check all arguments against this
      matches_target <- vapply(dim_list,
                               identical,
                               FUN.VALUE = FALSE,
                               target_dim)

      # error if not
      if (!all(matches_target)) {
        stop (sprintf('array dimensions should be %s, but input dimensions were %s',
                      paste(target_dim, collapse = 'x'),
                      dims_text),
              call. = FALSE)
      }

    }

    output_dim <- target_dim

  } else {

    # otherwise, find the correct output dimension
    output_dim <- do.call(pmax, dim_list)

  }

  output_dim

}

# check truncation for different distributions
check_positive <- function (truncation) {
  if (truncation[1] < 0) {
    stop ("lower bound must be 0 or higher",
          call. = FALSE)
  }
}

check_unit <- function (truncation) {
  if (truncation[1] < 0 | truncation[2] > 1) {
    stop ("lower and upper bounds must be between 0 and 1",
          call. = FALSE)
  }
}

# check whether the function calling this is being used as the 'family' argument
# of another modelling function
check_in_family <- function (function_name) {
  greta_function <- get(function_name, envir = asNamespace("greta"))
  family <- parent.frame(2)$family
  if (!is.null(family) && identical(family, greta_function)) {
    msg <- paste0("It looks like you're using greta's ", function_name,
                  " function in the family argment of another model.",
                  " Maybe you want to use 'family = stats::", function_name,
                  "' instead?")
    stop (msg, call. = FALSE)
  }
}

checks_module <- module(check_dims,
                        check_unit,
                        check_positive,
                        check_in_family)

# convert an array to a vector row-wise
flatten_rowwise <- function (array) {
  dim <- dim(array)
  array <- aperm(array, rev(seq_along(dim)))
  dim(array) <- NULL
  array
}

# convert an vector to an array row-wise
unflatten_rowwise <- function (array, dim) {

  array <- as.array(array)
  # if any dim has length 1, make it a column vector
  if (length(dim) == 1)
    dim <- c(dim, 1)

  dim(array) <- rev(dim)
  array <- aperm(array, rev(seq_along(dim)))
  dim(array) <- dim
  array
}

# create an array with the same dimensions as tensor and fill it with
# consecutive increasing integers in python order
dummy <- function (dims) {
  vec <- seq_len(prod(dims)) - 1
  unflatten_rowwise(vec, dims)
}

dummy_array_module <- module(flatten_rowwise,
                             unflatten_rowwise,
                             dummy)

# given a base colour, return a function taking a value between 0 and 1 and
# returning a colour linearly interpolated between black, the colour and white,
# so that values close to 0.5 match the base colour, values close to 0 are
# nearer black, and values close to 1 are nearer white
palettize <- function (base_colour) {
  pal <- colorRampPalette(c('#000000', base_colour, '#ffffff'))
  function (val) {
    stopifnot(val > 0 & val < 1)
    cols <- pal(1001)
    cols[round(val * 1000 + 1)]
  }
}

# colour scheme for plotting
greta_col <- function (which = c('main',
                                 'dark',
                                 'light',
                                 'lighter',
                                 'super_light')) {
  which <- match.arg(which)
  pal <- palettize('#996bc7')
  switch (which,
          dark = pal(0.45),  #45%
          main = pal(0.55),  #55%
          light = pal(0.65),  #65%ish
          lighter = pal(0.85),  #85%ish
          super_light = pal(0.95))  #95%ish
}

colour_module <- module(palettize,
                       greta_col)

# look in the environment specified by env, and return a named list of all greta
# arrays in that environment
all_greta_arrays <- function (env = parent.frame(),
                              include_data = TRUE) {

  # all objects in that environment as a named list
  all_object_names <- ls(envir = env)
  all_objects <- lapply(all_object_names, get, envir = env)
  names(all_objects) <- all_object_names

  # find the greta arrays
  is_greta_array <- vapply(all_objects,
                           inherits,
                           "greta_array",
                           FUN.VALUE = FALSE)
  all_arrays <- all_objects[is_greta_array]

  # optionally strip out the data arrays
  if (!include_data) {

    is_data <- vapply(all_arrays,
                      function (x) inherits(x$node, 'data_node'),
                      FUN.VALUE = FALSE)
    all_arrays <- all_arrays[!is_data]

  }

  all_arrays

}

# evaluate expressions (dag density or gradient), capturing numerical errors
# like matrix inversions as bad samples, and erroring otherwise
cleanly <- function (expr) {

  res <- tryCatch(expr, error = function (e) e)

  # if it errored
  if (inherits(res, 'error')) {

    numerical_messages <- c("is not invertible",
                            "Cholesky decomposition was not successful")

    numerical_errors <- vapply(numerical_messages,
                               grepl,
                               res$message,
                               FUN.VALUE = 0) == 1

    # if it was just a numerical error, quietly return a bad value
    if (any(numerical_errors))
      res <- NA
    else
      stop ("greta hit a tensorflow error:\n\n", res, call. = FALSE)

  }

  res

}

# prepare a matrix of draws and return as an mcmc object
#' @noRd
#' @importFrom coda mcmc
prepare_draws <- function (draws) {
  draws_df <- data.frame(draws)
  draws_df <- na.omit(draws_df)
  coda::mcmc(draws_df)
}

build_sampler <- function (initial_values, sampler, model) {
  sampler$class$new(initial_values,
                    model,
                    sampler$parameters)
}

# unlist and flatten a list of arrays to a vector row-wise
unlist_tf <- function (x) {
  # flatten each element row-wise and concatenate
  x <- lapply(x, flatten_rowwise)
  do.call(c, x)
}

# relist a vector into a list of arrays row-wise
relist_tf <- function (x, list_template) {

  # get expected dimensions of arrays and number of elements
  dims <- lapply(list_template, dim)
  lengths <- vapply(dims, prod, 1)
  runs <- rep(seq_along(lengths), lengths)

  # chop up vector into shorter lengths
  vectors <- split(x, runs)
  names(vectors) <- NULL

  # loop through vectors coercing into arrays
  list <- mapply(unflatten_rowwise, vectors, dims, SIMPLIFY = FALSE)
  names(list) <- names(list_template)
  list

}

sampler_utils_module <- module(all_greta_arrays,
                               cleanly,
                               build_sampler,
                               prepare_draws,
                               unlist_tf,
                               relist_tf)

flat_to_chol <- function (x, dim, correl = FALSE) {

  dimfun <- function (elem_list)
    dim

  fun <- ifelse(correl,
                tf_flat_to_chol_correl,
                tf_flat_to_chol)

  # sum the elements
  op('flat_to_chol',
     x,
     operation_args = list(dims = dim),
     tf_operation = fun,
     dimfun = dimfun)

}

chol_to_symmetric <- function (L) {

  dimfun <- function (elem_list)
    dim(elem_list[[1]])

  # sum the elements
  op('chol_to_symmetric',
     L,
     tf_operation = tf_chol_to_symmetric,
     dimfun = dimfun)

}

greta_array_operations_module <- module(flat_to_chol,
                                        chol_to_symmetric)

# utilities to export via .internals
utilities_module <- module(misc = misc_module,
                           dummy_arrays = dummy_array_module,
                           greta_array_operations = greta_array_operations_module,
                           samplers = sampler_utils_module,
                           checks = checks_module,
                           colours = colour_module)
