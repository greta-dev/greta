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
    is_tfp_available <- utils::compareVersion("0.15.0", tfp_version) <= 0

  }

  return(is_tfp_available)

}

#' @importFrom reticulate py_module_available
have_tf <- function() {
  is_tf_available <- reticulate::py_module_available("tensorflow")

  if (is_tf_available) {

    tf_version <- suppressMessages(tf$`__version__`)
    is_tf_available <- utils::compareVersion("2.9.0", tf_version) <= 0

  }

  return(is_tf_available)

}

version_tf <- function(){
  if (have_tf()) {
    tf$`__version__`
  } else {
    NULL
  }
}

version_tfp <- function(){
  if (have_tfp()) {
    tfp$`__version__`
  } else {
    NULL
  }
}

# helper for *apply statements on R6 objects
member <- function(x, method) {
  eval(parse(text = glue::glue("x${method}")))
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

# get the tensor for the batch size in the dag recently defined (since it's
# not always possible to pass the dag in)
get_batch_size <- function() {
  greta_stash$batch_size
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
  # if n is >= 2^31 then the vector is represented as a double, and causes
  # a bunch of TF mechanics to break as they require integers
  sample.int(
    n = 2^30,
    size = 1
    )
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
  one_dimensional <- n_dim(x) == 1
  if (one_dimensional) {
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
  not_1d <- n_dim(x) > 1
  if (not_1d) {
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
  is_2_by_1 <- length(dims) == 2 && dims[2] == 1L
  if (is_2_by_1) {
    x <- as.vector(x)
  }
  x
}

# where x is a tensor with no batch dimension, and y is a tensor with a batch
# dimension, tile x to have first dimension matching y (dimension determined at
# run time)
expand_to_batch <- function(x, y) {
  batch_size <- tf$shape(y)[[0]]
  ndim <- n_dim(x)
  tf$tile(x, c(batch_size, rep(1L, ndim - 1)))
}

# does this tensor have a batch dimension (of unknown size) as its first
# dimension?
has_batch <- function(x) is.na(dim(x)[1])

# given a list of tensors, if none or all of them have a batch dimension, return
# the list. If any (but not all) of them has a batch dimension, tile the
# unbatched ones (which are assumed to have first dimension 1) to match the
# dimension of the batched ones dimension
match_batches <- function(values) {
  is_tensor <- vapply(values, inherits, "tensorflow.tensor", FUN.VALUE = FALSE)

  values_mutable <- values[is_tensor]

  have_batches <- vapply(values_mutable, has_batch, FUN.VALUE = TRUE)

  any_but_not_all_have_batch_and_dim <- !all(have_batches) & any(have_batches)
  if (any_but_not_all_have_batch_and_dim) {
    # tile the others to match the batch
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
  has_2d <- length(dim) == 2
  is_2_by_1 <- has_2d && dim[2] == 1L
  if (is_2_by_1) {
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
      cli::cli_abort(
        "Invalid colour: {colour}"
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
  is_greta_array <- are_greta_array(all_objects)

  all_arrays <- all_objects[is_greta_array]

  # optionally strip out the data arrays
  if (!include_data) {
    is_data <- vapply(all_arrays,
                      function(x) is.data_node(get_node(x)),
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

  check_for_errors(res)

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

build_sampler <- function(initial_values, sampler, model, seed = get_seed(),
                          compute_options) {
  ## TF1/2 retracing
  ## This is where a retracing warning happens
  ## in mcmc
  sampler$class$new(initial_values,
                    model,
                    sampler$parameters,
                    seed = seed,
                    compute_options = compute_options
  )
}

# unlist and flatten a list of arrays to a vector row-wise
unlist_tf <- function(x) {
  # flatten each element row-wise and concatenate
  x <- lapply(x, flatten_rowwise)
  do.call(c, x)
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
    name <- glue::glue("{name}[{mid_text}]")
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
get_model_info <- function(draws) {

  check_if_greta_mcmc_list(draws)

  model_info <- attr(draws, "model_info")

  check_if_model_info(model_info)

  model_info
}

sampler_utils_module <- module(
  all_greta_arrays,
  cleanly,
  build_sampler,
  prepare_draws,
  unlist_tf,
  get_indices_text,
  flatten_trace,
  get_model_info
)

# TF1/2 check remove?
# Is this still needed with the new `tf_function` from TF2?
# I cannot actually currently see uses of `as_tf_function ` in the code
# base currently
# convert a function on greta arrays into a function on corresponding tensors,
# given the greta arrays for inputs. When executed, this needs to be wrapped in
# dag$on_graph() to get the tensors connected up with the rest of the graph
# NOTE: Could use this as a way of getting the functions we need from greta
# we could use this as a way of returning a function that TF recognises
# as a function tensorflow function that returns tensors
as_tf_function <- function(r_fun, ...) {

  # run the operation on isolated greta arrays, so nothing gets attached to the
  # model real greta arrays in dots
  # creating a fake greta array
  ga_dummies <- lapply(list(...), dummy_greta_array)

  # now run the function on these completely separate ones
  ga_out <- do.call(r_fun, ga_dummies)
  ga_out

  # a function that will act on TF things
  function(...) {
    tensor_inputs <- list(...)

    # if any of these are shapeless, make them into greta scalars (3D)
    tensor_inputs <- lapply(
      tensor_inputs,
      function(x) {
        empty_dim <- identical(dim(x), list())
        if (empty_dim) {
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

    # TF1/2 check remove
      # `get_default_graph()` doesn't work with either eager execution or
      # `tf.function`.
    # use the default graph, so that it can be overwritten when this is called?
    # alternatively fetch from above, or put it in greta_stash?
    # sub_dag$tf_graph <- tf$compat$v1$get_default_graph()
    sub_tfe <- sub_dag$tf_environment

    # pass on the batch size, used when defining data
    #  - how many chains or whatever to use
    # get the batch size from the input tensors - it should be written to the
    # stash by the main dag - but only if a main dag is defined. What about in calculate?
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
    # trying to not get them to use placeholders
    # (TF can have data as a placeholder or a constant)
    # (using a constant is expensive, normally)
    greta_stash$data_as_constants <- TRUE
    # TODO explore changin this to previous state
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
  colours = colour_module
)

# remove empty strings
base_remove_empty_string <- function(string){
  string[string != ""]
}


other_install_fail_msg <- function(error_passed){
  # drop ""
  error_passed <- base_remove_empty_string(error_passed)
  cli::format_error(
    message = c(
      "Stopping as installation of {.pkg greta} dependencies failed",
      "An error occured:",
      "{.code {cat(error_passed)}}",
      "You can perform the entire installation manually with:",
      "{.code reticulate::install_miniconda()}",
      "Then:",
      "{.code reticulate::conda_create(envname = 'greta-env-tf2', \\
      python_version = '3.8')}",
      "Then:",
      "{.code reticulate::py_install(
        packages = c(
          'numpy',
          'tensorflow',
          'tensorflow-probability'
          ),
        pip = TRUE
        )}",
      "Then, restart R, and load {.pkg greta} with: {.code library(greta)}",
      "If this does not work, lodge an issue on github at:",
      "{.url https://github.com/greta-dev/greta/issues/new}"
    )
  )
}

timeout_install_msg <- function(timeout = 5, py_error = NULL){
  msg <- c(
    "Stopping as installation of {.pkg greta} dependencies took longer than \\
        {timeout} minutes",
    "You can increase the timeout time by increasing the {.arg timeout} \\
        argument.",
    "For example, to wait 5 minutes:",
    "{.code install_greta_deps(timeout = 5)}",
    "Alternatively, you can perform the entire installation with:",
    "{.code reticulate::install_miniconda()}",
    "Then:",
    "{.code reticulate::conda_create(envname = 'greta-env-tf2', \\
        python_version = '3.8')}",
    "Then:",
    "{.code reticulate::py_install(
        packages = c(
          'numpy',
          'tensorflow',
          'tensorflow-probability'
          ),
        pip = TRUE
        )}",
    "Then, restart R, and load {.pkg greta} with: {.code library(greta)}"
  )

  if (nchar(py_error) == 0) {
    py_error <- NULL
  }

  if (is.null(py_error)){
    cli::format_error(
      message = msg
    )
  } else {
    msg <- c(
      msg,
      "Additionally, the following error appeared:",
      "{cat({py_error})}"
    )
    cli::format_error(
      message = msg
    )
  }
}

is_DiagrammeR_installed <- function(){
  requireNamespace("DiagrammeR", quietly = TRUE)
}

check_if_software_available <- function(software_available,
                                        version = NULL,
                                        ideal_version = NULL,
                                        software_name){

  cli::cli_process_start("checking if {.pkg {software_name}} available")
  # if the software is detected

  if (!software_available) {
    cli::cli_process_failed(
      msg_failed = "{.pkg {software_name}} not available"
    )
  }

  if (software_available) {

    if (is.null(ideal_version) & !is.null(version)){
      cli::cli_process_done(
        msg_done = "{.pkg {software_name}} (v{version}) available"
      )
    }

    # if it has a version and ideal version
    has_ideal_version <- !is.null(version) & !is.null(ideal_version)
    if (has_ideal_version){
      version_chr <- paste0(version)
      version_match <- compareVersion(version_chr, ideal_version) == 0

      if (version_match){
        cli::cli_process_done(
          msg_done = "{.pkg {software_name}} (v{version}) available"
        )
      }
      if (!version_match){
        cli::cli_process_failed(
          msg_failed = "{.pkg {software_name}} available, \\
          however {.strong {ideal_version}} is needed and \\
          {.strong {version}} was detected"
        )
      }
      # if there is no version for the software
    } else if (is.null(version)){
      cli::cli_process_done(
        msg_done = "{.pkg {software_name}} available"
      )
    }
  }
}

compare_version_vec <- Vectorize(
    FUN = compareVersion,
    vectorize.args = "b",
    SIMPLIFY = TRUE
  )

#' Greta Situation Report
#'
#' This checks if Python, Tensorflow, Tensorflow Probability, and the greta
#'   conda environment are available, and also loads and initialises python
#'
#' @return Message if greta is ready to use
#' @export
#'
#' @examples
#' \dontrun{
#' greta_sitrep()
#' }
greta_sitrep <- function(){

  check_if_software_available(software_available = have_python(),
                              version = reticulate::py_version(),
                              software_name = "python")

  check_if_software_available(software_available = have_tf(),
                              version = version_tf(),
                              software_name = "TensorFlow")

  check_if_software_available(software_available = have_tfp(),
                              version = version_tfp(),
                              software_name = "TensorFlow Probability")

  check_if_software_available(software_available = have_greta_conda_env(),
                              software_name = "greta conda environment")

  software_available <- c(
    python = have_python(),
    tf = have_tf(),
    tfp = have_tfp(),
    greta_env = have_greta_conda_env()
  )

  if (!all(software_available)) {
    check_tf_version("warn")
  } else if (all(software_available)) {
    software_version <- data.frame(
      software = c(
        "python",
        "tfp",
        "tf"
      ),
      current = c(
        paste0(reticulate::py_version()),
        paste0(version_tf()),
        paste0(version_tfp())
      ),
      # versions must be at least this version
      ideal = c(
        "3.8",
        "2.15.0",
        "0.23.0"
      )
    )
    software_version$match <- c(
      compareVersion(software_version$current[1], software_version$ideal[1]) >= 0,
      compareVersion(software_version$current[2], software_version$ideal[2]) >= 0,
      compareVersion(software_version$current[3], software_version$ideal[3]) >= 0
    )

    if (all(software_version$match)){
      check_tf_version("none")
      cli::cli_alert_info("{.pkg greta} is ready to use!",
                          wrap = TRUE)
    } else {
      check_tf_version("warn")
    }

  }

}

# adapted from https://github.com/rstudio/tensorflow/blob/main/R/utils.R
is_mac_arm64 <- function() {
  if (nzchar(Sys.getenv("GRETA_M1_MESSAGE_TESTING"))) {
    return(TRUE)
  }
  si <- Sys.info()
  is_darwin <- si[["sysname"]] == "Darwin"
  is_arm64 <- si[["machine"]] == "arm64"
  is_darwin && is_arm64
}

read_char <- function(path){
  trimws(readChar(path, nchars = file.info(path)$size))
}

create_temp_file <- function(path){
  file_path <- tempfile(path, fileext = ".txt")
  file.create(file_path)
  return(file_path)
}

#' @title Set GPU or CPU usage
#' @name gpu_cpu
#' @description These functions set the use of CPU or GPU inside of greta. They
#'   simply return either "GPU" or "CPU", but in the future may handle more
#'   complexity. These functions are passed to `compute_options` inside of a few
#'   functions: [mcmc()], [opt()], and [calculate()].
#' @export
gpu_only <- function(){
  "GPU"
}

#' @rdname gpu_cpu
#' @export
cpu_only <- function(){
  "CPU"
}

compute_text <- function(n_cores, compute_options){
  ifelse(
    test = n_cores == 1,
    yes = "each on 1 core",
    no = ifelse(
      test = compute_options == "CPU",
      yes = glue::glue("on up to {n_cores} {compute_options} cores"),
      # "on GPU"
      no = glue::glue("on {compute_options}")
    )
  )
}

connected_to_draws <- function(dag, mcmc_dag) {
  names(dag$node_list) %in% names(mcmc_dag$node_list)
}



is_using_gpu <- function(x){
  x == "GPU"
}

is_using_cpu <- function(x){
  x == "CPU"
}

`%||%` <- function(x, y) if (is.null(x)) y else x

message_if_using_gpu <- function(compute_options){
  if (is_using_gpu(compute_options)) {
    if (getOption("greta_gpu_message") %||% TRUE){
      cli::cli_inform(
        c(
          "NOTE: When using GPU, the random number seed may not always be \\
          respected (results may not be fully reproducible).",
          "For more information, see details of the {.code compute_options} \\
          argument in {.code ?calculate}.",
          "You can turn off this message with:",
          "{.code options(greta_gpu_message = FALSE)}"
        )
      )
    }
  }
}

n_dim <- function(x) length(dim(x))
is_2d <- function(x) n_dim(x) == 2

is.node <- function(x, ...){
  inherits(x, "node")
}

is.data_node <- function(x, ...){
  inherits(x, "data_node")
}

is.distribution_node <- function(x, ...){
  inherits(x, "distribution_node")
}

is.variable_node <- function(x, ...){
  inherits(x, "variable_node")
}

is.greta_model <- function(x, ...){
  inherits(x, "greta_model")
}

is.unknowns <- function(x, ...){
  inherits(x, "unknowns")
}

is.initials <- function(x, ...){
  inherits(x, "initials")
}

node_type_colour <- function(type){

  switch_cols <- switch(
    type,
    variable = cli::col_red(type),
    data = cli::col_green(type),
    operation = cli::col_cyan(type),
    distribution = cli::col_yellow(type)
    )

  switch_cols
}

extract_unique_names <- function(x){
  vapply(
    X = x,
    FUN = member,
    "unique_name",
    FUN.VALUE = character(1)
  )
}

are_identical <- function(x, y){
  vapply(
    X = x,
    FUN = identical,
    FUN.VALUE = logical(1),
    y
  )
}

#' Vectorised is.null
#'
#' @param x list of things that may contain NULL values
#'
#' @return logical
#' @export
#'
#' @examples
#' is.null(list(NULL, NULL, 1))
#' are_null(list(NULL, NULL, 1))
#' are_null(list(NULL, NULL, NULL))
#' are_null(list(1, 2, 3))
#' is.null(list(1, 2, 3))
are_null <- function(x){
  vapply(
    x,
    is.null,
    FUN.VALUE = logical(1)
    )
}

are_greta_array <- function(x){
  vapply(
    x,
    is.greta_array,
    FUN.VALUE = logical(1)
  )
}

have_distribution <- function(x){
  vapply(
    x,
    has_distribution,
    FUN.VALUE = logical(1)
  )
}

is_windows <- function() {
  identical(.Platform$OS.type, "windows")
}

is_mac <- function() {
  as.logical(Sys.info()["sysname"] == "Darwin")
}

is_linux <- function() {
  identical(tolower(Sys.info()[["sysname"]]), "linux")
}

os_name <- function(){
  os <- c(
    windows = is_windows(),
    mac = is_mac(),
    linux = is_linux()
  )
  names(which(os))
}

# semantic version finder
closest_version <- function(current, available){

  available <- sort(available)
  not_available <- !(current %in% available)

  current_gt_available <- all(current > available)
  current_lt_available <- all(current < available)
  current_btn_available <- any(current > available) && any(current < available)

  pick_largest <- not_available && current_gt_available
  pick_smallest <- not_available && current_lt_available

  if (pick_largest) {
    closest <- max(available)
  }

  if (pick_smallest) {
    closest <- min(available)
  }

  if (current_btn_available){
    version_gt <- current > available
    closest <- max(available[version_gt])
  }

  return(closest)

}

outside_version_range <- function(provided, range) {
  version_num <- numeric_version(provided)
  above_range <- all(version_num > range)
  below_range <- all(version_num < range)
  outside_range <- above_range || below_range
  outside_range
}

pretty_dim <- function(x) paste0(dim(x), collapse = "x")

are_initials <- function(x){
  vapply(
    X = x,
    FUN = is.initials,
    FUN.VALUE = logical(1)
    )
}
