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

    tf_version <- suppressMessages(tf$`__version__`)
    is_tf_available <- utils::compareVersion("1.14.0", tf_version) == 0

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
has_batch <- function(x) is.na(dim(x)[1])

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
      "{.code reticulate::conda_create(envname = 'greta-env', \\
      python_version = '3.7')}",
      "Then:",
      "{.code reticulate::conda_install(envname = 'greta-env',
      packages = c('numpy==1.16.4', 'tensorflow-probability==0.7.0',
      'tensorflow==1.14.0'))}",
      "Then, restart R, and load {.pkg greta} with: {.code library(greta)}",
      "If this does not work, lodge an issue on github at:",
      "{.url https://github.com/greta-dev/greta/issues/new}"
    )
  )
}

timeout_install_msg <- function(timeout, py_error = NULL){
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
    "{.code reticulate::conda_create(envname = 'greta-env', \\
        python_version = '3.7')}",
    "Then:",
    "{.code reticulate::conda_install(envname = 'greta-env', \\
        packages = c('numpy==1.16.4', 'tensorflow-probability==0.7.0', \\
        'tensorflow==1.14.0'))}",
    "Then, restart R, and load {.pkg greta} with:",
    "{.code library(greta)}"
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
    # if it has a version and ideal version
    if (!is.null(version) & !is.null(ideal_version)){
      version_chr <- paste0(version)
      version_match <- compareVersion(version_chr, ideal_version) == 0

      if (version_match){
        cli::cli_process_done(
          msg_done = "{.pkg {software_name}} (version {version}) available"
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

compare_version_vec <- function(current,ideal){
  compareVersion(
      paste0(current),
      ideal
    )
}


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
                              ideal_version = "3.7",
                              software_name = "python")

  check_if_software_available(software_available = have_tf(),
                              version = version_tf(),
                              ideal_version = "1.14.0",
                              software_name = "TensorFlow")

  check_if_software_available(software_available = have_tfp(),
                              version = version_tfp(),
                              ideal_version = "0.7.0",
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
        "tf",
        "tfp"
      ),
      current = c(
        paste0(reticulate::py_version()),
        paste0(version_tf()),
        paste0(version_tfp())
      ),
      ideal = c(
        "3.7",
        "1.14.0",
        "0.7.0"
      )
    )

    software_version$match <- c(
      compareVersion(software_version$current[1], software_version$ideal[1]) == 0,
      compareVersion(software_version$current[2], software_version$ideal[2]) == 0,
      compareVersion(software_version$current[3], software_version$ideal[3]) == 0
    )

    if (all(software_version$match)){
      check_tf_version("none")
      cli::cli_alert_info("{.pkg greta} is ready to use!")
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
