# an array of random standard normals with the specificed dims
# e.g. randn(3, 2, 1)
randn <- function(...) {
  dim <- c(...)
  array(stats::rnorm(prod(dim)), dim = dim)
}

# ditto for standard uniforms
randu <- function(...) {
  dim <- c(...)
  array(stats::runif(prod(dim)), dim = dim)
}

# create a variable with the same dimensions as as_data(x)
as_variable <- function(x) {
  x <- as_2d_array(x)
  variable(dim = dim(x))
}


# check a greta operation and the equivalent R operation give the same output
# e.g. check_op(sum, randn(100, 3))
check_op <- function(op, a, b, greta_op = NULL,
                     other_args = list(),
                     tolerance = 1e-3,
                     only = c("data", "variable", "batched"),
                     relative_error = FALSE) {
  greta_op <- greta_op %||% op

  r_out <- run_r_op(op, a, b, other_args)

  for (type in only) {
    # compare with ops on data greta arrays
    greta_out <- run_greta_op(greta_op, a, b, other_args, type)
    compare_op(r_out, greta_out, tolerance, relative_error = relative_error)
  }
}

compare_op <- function(r_out, greta_out, tolerance = 1e-4, relative_error = FALSE) {
  if (relative_error){
    difference <- as.vector(abs(r_out - greta_out) / abs(r_out))
  } else if (!relative_error){
    difference <- as.vector(abs(r_out - greta_out))
  }
  difference_lt_tolerance <- difference < tolerance
  are_all_true <- all(difference_lt_tolerance)
  are_all_true
  testthat::expect_true(are_all_true)
}

run_r_op <- function(op, a, b, other_args) {
  arg_list <- list(a)
  if (!missing(b)) {
    arg_list <- c(arg_list, list(b))
  }
  arg_list <- c(arg_list, other_args)
  do.call(op, arg_list)
}

run_greta_op <- function(greta_op, a, b, other_args,
                         type = c("data", "variable", "batched")) {
  type <- match.arg(type)

  converter <- switch(type,
                      data = as_data,
                      variable = as_variable,
                      batched = as_variable
  )

  g_a <- converter(a)

  arg_list <- list(g_a)
  values <- list(g_a = a)

  if (!missing(b)) {
    g_b <- converter(b)
    arg_list <- c(arg_list, list(g_b))
    values <- c(values, list(g_b = b))
  }

  arg_list <- c(arg_list, other_args)
  out <- do.call(greta_op, arg_list)

  if (type == "data") {
    # data greta arrays should provide their own values
    result <- calculate(out, values = list())[[1]]
  } else if (type == "variable") {
    result <- grab_via_free_state(out, values)
  } else if (type == "batched") {
    result <- grab_via_free_state(out, values, batches = 3)
  } else {
    result <- calculate(out, values = values)[[1]]
  }

  result
}

# get the value of the target greta array, by passing values for the named
# variable greta arrays via the free state parameter, optionally with batches
grab_via_free_state <- function(target, values, batches = 1) {
  dag <- dag_class$new(list(target))
  inits <- do.call(initials, values)
  inits_flat <- prep_initials(inits, 1, dag)[[1]]
  if (batches > 1) {
    inits_list <- replicate(batches, inits_flat, simplify = FALSE)
    inits_flat <- do.call(rbind, inits_list)
    vals <- dag$trace_values(inits_flat)[1, ]
  } else {
    vals <- dag$trace_values(inits_flat)
  }
  array(vals, dim = dim(target))
}

expect_ok <- function(expr) {
  testthat::expect_error(expr, NA)
}

is.greta_array <- function(x) { # nolint
  inherits(x, "greta_array")
}
