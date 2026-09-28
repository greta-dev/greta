# a one-parameter model over mutable data, which is what most of these tests
# want: the likelihood is tight enough that the log density has to move when
# the data does
mutable_model <- function(values = rep(0, 5)) {
  x <- as_data_mutable(values)
  z <- normal(0, 1)
  distribution(x) <- normal(z, 1)
  list(x = x, z = z, model = model(z))
}

log_prob_at <- function(dag, free_state = matrix(0.5, 1, 1)) {
  as.numeric(dag$tf_log_prob_function_adjusted(free_state))
}

test_that("declaring data mutable backs it with a tf$Variable that follows it", {
  skip_if_not(check_tf_version())

  fixture <- mutable_model(rep(1, 5))
  m <- fixture$model
  key <- get_node(fixture$x)$unique_name

  # the variable is what makes the data mutable: the graph holds a reference to
  # it, so its value is the whole mechanism, and checking it needs no sampler
  variable_value <- function() {
    as.numeric(m$dag$data_variables[[key]]$numpy())
  }

  expect_equal(variable_value(), rep(1, 5))

  data_values(m, fixture$x) <- rep(7, 5)

  expect_equal(variable_value(), rep(7, 5))
  expect_equal(as.numeric(get_node(fixture$x)$value()), rep(7, 5))
})

test_that("data can be swapped without retracing the log prob function", {
  skip_if_not(check_tf_version())

  fixture <- mutable_model()
  dag <- fixture$model$dag
  traces <- function() {
    dag$tf_log_prob_function$experimental_get_tracing_count()
  }

  before <- log_prob_at(dag)
  traces_before <- traces()

  dag$set_data_value(fixture$x, as.matrix(rep(3, 5)))

  # the density follows the new data, and nothing was retraced to do it:
  # rebuilding the graph per swap is what made the Geweke checks slow,
  # greta-dev/greta#739
  after <- log_prob_at(dag)
  expect_false(identical(after, before))
  expect_true(is.finite(after))
  expect_identical(traces(), traces_before)
})

test_that("set_data_value() errors informatively on bad input", {
  skip_if_not(check_tf_version())

  fixture <- mutable_model()
  dag <- fixture$model$dag
  x <- fixture$x
  z <- fixture$z

  # not data
  expect_snapshot(error = TRUE, dag$set_data_value(z, 1))

  # right node, wrong shape: a variable's shape is fixed
  expect_snapshot(error = TRUE, dag$set_data_value(x, rep(1, 3)))
})

test_that("swapped data reaches the sampler, not just the log prob", {
  skip_if_not(check_tf_version())

  # the posterior for z sits wherever the data is, but not so tightly that the
  # chain cannot walk there: sd 1 over 10 observations puts the posterior sd
  # near 0.32, and the swap below moves the mean about six of those
  x <- as_data_mutable(rep(0, 10))
  z <- normal(0, 10)
  distribution(x) <- normal(z, 1)
  m <- model(z)

  draws <- mcmc(m, chains = 1, warmup = 200, n_samples = 100, verbose = FALSE)
  before <- mean(as.matrix(draws))

  # extra_samples() goes through the sampler's own traced function, not
  # tf_log_prob_function, so this is the path that would silently keep sampling
  # against stale data. Keep the move modest: extra_samples() resumes with a
  # step size tuned for the old posterior, and a jump of tens of posterior sds
  # freezes the chain at 100% rejection, which looks exactly like stale data
  m$dag$set_data_value(x, as.matrix(rep(2, 10)))

  # the deterministic half: the variable the already-traced sampler reads now
  # holds the new data, whatever the chain below then makes of it
  key <- get_node(x)$unique_name
  expect_equal(as.numeric(m$dag$data_variables[[key]]$numpy()), rep(2, 10))

  after_draws <- extra_samples(draws, n_samples = 300, verbose = FALSE)
  after <- mean(tail(as.matrix(after_draws), 100))

  # measured over 8 runs: before spans -0.05 to 0.11, after 1.90 to 2.01
  expect_lt(abs(before), 0.5)
  expect_gt(after, 1)
})

test_that("setting a node value and rebuilding still changes the graph", {
  skip_if_not(check_tf_version())

  fixture <- mutable_model()
  dag <- fixture$model$dag
  before <- log_prob_at(dag)

  # the older way of changing data, which set_data_value() has to keep working:
  # set the node's value, then rebuild. If the rebuild does not refresh the
  # variable it silently keeps computing on the original data
  get_node(fixture$x)$value(as.matrix(rep(3, 5)))
  dag$define_tf_log_prob_function()

  expect_false(identical(log_prob_at(dag), before))
})

test_that("a rebuild does not revert a value set with set_data_value()", {
  skip_if_not(check_tf_version())

  fixture <- mutable_model()
  dag <- fixture$model$dag

  dag$set_data_value(fixture$x, as.matrix(rep(3, 5)))
  swapped <- log_prob_at(dag)

  # rebuilding refreshes the variables from the node values, so set_data_value()
  # has to write through to the node or its value is silently reverted here
  dag$define_tf_log_prob_function()

  expect_identical(log_prob_at(dag), swapped)
  expect_true(is.finite(swapped))
})

test_that("data_values<- swaps data on a model", {
  skip_if_not(check_tf_version())

  fixture <- mutable_model(rep(1, 5))
  m <- fixture$model
  before <- log_prob_at(m$dag)

  data_values(m, fixture$x) <- rep(3, 5)

  expect_false(identical(log_prob_at(m$dag), before))
  expect_true(is.finite(log_prob_at(m$dag)))
})

test_that("data_values<- errors informatively", {
  skip_if_not(check_tf_version())

  fixture <- mutable_model(rep(1, 5))
  m <- fixture$model
  x <- fixture$x
  z <- fixture$z

  expect_snapshot(error = TRUE, data_values(m, x) <- rep(1, 3))
  expect_snapshot(error = TRUE, data_values(m, z) <- 1)
  not_a_model <- "not a model"
  expect_snapshot(error = TRUE, data_values(not_a_model, x) <- rep(1, 5))

  # declared mutable, but belonging to a different model
  elsewhere <- mutable_model(rep(1, 5))$x
  expect_snapshot(error = TRUE, data_values(m, elsewhere) <- rep(1, 5))
})

test_that("a replacement has to clear the same bar as_data() sets", {
  skip_if_not(check_tf_version())

  fixture <- mutable_model(rep(1, 5))
  m <- fixture$model
  x <- fixture$x

  # assigned unchecked, these reach the tf$Variable and turn every density
  # downstream into NA, or surface as a Python ValueError
  expect_snapshot(error = TRUE, data_values(m, x) <- c(1, NA, 3, 4, 5))
  expect_snapshot(error = TRUE, data_values(m, x) <- c(1, Inf, 3, 4, 5))
  expect_snapshot(error = TRUE, data_values(m, x) <- letters[1:5])
})

test_that("a data frame is a valid replacement for data frame data", {
  skip_if_not(check_tf_version())

  wide <- as_data_mutable(data.frame(a = rnorm(5), b = rnorm(5)))
  z <- normal(0, 1)
  distribution(wide) <- normal(z, 1)
  m <- model(z)

  replacement <- data.frame(a = rep(2, 5), b = rep(3, 5))
  data_values(m, wide) <- replacement

  expect_equal(get_node(wide)$value(), unname(as.matrix(replacement)))
})

test_that("a swap reaches opt() and calculate(), not just mcmc()", {
  skip_if_not(check_tf_version())

  x <- as_data_mutable(rep(1, 5))
  doubled <- x * 2
  z <- normal(0, 1)
  obs <- as_data(rep(0, 5))
  distribution(obs) <- normal(z + x, 1)
  m <- model(z)

  data_values(m, x) <- rep(3, 5)

  # mode of normal(0, 1) against five observations of 0 with mean z + 3
  expect_equal(opt(m)$par$z, -2.5, tolerance = 1e-4)
  expect_equal(as.numeric(calculate(doubled)[[1]]), rep(6, 5))
})

test_that("only as_data_mutable() nodes are backed by variables", {
  skip_if_not(check_tf_version())

  # the 0 and 1 in normal(0, 1) are data nodes too, but nobody will replace them
  m <- mutable_model(rep(1, 5))$model

  data_nodes <- m$dag$node_list[m$dag$node_types == "data"]
  mutable <- vapply(data_nodes, function(node) node$mutable, logical(1))

  expect_gt(length(data_nodes), sum(mutable))
  expect_identical(length(m$dag$data_variables), sum(mutable))
})

test_that("%*% does not mark its operands mutable", {
  skip_if_not(check_tf_version())

  # coercion inside an operator is not the user declaring data, so none of
  # these become tf$Variables
  design <- matrix(rnorm(20), 10, 2)
  coefs <- normal(0, 1, dim = 2)
  obs <- rnorm(10)
  distribution(obs) <- normal(design %*% coefs, 1)
  m <- model(coefs)

  data_nodes <- m$dag$node_list[m$dag$node_types == "data"]
  mutable <- vapply(data_nodes, function(node) node$mutable, logical(1))

  expect_identical(sum(mutable), 0L)
})

test_that("replacing non-mutable data points at as_data_mutable()", {
  skip_if_not(check_tf_version())

  x <- as_data(rep(1, 5))
  z <- normal(0, 1)
  distribution(x) <- normal(z, 1)
  m <- model(z)

  expect_snapshot(error = TRUE, data_values(m, x) <- rep(3, 5))
})

test_that("marking data after the model is built is reported, not silent", {
  skip_if_not(check_tf_version())

  x <- as_data(rep(1, 5))
  z <- normal(0, 1)
  distribution(x) <- normal(z, 1)
  m <- model(z)

  # the graph already reads a constant for x, so a variable assigned now would
  # change nothing
  node <- get_node(x)
  node$mutable <- TRUE

  expect_snapshot(error = TRUE, data_values(m, x) <- rep(3, 5))
})
