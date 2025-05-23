test_that("as_data coerces correctly", {
  skip_if_not(check_tf_version())

  # logical, integer and numeric
  # vector, matrix, array, dataframe

  # vectors
  log_vec <- sample(c(TRUE, FALSE), 100, replace = TRUE)
  int_vec <- sample.int(10, 100, replace = TRUE)
  num_vec <- rnorm(100)

  expect_true(is.logical(log_vec) & is.vector(log_vec))
  expect_true(is.numeric(int_vec) & is.integer(int_vec) & is.vector(int_vec))
  expect_true(is.numeric(num_vec) & is.vector(num_vec))

  ga_log_vec <- as_data(log_vec)
  ga_int_vec <- as_data(int_vec)
  ga_num_vec <- as_data(num_vec)

  expect_s3_class(ga_log_vec, "greta_array")
  expect_s3_class(ga_int_vec, "greta_array")
  expect_s3_class(ga_num_vec, "greta_array")

  expect_identical(c(length(log_vec), 1L), dim(ga_log_vec))
  expect_identical(c(length(int_vec), 1L), dim(ga_int_vec))
  expect_identical(c(length(num_vec), 1L), dim(ga_num_vec))

  # matrices
  log_mat <- matrix(log_vec, nrow = 10, ncol = 10)
  int_mat <- matrix(int_vec, nrow = 10, ncol = 10)
  num_mat <- matrix(num_vec, nrow = 10, ncol = 10)

  expect_true(is.logical(log_mat) & is.matrix(log_mat))
  expect_true(is.numeric(int_mat) & is.integer(int_mat) & is.matrix(int_mat))
  expect_true(is.numeric(num_mat) & is.matrix(num_mat))

  ga_log_mat <- as_data(log_mat)
  ga_int_mat <- as_data(int_mat)
  ga_num_mat <- as_data(num_mat)

  expect_s3_class(ga_log_mat, "greta_array")
  expect_s3_class(ga_int_mat, "greta_array")
  expect_s3_class(ga_num_mat, "greta_array")

  expect_identical(dim(log_mat), dim(ga_log_mat))
  expect_identical(dim(int_mat), dim(ga_int_mat))
  expect_identical(dim(num_mat), dim(ga_num_mat))

  # arrays
  log_arr <- array(log_vec, dim = c(10, 2, 5))
  int_arr <- array(int_vec, dim = c(10, 2, 5))
  num_arr <- array(num_vec, dim = c(10, 2, 5))

  expect_true(is.logical(log_arr) & is.array(log_arr))
  expect_true(is.numeric(int_arr) & is.integer(int_arr) & is.array(int_arr))
  expect_true(is.numeric(num_arr) & is.array(num_arr))

  ga_log_arr <- as_data(log_arr)
  ga_int_arr <- as_data(int_arr)
  ga_num_arr <- as_data(num_arr)

  expect_s3_class(ga_log_arr, "greta_array")
  expect_s3_class(ga_int_arr, "greta_array")
  expect_s3_class(ga_num_arr, "greta_array")

  expect_identical(dim(log_arr), dim(ga_log_arr))
  expect_identical(dim(int_arr), dim(ga_int_arr))
  expect_identical(dim(num_arr), dim(ga_num_arr))

  # dataframes
  log_df <- as.data.frame(log_mat)
  int_df <- as.data.frame(int_mat)
  num_df <- as.data.frame(num_mat)

  expect_true(
    is.data.frame(log_df) &
      all(vapply(log_df, is.logical, FALSE))
  )
  expect_true(
    is.data.frame(int_df) &
      all(vapply(int_df, is.numeric, FALSE)) &
      all(vapply(int_df, is.integer, FALSE))
  )
  expect_true(
    is.data.frame(num_df) &
      all(vapply(num_df, is.numeric, FALSE))
  )

  ga_log_df <- as_data(log_df)
  ga_int_df <- as_data(int_df)
  ga_num_df <- as_data(num_df)

  expect_s3_class(ga_log_df, "greta_array")
  expect_s3_class(ga_int_df, "greta_array")
  expect_s3_class(ga_num_df, "greta_array")

  expect_identical(dim(log_df), dim(ga_log_df))
  expect_identical(dim(int_df), dim(ga_int_df))
  expect_identical(dim(num_df), dim(ga_num_df))

  # data greta arrays should be identical under coercion
  ga_log_df2 <- as_data(ga_log_df)
  ga_one <- ones(3, 3)
  ga_zero <- zeros(3, 3, 2)

  expect_identical(ga_log_df, ga_log_df2)
  expect_identical(as_data(ga_one), ga_one)
  expect_identical(as_data(ga_zero), ga_zero)
})

test_that("as_data errors informatively", {
  skip_if_not(check_tf_version())

  # wrong class of object
  expect_snapshot(
    error = TRUE,
    as_data(NULL)
  )

  expect_snapshot(
    error = TRUE,
    as_data(list())
  )

  expect_snapshot(
    error = TRUE,
    as_data(environment())
  )

  # correct classes with wrong types
  cha_vec <- letters[1:20]
  cha_mat <- matrix(cha_vec, nrow = 10, ncol = 2)
  cha_arr <- array(cha_vec, dim = c(5, 2, 2))
  cha_df <- as.data.frame(cha_mat, stringsAsFactors = FALSE)
  cha_df2 <- as.data.frame(cha_mat, stringsAsFactors = TRUE)

  expect_snapshot(
    error = TRUE,
    as_data(cha_vec)
  )

  expect_snapshot(
    error = TRUE,
    as_data(cha_mat)
  )

  expect_snapshot(
    error = TRUE,
    as_data(cha_arr)
  )

  expect_snapshot(
    error = TRUE,
    as_data(cha_df)
  )

  expect_snapshot(
    error = TRUE,
    as_data(cha_df2)
  )

  # correct class and type but infinite or missing values
  arr_inf <- randn(3, 3)
  arr_inf[3, 2] <- Inf
  arr_minf <- randn(3, 3)
  arr_minf[1, 1] <- -Inf
  arr_na <- randn(3, 3)
  arr_na[1, 3] <- NA

  expect_snapshot(
    error = TRUE,
    as_data(arr_inf)
  )

  expect_snapshot(
    error = TRUE,
    as_data(arr_minf)
  )

  expect_snapshot(
    error = TRUE,
    as_data(arr_na)
  )

  # non-data greta arrays
  stoch <- normal(0, 1, dim = c(2, 3))
  op <- stoch^2

  expect_snapshot(
    error = TRUE,
    as_data(stoch)
  )
  expect_snapshot(
    error = TRUE,
    as_data(op)
  )
})
