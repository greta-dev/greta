context('dynamics module')

test_that('lambda iteration works', {

  skip_if_not(check_tf_version())
  source('helpers.R')

  n <- 10
  mat <- randu(n, n)
  init <- rep(1, n)
  niter <- 50

  # r version
  target_lambda <- it_lambda(matrix = mat,
                             state = init,
                             niter = niter)

  # greta version
  lambda <- greta:::iterate_lambda(matrix = mat,
                                   state = init,
                                   niter = niter)
  greta_lambda <- grab(lambda)[1, 1]

  difference <- abs(greta_lambda - target_lambda)
  expect_true(difference < 1e-6)

})

test_that('state iteration works', {

  skip_if_not(check_tf_version())
  source('helpers.R')

  n <- 10
  mat <- randu(n, n)
  init <- rep(1, n)
  niter <- 50

  # r version
  target_state <- it_state(matrix = mat,
                           state = init,
                           niter = niter)
  target_state <- target_state / sum(target_state)

  # greta version
  state <- greta:::iterate_state(matrix = mat,
                                 state = init,
                                 niter = niter)
  greta_state <- grab(state)[, 1]
  greta_state <- greta_state / sum(greta_state)

  difference <- abs(greta_state - target_state)
  expect_true(all(difference < 1e-6))

})

test_that('vectorised lambda iteration works', {

  skip_if_not(check_tf_version())
  source('helpers.R')

  n <- 10
  n_mat <- 20
  mat_list <- replicate(n_mat, randu(n, n), simplify = FALSE)
  init <- rep(1, n)
  niter <- 50

  # r version
  target_lambda_list <- lapply(mat_list,
                               it_lambda,
                               state = init,
                               niter = niter)
  target_lambdas <- unlist(target_lambda_list)

  # greta version (unpack first)
  flat_mat_list <- lapply(mat_list, greta:::flatten_rowwise)
  matrices <- do.call(rbind, flat_mat_list)

  lambdas <- greta:::iterate_lambda_vectorised(matrices,
                                               state = init,
                                               niter = niter,
                                               n = n_mat,
                                               m = n)
  greta_lambdas <- grab(lambdas)[, 1]

  difference <- abs(greta_lambdas - target_lambdas)
  expect_true(all(difference < 1e-6))

})

test_that('dynamics module errors informatively', {

  source('helpers.R')

  niter <- 3
  n <- 10
  m <- 3

  bad_mat <- randu(m, m + 1)
  bad_state <- randu(m, 2)
  bad_matrices <- randu(n, m ^ 2, 1)

  good_mat <- randu(m, m)
  good_state <- randu(m, 1)
  good_matrices <- randu(n, m ^ 2)

  mismatched_state <- randu(m + 1, 1)

  # wrongly shaped matrix
  expect_error(greta:::iterate_lambda(matrix = bad_mat,
                                      state = good_state,
                                      niter = niter),
               'matrix must be a two-dimensional square greta array')

  expect_error(greta:::iterate_state(matrix = bad_mat,
                                     state = good_state,
                                     niter = niter),
               'matrix must be a two-dimensional square greta array')

  expect_error(greta:::iterate_lambda_vectorised(matrices = bad_matrices,
                                                 state = good_state,
                                                 niter = niter,
                                                 n = n,
                                                 m = m),
               '^matrix must be a rectangular greta array with dimensions')


  # wrongly shaped state
  expect_error(greta:::iterate_lambda(matrix = good_mat,
                                      state = bad_state,
                                      niter = niter),
               'state must be a column vector greta array')

  expect_error(greta:::iterate_state(matrix = good_mat,
                                     state = bad_state,
                                     niter = niter),
               'state must be a column vector greta array')

  expect_error(greta:::iterate_lambda_vectorised(matrices = good_matrices,
                                                 state = bad_state,
                                                 niter = niter,
                                                 n = n,
                                                 m = m),
               'state must be a column vector greta array')

  # mismatched matrix and state
  expect_error(greta:::iterate_lambda(matrix = good_mat,
                                      state = mismatched_state,
                                      niter = niter),
               'number of elements in state must match the dimension of matrix')

  expect_error(greta:::iterate_state(matrix = good_mat,
                                     state = mismatched_state,
                                     niter = niter),
               'number of elements in state must match the dimension of matrix')

  expect_error(greta:::iterate_lambda_vectorised(matrices = good_matrices,
                                                 state = mismatched_state,
                                                 niter = niter,
                                                 n = n,
                                                 m = m),
               'number of elements in state must match the dimension of matrix')

})
