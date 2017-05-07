context('dynamics module')

test_that('lambda iteration works', {

  source('helpers.R')

  n <- 10
  mat <- randu(n, n)
  init <- rep(1, n)
  niter <- 50

  # r version
  target_lambda <- iterate_lambda(matrix = mat,
                                  state = init,
                                  niter = niter)

  # greta version
  lambda <- dynamics$iterate_lambda(matrix = mat,
                                    state = init,
                                    niter = niter)
  greta_lambda <- grab(lambda)[1, 1]

  difference <- abs(greta_lambda - target_lambda)
  expect_true(difference < 1e-6)

})

test_that('state iteration works', {

  source('helpers.R')

  n <- 10
  mat <- randu(n, n)
  init <- rep(1, n)
  niter <- 50

  # r version
  target_state <- iterate_state(matrix = mat,
                                state = init,
                                niter = niter)
  target_state <- target_state / sum(target_state)

  # greta version
  state <- dynamics$iterate_state(matrix = mat,
                                    state = init,
                                    niter = niter)
  greta_state <- grab(state)[, 1]
  greta_state <- greta_state / sum(greta_state)

  difference <- abs(greta_state - target_state)
  expect_true(all(difference < 1e-6))

})


test_that('vectorised lambda iteration works', {

  source('helpers.R')

  n <- 10
  n_mat <- 20
  mat_list <- replicate(n_mat, randu(n, n), simplify = FALSE)
  init <- rep(1, n)
  niter <- 50

  # r version
  target_lambda_list <- lapply(mat_list,
                               iterate_lambda,
                               state = init,
                               niter = niter)
  target_lambdas <- unlist(target_lambda_list)

  # greta version (unpack first)
  flat_mat_list <- lapply(mat_list, greta:::flatten_rowwise)
  matrices <- do.call(rbind, flat_mat_list)
  lambdas <- dynamics$iterate_lambda_vectorised(as_data(matrices),
                                                state = as_data(init),
                                                niter = niter,
                                                n = n_mat,
                                                m = n)
  greta_lambdas <- grab(lambdas)[, 1]

  difference <- abs(greta_lambdas - target_lambdas)
  expect_true(all(difference < 1e-6))

})
