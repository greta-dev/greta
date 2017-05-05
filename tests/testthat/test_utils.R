context('utils')

test_that('check_tf_version works', {

  # record the true version and forge an old version
  true_version <- tf$`__version__`
  tf$`__version__` <- "0.9.0"

  # expected text
  expected_message <-
  paste0("\n\n  greta requires TensorFlow version 1.0.0 or higher, but you ",
         "have version 0.9.0\n  You can write models, but not sample from ",
         "them.\n  See https://www.tensorflow.org/install for installation ",
         "instructions.\n\n")

  expect_error(greta:::check_tf_version('error'),
               expected_message)
  expect_warning(greta:::check_tf_version('warn'),
                 expected_message)
  expect_message(greta:::check_tf_version('message'),
                 expected_message)

  # reset the true version
  tf$`__version__` <- true_version

})
