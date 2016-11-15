# syntax definitions


# special operator to create a data node in the parent environment,
# and assign it a likelihood
`%~%` <- function (data, distribution) {

  if (!inherits(data, 'data_node'))
    stop ('left hand side of likelihood must be a data node')

  if (!inherits(distribution, 'distribution'))
    stop ('left hand side of likelihood must be a data node')


  # provide the data to the likelihood and lock in the values in the
  # distribution
  distribution$value(data$value())
  distribution$.fixed_value <- TRUE

  # give the distribution to the data as a likelihood (this will register the
  # child distribution)
  data$set_likelihood(distribution)

  # register the data node, with it's own name
  data$register()

}
