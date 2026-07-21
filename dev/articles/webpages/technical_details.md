# Technical details

This page provides technical implementation details for potential
contributors or the curious. You don’t need to read this to be able to
use greta.

## greta arrays, nodes and tensors

There are three layers to how greta defines a model: users manipulate
*greta arrays*, these define *nodes*, and nodes then define *Tensors*.

### greta arrays

greta arrays are the user-facing representation of the model. greta
arrays extend R arrays and have the classes `greta_array` and `array`.

``` r

x <- ones(3, 3)
class(x)
```

    [1] "greta_array" "array"      

### nodes

The main difference between greta arrays and R arrays is that greta
array has a `node` attribute; an R6 object inheriting from the R6 class
‘node’, as well as one of the three node types: ‘data’, ‘operation’ or
‘variable’.

``` r

x_node <- attr(x, "node")
class(x_node)
```

    [1] "data_node" "node"      "R6"       

There is a fourth node type: ‘distribution’, but these are never
directly associated with greta arrays.

These R6 node objects are where the magic happens. When created, each
node points to its ‘parent’ nodes - the nodes for the greta arrays that
were used to create this one.

``` r

# data nodes have no parents
length(x_node$parents)
```

    [1] 0

``` r

# operation nodes have one or more parents
z <- x * 3
z_node <- attr(z, "node")
length(z_node$parents)
```

    [1] 2

Each node also has a list of its children, the nodes that have been
created from this one.

When
[`model()`](https://greta-dev.github.io/greta/dev/reference/model.md) is
called, that inheritance information is used to construct the directed
acyclic graph (DAG) that defines the model. The inheritance also
preserves intermediate nodes, such as those creates in multi-part
operations, but not assigned as greta arrays.

Nodes also have a value member, which is an array for data nodes or an
‘unknowns’ object for other node types. The unknowns class is a thin
wrapper around arrays, which prints the question marks. Generic
functions for working on arrays (e.g. `dim`, `length`, `print`) use
these node values to return something familiar to the user.

``` r

x_node$value()
```

         [,1] [,2] [,3]
    [1,]    1    1    1
    [2,]    1    1    1
    [3,]    1    1    1

``` r

# R calls this a matrix because it's 2d, but it's an array
class(x_node$value())
```

    [1] "matrix" "array" 

``` r

z_node$value()
```

         [,1] [,2] [,3]
    [1,]  ?    ?    ?  
    [2,]  ?    ?    ?  
    [3,]  ?    ?    ?  

``` r

class(z_node$value())
```

    [1] "unknowns" "matrix"   "array"   

### Tensors

In addition to remembering their shape and size and where they are in
the DAG, each node has methods to define a corresponding TensorFlow
Tensor object in a specified environment. That doesn’t happen until the
user runs
[`model()`](https://greta-dev.github.io/greta/dev/reference/model.md),
which creates a ‘dag_class’ object to store the relevant nodes, the
environment for the tensors, and methods to talk to the TensorFlow
graph.

The node `tf()` method takes the DAG as an argument, and defines a
tensor representing itself in the tensorflow environment, with a name
determined by the dag object.

``` r

x_node$tf
```

    function(dag) {
          tfe <- dag$tf_environment
          tf_name <- dag$tf_name(self)
          unbatched_name <- glue::glue("{tf_name}_unbatched")

          mode <- dag$how_to_define(self)

          # if we're in sampling mode, get the distribution constructor and sample
          if (mode == "sampling") {
            batched_tensor <- dag$draw_sample(self$distribution)
          }

          # if we're defining the forward mode graph, create either a constant or a
          # placeholder
          if (mode == "forward") {
            value <- self$value()
            ndim <- n_dim(value)
            shape <- to_shape(c(1, dim(value)))
            value <- add_first_dim(value)

            # under some circumstances we define data as constants, but normally as
            # placeholders
            using_constants <- !is.null(greta_stash$data_as_constants)

            if (using_constants) {
              unbatched_tensor <- tf$constant(
                value = value,
                dtype = tf_float(),
                shape = shape
              )
            } else {
              # TF1/2 check
              # We can pass tensors directly into ops and layers
              # tf.function arguments do the job of placeholders
              # or we can use tf$keras$Input ?
              # unbatched_tensor <- tf$keras$Input(
              # for data - find yourself so it can be substituted in
              # we need to fetch the data from the DAG
              # what is the TF2 method for casting data into a tensor
              # we can probably just use `as_tensor`
              unbatched_tensor <- tensorflow::as_tensor(
                x = value,
                shape = shape,
                dtype = tf_float()
              )
              # TF1/2 check
              # note - we might not need this anymore as it was to do with
              # stashing things for use in the feed_dict later
              dag$set_tf_data_list(unbatched_name, value)
            }

            # expand up to batch size - so we can run multiple chains
            tiling <- c(tfe$.batch_size, rep(1L, ndim))
            batched_tensor <- tf$tile(unbatched_tensor, tiling)

            # put unbatched tensor in environment so it can be set
            assign(unbatched_name, unbatched_tensor, envir = tfe)
          }

          assign(tf_name, batched_tensor, envir = tfe)
        }
    <environment: 0x89a63cc48>

Because R6 objects are pass-by-reference (rather than pass-by-value),
the dag accumulates all of the defined tensors, rather than being
re-written each time. Similarly, because nodes are R6 objects and know
which are their parents, they can make sure those parents are defined as
tensors before they are. The `define_tf()` member function ensures that
that happens, enabling operation nodes to use the tensors for their
parents when defining their own tensor.

``` r

x_node$define_tf
```

    function(dag) {
          if (Sys.getenv("GRETA_DEBUG") == "true") {
            browser()
          }
          # if defined already, skip
          if (!self$defined(dag)) {
            # make sure parents are defined
            parents_defined <- vapply(
              self$list_parents(dag),
              function(x) x$defined(dag),
              FUN.VALUE = FALSE
            )
            if (!all(parents_defined)) {
              parents <- self$list_parents(dag)
              lapply(
                parents[which(!parents_defined)],
                function(x) {
                  x$define_tf(dag)
                }
              )
            }

            # then define self
            # stop("hi from the future ... parents are of class:", str(parents))
            self$tf(dag)
          }
        }
    <environment: 0x89a63b310>

## variables and free states

Hamiltonian Monte Carlo (HMC) requires all of the parameters to be
transformed to a continuous scale for sampling. Variable nodes are
therefore converted to tensors by first defining a ‘free’
(unconstrained) version of themselves as a tensor, and then applying a
transformation function to convert them back to the scale of interest.

``` r

a <- variable(lower = 0)
a_node <- attr(a, "node")
class(a_node)
```

    [1] "variable_node" "node"          "R6"           

``` r

a_node$tf_from_free
```

    function(x) {
          tf_bijector <- self$create_tf_bijector()
          tf_bijector$forward(x)
        }
    <environment: 0x8a14a4468>

## distributions

distribution nodes are node objects just like the others, but they are
not *directly* associated with greta arrays. Instead, greta arrays may
have a distribution node in their `distribution` slot to indicate that
their values are assumed to follow that distribution. The distribution
node will also be listed as a child node, and likewise the ‘target node’
will be listed as a child of the distribution. Distribution nodes also
have child nodes (data, variables or operations) representing their
parameters.

``` r

b <- normal(0, 1)
b_node <- attr(b, "node")
class(b_node)
```

    [1] "variable_node" "node"          "R6"           

``` r

class(b_node$distribution)
```

    [1] "normal_distribution" "distribution_node"   "node"               
    [4] "R6"                 

``` r

# b is the target of its own distribution
class(b_node$distribution$target)
```

    [1] "variable_node" "node"          "R6"           

When they define themselves as tensors, distribution nodes define the
log density of their target node/tensor given the tensors representing
their parameters.

``` r

b_node$distribution$tf
```

    function(dag) {
          # assign the distribution object constructor function to the environment
          assign(dag$tf_name(self), self$tf_distrib, envir = dag$tf_environment)
        }
    <environment: 0x8933e5bd0>

If the distribution was truncated, the log density is normalised using
the cumulative distribution function.

## Joint density

Those log-densities for these distributions are summed on the TensorFlow
graph to create a Tensor for the joint log-density of the model.
TensorFlow’s automatic gradient capabilities are then used to define a
Tensor for the gradient of the log-density with respect to each
parameter in the model.

The `dag` R6 object contained within the model then exposes methods to
send parameters to the TensorFlow graph and return the joint density and
gradient.

``` r

model <- model(b)
model$dag$send_parameters
```

    NULL

``` r

model$dag$log_density
```

    function() {
          res <- cleanly(self$tf_environment$joint_density_adj)

          if (inherits(res, "error")) {
            res <- NA
          }

          res
        }
    <environment: 0x89267ebd8>

``` r

model$dag$gradients
```

    NULL

These methods are used to check the validity of initial values, sampling
is now done using samplers from tensorflow probability, which require a
function mapping from the overall free state to the joint log density.
That’s created with the `generate_log_prob_function` method:

``` r

model$dag$generate_log_prob_function
```

    function(
          which = c(
            "both",
            "adjusted",
            "unadjusted"
          )
        ) {
          which <- match.arg(which)

          # we can only pass the free_state parameter through
          # we need some way to lexically scope the
          # batch size and the data
          function(free_state) {
            # temporarily define a new environment
            tfe_old <- self$tf_environment
            on.exit(self$tf_environment <- tfe_old)
            tfe <- self$tf_environment <- new.env()

            # put the free state in the environment, and build out the tf graph
            tfe$free_state <- free_state

            # we now make all of the operations define themselves now
            self$define_tf()
            # define the densities
            self$define_joint_density()

            objectives <- list(
              adjusted = tfe$joint_density_adj,
              unadjusted = tfe$joint_density
            )

            # return either of the densities, or a list of both
            result <- switch(
              which,
              adjusted = objectives$adjusted,
              unadjusted = objectives$unadjusted,
              both = objectives
            )

            result
          }
        }
    <environment: 0x89267ebd8>
