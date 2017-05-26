## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,
                      eval = TRUE,
                      cache = TRUE,
                      comment = NA,
                      progress = FALSE)
set.seed(1)
library(greta)

## ----greta_array1--------------------------------------------------------
x <- ones(3, 3)
is.list(x)
class(x)

## ----greta_array2--------------------------------------------------------
class(x$node)

## ----nodes1--------------------------------------------------------------
# data nodes have no children
length(x$node$children)

# operation nodes have one or more children
z <- x * 3
length(z$node$children)

## ----nodes2--------------------------------------------------------------
x$node$value()
# R calls this a matrix because it's 2d, but it's an array
class(x$node$value())
z$node$value()
class(z$node$value())

## ----tensors1------------------------------------------------------------
x$node$tf

## ----tensors2------------------------------------------------------------
z$node$tf

## ----free_state----------------------------------------------------------
a = free(lower = 0)
class(a$node)
a$node$tf_from_free

## ----distributions1------------------------------------------------------
b = normal(0, 1)
class(b$node)
class(b$node$distribution)
# a is the target of its own distribution
class(b$node$distribution$target)

## ----distributions2------------------------------------------------------
b$node$distribution$tf_log_density

## ----dag1----------------------------------------------------------------
model <- model(b)
model$dag$send_parameters
model$dag$log_density
model$dag$gradients

