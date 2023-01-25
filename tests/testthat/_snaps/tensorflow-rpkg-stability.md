# Tensor behaves as we expect

    Code
      length(x)
    Output
      [1] 1

---

    Code
      dim(x)
    Output
      integer(0)

---

    Code
      tf$reshape(x, tensorflow::as_tensor(shape(-1, 1, 1L, 1, 1L)))
    Output
      tf.Tensor([[[[[42]]]]], shape=(1, 1, 1, 1, 1), dtype=int32)

# shape returns right thing

    Code
      shape()
    Output
      TensorShape([])

---

    Code
      shape(NULL)
    Output
      TensorShape([None])

---

    Code
      shape(NA)
    Output
      TensorShape([None])

---

    Code
      shape(dims = NULL)
    Output
      TensorShape(None)

---

    Code
      shape(3, 4)
    Output
      TensorShape([3, 4])

---

    Code
      shape(NA, 4)
    Output
      TensorShape([None, 4])

---

    Code
      shape(dims = c(NA, 4))
    Output
      TensorShape([None, 4])

---

    Code
      shape(1, 1, 1)
    Output
      TensorShape([1, 1, 1])

# TensorShape conversions remain stable

    Code
      as.list(x)
    Output
      [[1]]
      NULL
      
      [[2]]
      [1] 3
      

---

    Code
      as.integer(x)
    Output
      [1] NA  3

---

    Code
      tensorflow::as_tensor(x)
    Output
      tf.Tensor([-1  3], shape=(2), dtype=int32)

---

    Code
      x[[1]]
    Output
      NULL
---

    Code
      x[[2]]
    Output
      [1] 3

# shape returns appropriate TensorShape object

    Code
      shape()
    Output
      TensorShape([])

---

    Code
      shape(NULL)
    Output
      TensorShape([None])

---

    Code
      shape(NA)
    Output
      TensorShape([None])

---

    Code
      shape(dims = NULL)
    Output
      TensorShape(None)

---

    Code
      shape(3, 4)
    Output
      TensorShape([3, 4])

---

    Code
      shape(NA, 4)
    Output
      TensorShape([None, 4])

---

    Code
      shape(dims = c(NA, 4))
    Output
      TensorShape([None, 4])

---

    Code
      c(shape(1), 3)
    Output
      TensorShape([1, 3])

---

    Code
      length(shape(1))
    Output
      [1] 1

---

    Code
      length(shape(1, 3))
    Output
      [1] 2

---

    Code
      as.integer(shape(1, 3))
    Output
      [1] 1 3

---

    Code
      as.numeric(shape(1, 3))
    Output
      [1] 1 3

---

    Code
      as.double(shape(1, 3))
    Output
      [1] 1 3

---

    Code
      shape(1, 3) == shape(1, 3)
    Output
      [1] TRUE

---

    Code
      shape(1, 3) == shape(1, 2)
    Output
      [1] FALSE

---

    Code
      shape(1, 3) != shape(1, 3)
    Output
      [1] FALSE

---

    Code
      shape(1, 3) != shape(1, 2)
    Output
      [1] TRUE

# [, [[, and assignment returns right object

    Code
      x_extract[1]
    Output
      TensorShape([1])

---

    Code
      x_extract[[1]]
    Output
      [1] 1

---

    Code
      x_extract[2:3]
    Output
      TensorShape([2, 3])

---

    Code
      x_extract[-1]
    Output
      TensorShape([2, 3])

---

    Code
      x_extract[1] <- 11
      x_extract[1]
    Output
      TensorShape([11])

---

    Code
      x_extract[1] <- shape(11)
      x_extract[1]
    Output
      TensorShape([11])

---

    Code
      x_extract[1] <- list(11)
      x_extract[1]
    Output
      TensorShape([11])

