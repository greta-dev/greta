# Tensor behaves as we expect

    Code
      x
    Output
      Tensor("Const:0", shape=(), dtype=int32)

---

    Code
      dim(x)
    Output
      integer(0)

---

    Code
      tf$reshape(x, tensorflow::as_tensor(shape(-1, 1, 1L, 1, 1L)))
    Output
      Tensor("Reshape:0", shape=(1, 1, 1, 1, 1), dtype=int32)

# shape behaves as we expect

    Code
      tensorflow::shape(1, 1, 1)
    Output
      TensorShape([Dimension(1), Dimension(1), Dimension(1)])

