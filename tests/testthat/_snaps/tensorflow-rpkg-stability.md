# Tensor behaves as we expect

    [1] 1

---

    integer(0)

---

    Tensor("Reshape:0", shape=(1, 1, 1, 1, 1), dtype=int32)

# shape returns right thing

    TensorShape([])

---

    TensorShape([Dimension(None)])

---

    TensorShape([Dimension(None)])

---

    TensorShape(None)

---

    TensorShape([Dimension(3), Dimension(4)])

---

    TensorShape([Dimension(None), Dimension(4)])

---

    TensorShape([Dimension(None), Dimension(4)])

---

    TensorShape([Dimension(1), Dimension(1), Dimension(1)])

# placeholder and friends behave the same way

    [1]  2 NA

---

    [1] NA

---

    [1]  2 NA

---

    [1] NA

---

    [1] 1

---

    NULL

---

    [1] 20

---

    [1]  2 10

---

    NULL

---

    [1] 1

---

    [1] 8

---

    [1] 2 4

# TensorShape conversions remain stable

    [[1]]
    NULL
    
    [[2]]
    [1] 3
    

---

    [1] NA  3

---

    Tensor("Const_8:0", shape=(2,), dtype=int32)

---

    NULL

---

    [1] 3

# shape returns appropriate TensorShape object

    TensorShape([])

---

    TensorShape([Dimension(None)])

---

    TensorShape([Dimension(None)])

---

    TensorShape(None)

---

    TensorShape([Dimension(3), Dimension(4)])

---

    TensorShape([Dimension(None), Dimension(4)])

---

    TensorShape([Dimension(None), Dimension(4)])

---

    TensorShape([Dimension(1), Dimension(3)])

---

    [1] 1

---

    [1] 2

---

    [1] 1 3

---

    [1] 1 3

---

    [1] 1 3

---

    [1] TRUE

---

    [1] FALSE

---

    [1] FALSE

---

    [1] TRUE

# [, [[, and assignment returns right object

    TensorShape([Dimension(1)])

---

    [1] 1

---

    TensorShape([Dimension(2), Dimension(3)])

---

    TensorShape([Dimension(2), Dimension(3)])

---

    

---

    

---

    

