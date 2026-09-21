# set_data_value() errors informatively on bad input

    Code
      m$dag$set_data_value(z, 1)
    Condition
      Error in `m$dag$set_data_value()`:
      ! `x` must be data.

---

    Code
      m$dag$set_data_value(x, rep(1, 3))
    Condition
      Error in `m$dag$set_data_value()`:
      ! `value` must have the same dimensions as the data it replaces.
      x Replacing data of dimension "5x1" with a value of dimension "3x1".
      i Changing the dimensions would mean rebuilding the graph.

