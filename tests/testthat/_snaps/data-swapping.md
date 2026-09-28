# set_data_value() errors informatively on bad input

    Code
      dag$set_data_value(z, 1)
    Condition
      Error in `dag$set_data_value()`:
      ! `z` must be data

---

    Code
      dag$set_data_value(x, rep(1, 3))
    Condition
      Error in `dag$set_data_value()`:
      ! `value` must have the same dimensions as the data it replaces
      x We see dimensions: "3x1"
      i But we expect dimensions: "5x1"
      i Changing the dimensions would mean rebuilding the graph

# data_values<- errors informatively

    Code
      data_values(m, x) <- rep(1, 3)
    Condition
      Error in `data_values<-`:
      ! `value` must have the same dimensions as the data it replaces
      x We see dimensions: "3x1"
      i But we expect dimensions: "5x1"
      i Changing the dimensions would mean rebuilding the graph

---

    Code
      data_values(m, z) <- 1
    Condition
      Error in `data_values<-`:
      ! `z` must be data

---

    Code
      data_values(not_a_model, x) <- rep(1, 5)
    Condition
      Error in `data_values<-`:
      ! `model` must be a <greta_model>
      But `model` is <character>

---

    Code
      data_values(m, elsewhere) <- rep(1, 5)
    Condition
      Error in `data_values<-`:
      ! `elsewhere` is not part of this model

# a replacement has to clear the same bar as_data() sets

    Code
      data_values(m, x) <- c(1, NA, 3, 4, 5)
    Condition
      Error in `as.greta_array()`:
      ! <greta_array> must not contain missing or infinite values

---

    Code
      data_values(m, x) <- c(1, Inf, 3, 4, 5)
    Condition
      Error in `as.greta_array()`:
      ! <greta_array> must not contain missing or infinite values

---

    Code
      data_values(m, x) <- letters[1:5]
    Condition
      Error in `as.greta_array()`:
      ! Object cannot be coerced to <greta_array>
      Objects of class <character> cannot be coerced to a <greta_array>

# replacing non-mutable data points at as_data_mutable()

    Code
      data_values(m, x) <- rep(3, 5)
    Condition
      Error in `data_values<-`:
      ! `x` is not mutable data, so it cannot be replaced
      i Declare it with `as_data_mutable()` rather than `as_data()`

# marking data after the model is built is reported, not silent

    Code
      data_values(m, x) <- rep(3, 5)
    Condition
      Error in `data_values<-`:
      ! `x` was declared mutable after this model was built
      i Build the model again to replace its values

