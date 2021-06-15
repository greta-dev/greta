# `distribution<-` errors informatively

    Code
      distribution(y) <- x
    Error <simpleError>
      right hand side must be a greta array

---

    Code
      distribution(y) <- as_data(x)
    Error <simpleError>
      right hand side must have a distribution

---

    Code
      distribution(y) <- variable()
    Error <simpleError>
      right hand side must have a distribution

---

    Code
      distribution(y) <- normal(0, 1, dim = c(3, 3, 1))
    Error <simpleError>
      left and right hand sides have different dimensions.
      The distribution must have dimension of either '3 x 3 x 2' or '1 x 1',but
      instead has dimension '3 x 3 x 1'

---

    Code
      distribution(y_) <- normal(0, 1)
    Error <simpleError>
      left hand side already has a distribution assigned

---

    Code
      distribution(y2) <- y1
    Error <simpleError>
      right hand side has already been assigned fixed values

---

    Code
      distribution(z) <- normal(0, 1)
    Error <simpleError>
      distributions can only be assigned to data greta arrays

---

    Code
      distribution(z2) <- normal(0, 1)
    Error <simpleError>
      distributions can only be assigned to data greta arrays

---

    Code
      distribution(z2) <- normal(0, 1)
    Error <simpleError>
      distributions can only be assigned to data greta arrays

# distribution() errors informatively

    Code
      distribution(y)
    Error <simpleError>
      `distribution()` expects object of type, <greta_array>
      object was not a <greta_array>, but <array>

