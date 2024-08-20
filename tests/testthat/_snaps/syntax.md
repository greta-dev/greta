# `distribution<-` errors informatively

    Code
      distribution(y) <- x
    Condition
      Error in `distribution<-`:
      ! right hand side must be a <greta_array>

---

    Code
      distribution(y) <- as_data(x)
    Condition
      Error in `distribution<-`:
      ! right hand side must have a distribution

---

    Code
      distribution(y) <- variable()
    Condition
      Error in `distribution<-`:
      ! right hand side must have a distribution

---

    Code
      distribution(y) <- normal(0, 1, dim = c(3, 3, 1))
    Condition
      Error in `distribution<-`:
      ! left and right hand sides have different dimensions.
      The distribution must have dimension of either "3x3x2" or "1x1",but instead has dimension "3x3x1"

---

    Code
      distribution(y_) <- normal(0, 1)
    Condition
      Error in `distribution<-`:
      ! left hand side already has a distribution assigned

---

    Code
      distribution(y2) <- y1
    Condition
      Error in `distribution<-`:
      ! right hand side has already been assigned fixed values

---

    Code
      distribution(z) <- normal(0, 1)
    Condition
      Error in `distribution<-`:
      ! distributions can only be assigned to data <greta array>s

---

    Code
      distribution(z2) <- normal(0, 1)
    Condition
      Error in `distribution<-`:
      ! distributions can only be assigned to data <greta array>s

---

    Code
      distribution(z2) <- normal(0, 1)
    Condition
      Error in `distribution<-`:
      ! distributions can only be assigned to data <greta array>s

# distribution() errors informatively

    Code
      distribution(y)
    Condition
      Error in `distribution()`:
      ! `greta_array` must be <greta_array>
      `greta_array` is: <array>

