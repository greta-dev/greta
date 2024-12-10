# abind errors informatively

    Code
      abind(a, b)
    Condition
      Error in `abind()`:
      ! all <greta_array>s must have the same dimensions except on the `along` dimension
      However, dimension 1 had varying sizes: 5 and 1

---

    Code
      abind(a, c, along = 5)
    Condition
      Error in `abind()`:
      ! `along` must be between 0 and 4
      Instead `along` was 5

# assign errors on variable greta arrays

    Code
      z[1] <- 3
    Condition
      Error in `[<-`:
      ! cannot replace values in a variable <greta_array>

# rbind and cbind give informative error messages

    Code
      rbind(a, b)
    Condition
      Error in `rbind()`:
      ! all <greta_array>s must be have the same number of columns

---

    Code
      cbind(a, b)
    Condition
      Error in `cbind()`:
      ! all <greta_array>s must be have the same number of rows

# replacement gives informative error messages

    Code
      x[1:2, , 1] <- seq_len(3)
    Condition
      Error in `[<-`:
      ! number of items to replace is not a multiple of replacement length

---

    Code
      x[1, 1, 3] <- 1
    Condition
      Error:
      ! subscript out of bounds

---

    Code
      x[3] <- 1
    Condition
      Error in `[<-`:
      ! subscript out of bounds

# extraction gives informative error messages

    Code
      x[1, 1, 3]
    Condition
      Error:
      ! subscript out of bounds

---

    Code
      x[3]
    Condition
      Error in `x[3]`:
      ! subscript out of bounds

# dim<- errors as expected

    Code
      dim(x) <- pi[0]
    Condition
      Error in `dim<-`:
      ! length-0 dimension vector is invalid

---

    Code
      dim(x) <- c(1, NA)
    Condition
      Error in `dim<-`:
      ! the dims contain missing values

---

    Code
      dim(x) <- c(1, -1)
    Condition
      Error in `dim<-`:
      ! the dims contain negative values:
      `dim(x)` returns 3 and 4

---

    Code
      dim(x) <- 13
    Condition
      Error in `dim<-`:
      ! dims [product 13] do not match the length of object [12]

