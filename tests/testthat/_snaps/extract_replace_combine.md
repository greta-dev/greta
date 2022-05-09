# abind errors informatively

    all <greta_array>s must have the same dimensions except on the `along` dimension
    However, dimension 1 had varying sizes: 5 and 1

---

    `along` must be between 0 and 4
    Instead `along` was 5

# assign errors on variable greta arrays

    cannot replace values in a variable <greta_array>

# rbind and cbind give informative error messages

    all <greta_array>s must be have the same number of columns

---

    all <greta_array>s must be have the same number of rows

# replacement gives informative error messages

    number of items to replace is not a multiple of replacement length

---

    subscript out of bounds

---

    subscript out of bounds

# extraction gives informative error messages

    subscript out of bounds

---

    subscript out of bounds

# dim<- errors as expected

    length-0 dimension vector is invalid

---

    the dims contain missing values

---

    the dims contain negative values:
    `dim(x)` returns 3 and 4

---

    dims [product 13] do not match the length of object [12]

