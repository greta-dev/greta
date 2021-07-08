# %*% errors informatively

    Code
      a %*% b
    Error <simpleError>
      incompatible dimensions: "3x4" vs "1x4"

---

    Code
      a %*% c
    Error <simpleError>
      only two-dimensional <greta_array>s can be matrix-multiplied
      dimensions recorded were 3 and 4

