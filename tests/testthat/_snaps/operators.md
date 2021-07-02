# %*% errors informatively

    Code
      a %*% b
    Error <simpleError>
      incompatible dimensions: "3x4" vs "1x4"

---

    Code
      a %*% c
    Error <simpleError>
      cannot coerce type 'builtin' to vector of type 'character'

