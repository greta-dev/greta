# variable() errors informatively

    Code
      variable(upper = NA)
    Condition
      Error in `initialize()`:
      ! lower and upper must be numeric
      lower has class: numeric
      lower has length: 1
      upper has class: logical
      upper has length: 1

---

    Code
      variable(upper = head)
    Condition
      Error in `initialize()`:
      ! lower and upper must be numeric
      lower has class: numeric
      lower has length: 1
      upper has class: function
      upper has length: 1

---

    Code
      variable(lower = NA)
    Condition
      Error in `initialize()`:
      ! lower and upper must be numeric
      lower has class: logical
      lower has length: 1
      upper has class: numeric
      upper has length: 1

---

    Code
      variable(lower = head)
    Condition
      Error in `initialize()`:
      ! lower and upper must be numeric
      lower has class: function
      lower has length: 1
      upper has class: numeric
      upper has length: 1

---

    Code
      variable(lower = 0:2, upper = 1:2)
    Condition
      Error in `initialize()`:
      ! incompatible dimensions: 3x1, 2x1

---

    Code
      variable(lower = 1, upper = 1)
    Condition
      Error in `initialize()`:
      ! upper bounds must be greater than lower bounds
      lower is: 1
      upper is: 1

# constrained variable constructors error informatively

    Code
      cholesky_variable(dim = 2:3)
    Condition
      Error in `cholesky_variable()`:
      ! Object must be 2D square array
      x But it had dimension: "2x3"

---

    Code
      cholesky_variable(dim = rep(2, 3))
    Condition
      Error in `cholesky_variable()`:
      ! `dim` can either be a scalar or a vector of length 2
      However `dim` has length 3, and contains:
      2, 2, and 2

---

    Code
      simplex_variable(1)
    Condition
      Error in `simplex_variable()`:
      ! The final dimension of a simplex variable must have more than one element
      The final dimension has: "1 element"

---

    Code
      simplex_variable(c(3, 1))
    Condition
      Error in `simplex_variable()`:
      ! The final dimension of a simplex variable must have more than one element
      The final dimension has: "1 element"

---

    Code
      ordered_variable(1)
    Condition
      Error in `ordered_variable()`:
      ! The final dimension of a ordered variable must have more than one element
      The final dimension has: "1 element"

---

    Code
      ordered_variable(c(3, 1))
    Condition
      Error in `ordered_variable()`:
      ! The final dimension of a ordered variable must have more than one element
      The final dimension has: "1 element"

