# variable() errors informatively

    Code
      variable(upper = NA)
    Error <simpleError>
      lower and upper must be numeric
      lower has class: numeric
      lower has length: 1
      upper has class: logical
      upper has length: 1

---

    Code
      variable(upper = head)
    Error <simpleError>
      lower and upper must be numeric
      lower has class: numeric
      lower has length: 1
      upper has class: function
      upper has length: 1

---

    Code
      variable(lower = 0:2, upper = 1:2)
    Error <simpleError>
      incompatible dimensions: 3x1, 2x1

---

    Code
      variable(lower = 1, upper = 1)
    Error <simpleError>
      upper bounds must be greater than lower bounds
      lower is: 1
      upper is: 1

# constrained variable constructors error informatively

    Code
      cholesky_variable(dim = 2:3)
    Error <simpleError>
      cholesky variables must be square
      However its dimension is: "2x3"

---

    Code
      cholesky_variable(dim = rep(2, 3))
    Error <simpleError>
      `dim` can either be a scalar or a vector of length 2
      However `dim` has length 3, and contains: "2, 2, 2"

---

    Code
      simplex_variable(1)
    Error <simpleError>
      the final dimension of a simplex variable must have more than one
      element

---

    Code
      simplex_variable(c(3, 1))
    Error <simpleError>
      the final dimension of a simplex variable must have more than one
      element

---

    Code
      ordered_variable(1)
    Error <simpleError>
      the final dimension of an ordered variable must have more than one
      element

---

    Code
      ordered_variable(c(3, 1))
    Error <simpleError>
      the final dimension of an ordered variable must have more than one
      element

