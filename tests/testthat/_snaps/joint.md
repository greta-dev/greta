# joint of fixed and continuous distributions errors

    Code
      joint(bernoulli(0.5), normal(0, 1))
    Error <simpleError>
      cannot construct a joint distribution from a combination of discrete and
      continuous distributions

# joint with insufficient distributions errors

    Code
      joint(normal(0, 2))
    Error <simpleError>
      `joint()` must be passed at least two distributions
      The number of distributions passed was 1

---

    Code
      joint()
    Error <simpleError>
      `joint()` must be passed at least two distributions
      The number of distributions passed was 0

# joint with non-scalar distributions errors

    Code
      joint(normal(0, 2, dim = 3), normal(0, 1, dim = 3))
    Error <simpleError>
      `joint()` only accepts probability distributions over scalars

