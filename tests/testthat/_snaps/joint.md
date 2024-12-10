# joint of fixed and continuous distributions errors

    Code
      joint(bernoulli(0.5), normal(0, 1))
    Condition
      Error in `initialize()`:
      ! Cannot construct a joint distribution from a combination of discrete and continuous distributions

# joint with insufficient distributions errors

    Code
      joint(normal(0, 2))
    Condition
      Error in `initialize()`:
      ! `joint()` must be passed at least 2 distributions
      The number of distributions passed was: 1

---

    Code
      joint()
    Condition
      Error in `initialize()`:
      ! `joint()` must be passed at least 2 distributions
      The number of distributions passed was: 0

# joint with non-scalar distributions errors

    Code
      joint(normal(0, 2, dim = 3), normal(0, 1, dim = 3))
    Condition
      Error in `initialize()`:
      ! `joint()` only accepts probability distributions over scalars

