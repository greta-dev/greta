# bad truncations error

    Code
      lognormal(0, 1, truncation = c(-1, Inf))
    Condition
      Error in `initialize()`:
      ! lower bound must be 0 or higher
      lower bound is: -1

---

    Code
      beta(1, 1, truncation = c(-1, 2))
    Condition
      Error in `initialize()`:
      ! lower and upper bounds must be between 0 and 1
      lower bound is: -1
      upper bound is: 2

