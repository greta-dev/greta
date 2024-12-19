# simulate errors if distribution-free variables are not fixed

    Code
      sims <- simulate(m)
    Condition
      Error in `calculate_list()`:
      ! the target <greta_array>s are related to variables that do not have distributions so cannot be sampled

# simulate errors if a distribution cannot be sampled from

    Code
      sims <- simulate(m)
    Condition
      Error in `check_sampling_implemented()`:
      ! Sampling is not yet implemented for "hypergeometric" distributions

# simulate errors nicely if nsim is invalid

    Code
      simulate(m, nsim = 0)
    Condition
      Error:
      ! nsim must be a positive integer
      However the value provided was: 0

---

    Code
      simulate(m, nsim = -1)
    Condition
      Error:
      ! nsim must be a positive integer
      However the value provided was: -1

---

    Code
      simulate(m, nsim = "five")
    Condition
      Error:
      ! nsim must be a positive integer
      However the value provided was: NA

