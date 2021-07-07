# simulate errors if distribution-free variables are not fixed

    Code
      sims <- simulate(m)
    Error <simpleError>
      the target <greta_array>s are related to variables that do not have
      distributions so cannot be sampled

# simulate errors if a distribution cannot be sampled from

    Code
      sims <- simulate(m)
    Error <simpleError>
      sampling is not yet implemented for "hypergeometric" distributions

# simulate errors nicely if nsim is invalid

    Code
      simulate(m, nsim = 0)
    Error <simpleError>
      value must be a positive integer
      However the value provided was: 0

---

    Code
      simulate(m, nsim = -1)
    Error <simpleError>
      value must be a positive integer
      However the value provided was: -1

---

    Code
      simulate(m, nsim = "five")
    Error <simpleError>
      value must be a positive integer
      However the value provided was: NA

