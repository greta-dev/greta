# adaptive_hmc() errors when given incorrect warmup or chains

    Code
      draws <- mcmc(m, n_samples = 1, warmup = 0, sampler = adaptive_hmc())
    Message
      running 4 chains simultaneously on up to 8 CPU cores
    Output
      
    Condition
      Error in `self$run_warmup()`:
      ! `warmup` must be greater than 0 for `adaptive_hmc()` sampler
      We saw: `warmup = 0`

---

    Code
      draws <- mcmc(m, n_samples = 1, warmup = 1, chains = 1, sampler = adaptive_hmc())
    Condition
      Error in `py_call_impl()`:
      ! ValueError: SNAPERHMC requires at least 2 chains. Got: 1
      Run `reticulate::py_last_error()` for details.

# bad mcmc proposals are rejected

    Code
      draws <- mcmc(m, chains = 1, n_samples = 2, warmup = 1, verbose = FALSE,
        sampler = adaptive_hmc(), initial_values = initials(z = 1e+120))
    Condition
      Error in `self$check_valid_parameters()`:
      ! The log density could not be evaluated at these initial values
      Try using these initials as the `values` argument in `calculate()` to see what values of subsequent <greta_array>s these initial values lead to.

---

    Code
      mcmc(m, chains = 1, n_samples = 1, warmup = 1, sampler = adaptive_hmc(),
      verbose = FALSE)
    Condition
      Error in `self$check_reasonable_starting_values()`:
      ! Could not find reasonable starting values after 20 attempts.
      Please specify initial values manually via the `initial_values` argument

