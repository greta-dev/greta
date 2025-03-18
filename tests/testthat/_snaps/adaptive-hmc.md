# adaptive_hmc() errors when given 1 chain

    Code
      draws <- mcmc(m, n_samples = 1, warmup = 1, chains = 1, sampler = adaptive_hmc())
    Condition
      Error in `py_call_impl()`:
      ! ValueError: SNAPERHMC requires at least 2 chains. Got: 1
      Run `reticulate::py_last_error()` for details.

# adaptive_hmc() does not error when given 0 warmup

    Code
      draws <- mcmc(m, n_samples = 1, warmup = 0, chains = 2, sampler = adaptive_hmc())
    Message
      running 2 chains simultaneously on up to 8 CPU cores
    Output
      
    Condition
      Error in `self$run_warmup()`:
      ! object 'result' not found

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

