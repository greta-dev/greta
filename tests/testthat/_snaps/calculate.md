# stochastic calculate works with greta_mcmc_list objects

    Code
      calc_a <- calculate(a, y, values = draws)
    Condition
      Error in `calculate_greta_mcmc_list()`:
      ! `nsim` must be set to sample <greta array>s not in MCMC samples
      the greta arrays `y` have distributions and are not in the MCMC samples, so cannot be calculated from the samples alone.
      Set `nsim` if you want to sample them conditionally on the MCMC samples

---

    `nsim` was greater than the number of posterior samples in values, so posterior samples had to be drawn with replacement

# calculate errors if the mcmc samples unrelated to target

    Code
      calc_c <- calculate(c, values = draws)
    Condition
      Error in `calculate_greta_mcmc_list()`:
      ! the target <greta array>s do not appear to be connected to those in the <greta_mcmc_list> object

# stochastic calculate works with mcmc samples & new stochastics

    Code
      calc_b <- calculate(b, values = draws)
    Condition
      Error in `calculate_greta_mcmc_list()`:
      ! `nsim` must be set to sample <greta array>s not in MCMC samples
      the target <greta array>s are related to new variables that are not in the MCMC samples, so cannot be calculated from the samples alone.
      Set `nsim` if you want to sample them conditionally on the MCMC samples

# calculate errors nicely if non-greta arrays are passed

    Code
      calc_y <- calculate(y, x, values = list(x = c(2, 1)))
    Condition
      Error in `calculate()`:
      ! `calculate()` arguments must be <greta_array>s
      The following object passed to `calculate()` is not a <greta array>:
      "x"
      Perhaps you forgot to explicitly name other arguments?

---

    Code
      calc_y <- calculate(y, list(x = c(2, 1)))
    Condition
      Error in `calculate()`:
      ! `calculate()` arguments must be <greta_array>s
      The following object passed to `calculate()` is not a <greta array>:
      "list(x = c(2, 1))"
      Perhaps you forgot to explicitly name other arguments?

# calculate errors nicely if values for stochastics not passed

    Code
      calc_y <- calculate(y, values = list(x = c(2, 1)))
    Condition
      Error in `lapply()`:
      ! Please provide values for the following 1 <greta_array>:
      `a`

# calculate errors nicely if values have incorrect dimensions

    Code
      calc_y <- calculate(y, values = list(a = c(1, 1)))
    Condition
      Error:
      ! a provided value has different number of elements than the <greta_array>

# calculate errors nicely with invalid batch sizes

    Code
      calc_y <- calculate(y, values = draws, trace_batch_size = 0)
    Condition
      Error in `calculate_greta_mcmc_list()`:
      ! `trace_batch_size` must be a single numeric value greater than or equal to 1

---

    Code
      calc_y <- calculate(y, values = draws, trace_batch_size = NULL)
    Condition
      Error in `calculate_greta_mcmc_list()`:
      ! `trace_batch_size` must be a single numeric value greater than or equal to 1

---

    Code
      calc_y <- calculate(y, values = draws, trace_batch_size = NA)
    Condition
      Error in `calculate_greta_mcmc_list()`:
      ! `trace_batch_size` must be a single numeric value greater than or equal to 1

# calculate errors if distribution-free variables are not fixed

    Code
      calc_a <- calculate(a, y, nsim = 1)
    Condition
      Error in `calculate_list()`:
      ! the target <greta_array>s are related to variables that do not have distributions so cannot be sampled

# calculate errors if a distribution cannot be sampled from

    Code
      sims <- calculate(y, nsim = 1)
    Condition
      Error in `check_sampling_implemented()`:
      ! Sampling is not yet implemented for "hypergeometric" distributions

# calculate errors nicely if nsim is invalid

    Code
      calc_x <- calculate(x, nsim = 0)
    Condition
      Error in `calculate()`:
      ! nsim must be a positive integer
      However the value provided was: 0

---

    Code
      calc_x <- calculate(x, nsim = -1)
    Condition
      Error in `calculate()`:
      ! nsim must be a positive integer
      However the value provided was: -1

---

    Code
      calc_x <- calculate(x, nsim = "five")
    Condition
      Error in `calculate()`:
      ! nsim must be a positive integer
      However the value provided was: NA

