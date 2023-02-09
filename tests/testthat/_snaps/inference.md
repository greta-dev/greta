# bad mcmc proposals are rejected

    The log density could not be evaluated at these initial values
    Try using these initials as the values argument in `calculate()` to see what values of subsequent <greta_array>s these initial values lead to.

---

    Could not find reasonable starting values after 20 attempts.
    Please specify initial values manually via the `initial_values` argument

# mcmc handles initial values nicely

    the number of provided initial values does not match chains
    3 sets of initial values were provided, but there are 2 chains

---

    the initial values provided have different dimensions than the named <greta_array>s

---

    Code
      draws <- mcmc(m, warmup = 10, n_samples = 10, chains = 2, initial_values = inits,
        verbose = FALSE)
    Message <simpleMessage>
      only one set of initial values was provided, and was used for all chains

# progress bar gives a range of messages

    Code
      draws <- mock_mcmc(1010)
    Message <message>
      
        sampling          1010/1010 | eta:  0s | <1% bad

---

    Code
      draws <- mock_mcmc(500)
    Message <message>
      
        sampling            500/500 | eta:  0s | 2% bad 

---

    Code
      draws <- mock_mcmc(10)
    Message <message>
      
        sampling =========== 10/10 | eta:  0s | 100% bad
      

# samples has object names

    Code
      rownames(summary(draws)$statistics)
    Output
      [1] "a"      "b[1,1]" "b[2,1]" "b[3,1]"

---

    Code
      rownames(summary(c_draws)$statistics)
    Output
      [1] "c[1,1]" "c[2,1]" "c[3,1]"

# model errors nicely

    `model()` arguments must be <greta_array>s
    The following object passed to `model()` is not a <greta array>:
    "a"
    

# numerical issues are handled in mcmc

    Python module tensorflow was not found.
    
    Detected Python configuration:
    
    
    

# mcmc errors for invalid parallel plans

    parallel mcmc samplers cannot be run with `plan(multicore)`

---

    parallel mcmc samplers cannot be run with a fork cluster

# initials works

    initial values must be numeric

---

    all initial values must be named

---

    Code
      initials(a = 3)
    Output
      a greta initials object with values:
      
      $a
           [,1]
      [1,]    3
      

# prep_initials errors informatively

    `initial_values` must be an initials object created with `initials()`, or a simple list of initials objects

---

    `initial_values` must be an initials object created with `initials()`, or a simple list of initials objects

---

    some <greta_array>s passed to `initials()` are not associated with the model:
    `g`

---

    initial values can only be set for variable <greta_array>s

---

    initial values can only be set for variable <greta_array>s

---

    some provided initial values are outside the range of values their variables can take

---

    some provided initial values are outside the range of values their variables can take

---

    some provided initial values are outside the range of values their variables can take

# samplers print informatively

    Code
      hmc()
    Output
      hmc sampler object with parameters:
        Lmin = 5, Lmax = 10, epsilon = 0.1, diag_sd = 1

---

    Code
      rwmh()
    Output
      rwmh sampler object with parameters:
        proposal = normal, epsilon = 0.1, diag_sd = 1

---

    Code
      slice()
    Output
      slice sampler object with parameters:
        max_doublings = 5

---

    Code
      hmc(Lmin = 1)
    Output
      hmc sampler object with parameters:
        Lmin = 1, Lmax = 10, epsilon = 0.1, diag_sd = 1

