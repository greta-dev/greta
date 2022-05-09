# opt converges with SciPy optimisers

    Code
      o <- opt(m, optimiser = optmr(), max_iterations = 500)

---

    Code
      o <- opt(m, optimiser = optmr(), max_iterations = 500)
    Warning <simpleWarning>
      This optimiser is deprecated and will be removed in greta 0.4.0.
      Please use a different optimiser.

---

    Code
      o <- opt(m, optimiser = optmr(), max_iterations = 500)
    Warning <simpleWarning>
      This optimiser is deprecated and will be removed in greta 0.4.0.
      Please use a different optimiser.

---

    Code
      o <- opt(m, optimiser = optmr(), max_iterations = 500)

---

    Code
      o <- opt(m, optimiser = optmr(), max_iterations = 500)
    Warning <simpleWarning>
      This optimiser is deprecated and will be removed in greta 0.4.0.
      Please use a different optimiser.

---

    Code
      o <- opt(m, optimiser = optmr(), max_iterations = 500)
    Warning <simpleWarning>
      This optimiser is deprecated and will be removed in greta 0.4.0.
      Please use a different optimiser.

---

    Code
      o <- opt(m, optimiser = optmr(), max_iterations = 500)
    Warning <simpleWarning>
      This optimiser is deprecated and will be removed in greta 0.4.0.
      Please use a different optimiser.

---

    Code
      o <- opt(m, optimiser = optmr(), max_iterations = 500)
    Warning <simpleWarning>
      This optimiser is deprecated and will be removed in greta 0.4.0.
      Please use a different optimiser.

---

    Code
      o <- opt(m, optimiser = optmr(), max_iterations = 500)
    Warning <simpleWarning>
      This optimiser is deprecated and will be removed in greta 0.4.0.
      Please use a different optimiser.

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
      mcmc(m, warmup = 10, n_samples = 10, chains = 2, initial_values = inits,
        verbose = FALSE)
    Message <simpleMessage>
      only one set of initial values was provided, and was used for all chains
    Output
      $`11`
      Markov Chain Monte Carlo (MCMC) output:
      Start = 1 
      End = 10 
      Thinning interval = 1 
                 z
      1  0.3742083
      2  0.3742083
      3  0.3742083
      4  0.3742083
      5  0.3742083
      6  0.3742083
      7  0.3742083
      8  0.3742083
      9  0.3742083
      10 0.3742083
      
      $`12`
      Markov Chain Monte Carlo (MCMC) output:
      Start = 1 
      End = 10 
      Thinning interval = 1 
                  z
      1  0.04036782
      2  0.04036782
      3  0.04036782
      4  0.04036782
      5  0.04036782
      6  0.04036782
      7  0.04036782
      8  0.04036782
      9  0.04036782
      10 0.04036782
      
      attr(,"class")
      [1] "greta_mcmc_list" "mcmc.list"      
      attr(,"model_info")
      attr(,"model_info")$raw_draws
      $`11`
      Markov Chain Monte Carlo (MCMC) output:
      Start = 1 
      End = 10 
      Thinning interval = 1 
             draws
      1  0.3742083
      2  0.3742083
      3  0.3742083
      4  0.3742083
      5  0.3742083
      6  0.3742083
      7  0.3742083
      8  0.3742083
      9  0.3742083
      10 0.3742083
      
      $`12`
      Markov Chain Monte Carlo (MCMC) output:
      Start = 1 
      End = 10 
      Thinning interval = 1 
              draws
      1  0.04036782
      2  0.04036782
      3  0.04036782
      4  0.04036782
      5  0.04036782
      6  0.04036782
      7  0.04036782
      8  0.04036782
      9  0.04036782
      10 0.04036782
      
      attr(,"class")
      [1] "mcmc.list"
      
      attr(,"model_info")$samplers
      attr(,"model_info")$samplers$`1`
      hmc_sampler object with parameters:
        Lmin = 5, Lmax = 10, epsilon = 0.7326749, diag_sd = 1
      
      attr(,"model_info")$model
      greta model

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
    

# mcmc doesn't support slice sampler with double precision models

    slice sampler can only currently be used for models defined with single precision
    set `model(..., precision = 'single')` instead

# numerical issues are handled in mcmc

    TensorFlow hit a numerical problem that caused it to error
    greta can handle these as bad proposals if you rerun `mcmc()` with the argument `one_by_one = TRUE`.
    This will slow down the sampler slightly.
    The error encountered can be recovered and viewed with:
    `greta_notes_tf_num_error()`

# mcmc errors for invalid parallel plans

    parallel mcmc samplers cannot be run with `plan(multiprocess)` or `plan(multicore)`

---

    parallel mcmc samplers cannot be run with `plan(multiprocess)` or `plan(multicore)`

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

