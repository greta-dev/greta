# mcmc prints out CPU and GPU text

    Code
      draws <- mcmc(m, n_samples = 5, warmup = 5, compute_options = cpu_only())
    Message <simpleMessage>
      running 4 chains simultaneously on up to 8 CPU cores
    Output
      
    Message <message>
      
          warmup                                              0/5 | eta:  ?s          
      
          warmup ============================================ 5/5 | eta:  0s          
      
      
        sampling                                              0/5 | eta:  ?s          
      
        sampling ============================================ 5/5 | eta:  0s          
      

---

    Code
      draws <- mcmc(m, n_samples = 5, warmup = 5, compute_options = gpu_only())
    Message <simpleMessage>
      NOTE: When using GPU, the random number seed may not always be respected (results may not be fully reproducible).
      For more information, see details of the `compute_options` argument in `?calculate`.
      You can turn off this message with:
      `options(greta_gpu_message = FALSE)`
      running 4 chains simultaneously on GPU
    Output
      
    Message <message>
      
          warmup                                              0/5 | eta:  ?s          
      
          warmup ============================================ 5/5 | eta:  0s          
      
      
        sampling                                              0/5 | eta:  ?s          
      
        sampling ============================================ 5/5 | eta:  0s          
      

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
    

# mcmc doesn't support slice sampler with double precision models

    greta hit a tensorflow error:
    Error in py_call_impl(callable, dots$args, dots$keywords): RuntimeError: Evaluation error: ValueError: slice index 1 of dimension 0 out of bounds. for '{{node strided_slice}} = StridedSlice[Index=DT_INT32, T=DT_DOUBLE, begin_mask=0, ellipsis_mask=0, end_mask=0, new_axis_mask=0, shrink_axis_mask=1](sampler_param_vec, strided_slice/stack, strided_slice/stack_1, strided_slice/stack_2)' with input shapes: [1], [1], [1], [1] and with computed input tensors: input[1] = <1>, input[2] = <2>, input[3] = <1>. .

# numerical issues are handled in mcmc

    Python module tensorflow was not found.
    
    Detected Python configuration:
    
    
    

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

