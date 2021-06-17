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

    Code
      mcmc(m, chains = 1, n_samples = 1, warmup = 0, initial_values = initials(z = 1e+20))
    Error <simpleError>
      The log density could not be evaluated at these initial values
      Try using `calculate()` to see whether they lead to values of other greta
      arrays in the model.

---

    Code
      mcmc(m, chains = 1, n_samples = 1, warmup = 0)
    Error <simpleError>
      Could not find reasonable starting values after 20 attempts.
      Please specify initial values manually via the `initial_values` argument

# mcmc handles initial values nicely

    Code
      mcmc(m, warmup = 10, n_samples = 10, verbose = FALSE, chains = 2,
        initial_values = inits)
    Error <simpleError>
      the number of provided initial values does not match chains
      3 sets of initial values were provided, but there are 2 chains

---

    Code
      mcmc(m, warmup = 10, n_samples = 10, verbose = FALSE, chains = 2,
        initial_values = inits)
    Error <simpleError>
      the initial values provided have different dimensions than the named
      greta arrays

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
      1  0.2320075
      2  0.2320075
      3  0.2320075
      4  0.2320075
      5  0.2320075
      6  0.2320075
      7  0.2320075
      8  0.2320075
      9  0.2320075
      10 0.2320075
      
      $`12`
      Markov Chain Monte Carlo (MCMC) output:
      Start = 1 
      End = 10 
      Thinning interval = 1 
                   z
      1  -0.03301933
      2  -0.03301933
      3  -0.03301933
      4  -0.03301933
      5  -0.03301933
      6  -0.03301933
      7  -0.03301933
      8  -0.03301933
      9  -0.03301933
      10 -0.03301933
      
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
      1  0.2320075
      2  0.2320075
      3  0.2320075
      4  0.2320075
      5  0.2320075
      6  0.2320075
      7  0.2320075
      8  0.2320075
      9  0.2320075
      10 0.2320075
      
      $`12`
      Markov Chain Monte Carlo (MCMC) output:
      Start = 1 
      End = 10 
      Thinning interval = 1 
               draws
      1  -0.03301933
      2  -0.03301933
      3  -0.03301933
      4  -0.03301933
      5  -0.03301933
      6  -0.03301933
      7  -0.03301933
      8  -0.03301933
      9  -0.03301933
      10 -0.03301933
      
      attr(,"class")
      [1] "mcmc.list"
      
      attr(,"model_info")$samplers
      attr(,"model_info")$samplers$`1`
      hmc_sampler object with parameters:
        Lmin = 5, Lmax = 10, epsilon = 0.7303997, diag_sd = 1
      
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

    Code
      model(a, b)
    Error <simpleError>
      `model()` arguments must be greta array
      The following object passed to `model()` is not a greta array:
      'a'

# mcmc doesn't support slice sampler with double precision models

    Code
      draws <- mcmc(m, sampler = slice(), n_samples = 100, warmup = 100)
    Error <simpleError>
      slice sampler can only currently be used for models defined with single
      precision
      set `model(..., precision = 'single')` instead

# numerical issues are handled in mcmc

    Code
      draws <- mcmc(m, verbose = FALSE)
    Error <simpleError>
      TensorFlow hit a numerical problem that caused it to error
      greta can handle these as bad proposals if you rerun `mcmc()` with the argument
      `one_by_one = TRUE`.
      This will slow down the sampler slightly.Error in py_call_impl(callable, dots$args, dots$keywords): InvalidArgumentError: Cholesky decomposition was not successful. The input might not be valid.
      	 [[node mcmc_sample_chain/trace_scan/while/smart_for_loop/while/mh_one_step/hmc_kernel_one_step/leapfrog_integrate/while/leapfrog_integrate_one_step/maybe_call_fn_and_grads/value_and_gradients/Cholesky (defined at Library/Frameworks/R.framework/Versions/4.1/Resources/library/reticulate/python/rpytools/call.py:13) ]]
      
      Original stack trace for 'mcmc_sample_chain/trace_scan/while/smart_for_loop/while/mh_one_step/hmc_kernel_one_step/leapfrog_integrate/while/leapfrog_integrate_one_step/maybe_call_fn_and_grads/value_and_gradients/Cholesky':
        File "Users/njtierney/Library/r-miniconda/envs/greta-env/lib/python3.7/site-packages/tensorflow_probability/python/mcmc/sample.py", line 361, in sample_chain
          parallel_iterations=parallel_iterations)
        File "Users/njtierney/Library/r-miniconda/envs/greta-env/lib/python3.7/site-packages/tensorflow_probability/python/mcmc/internal/util.py", line 370, in trace_scan
          parallel_iterations=parallel_iterations)
        File "Users/njtierney/Library/r-miniconda/envs/greta-env/lib/python3.7/site-packages/tensorflow/python/ops/control_flow_ops.py", line 3501, in while_loop
          return_same_structure)
        File "Users/njtierney/Library/r-miniconda/envs/greta-env/lib/python3.7/site-packages/tensorflow/python/ops/control_flow_ops.py", line 3012, in BuildLoop
          pred, body, original_loop_vars, loop_vars, shape_invariants)
        File "Users/njtierney/Library/r-miniconda/envs/greta-env/lib/python3.7/site-packages/tensorflow/python/ops/control_flow_ops.py", line 2937, in _BuildLoop
          body_result = body(*packed_vars_for_body)
        File "Users/njtierney/Library/r-miniconda/envs/greta-env/lib/python3.7/site-packages/tensorflow_probability/python/mcmc/internal/util.py", line 359, in _body
          state = loop_fn(state, elems_array.read(i))
        File "Users/njtierney/Library/r-miniconda/envs/greta-env/lib/python3.7/site-packages/tensorflow_probability/python/mcmc/sample.py", line 345, in _trace_scan_fn
          parallel_iterations=parallel_iterations)
        File "Users/njtierney/Library/r-miniconda/envs/greta-env/lib/python3.7/site-packages/tensorflow_probability/python/mcmc/internal/util.py", line 286, in smart_for_loop
          parallel_iterations=parallel_iterations
        File "Users/njtierney/Library/r-miniconda/envs/greta-env/lib/python3.7/site-packages/tensorflow/python/ops/control_flow_ops.py", line 3501, in while_loop
          return_same_structure)
        File "Users/njtierney/Library/r-miniconda/envs/greta-env/lib/python3.7/site-packages/tensorflow/python/ops/control_flow_ops.py", line 3012, in BuildLoop
          pred, body, original_loop_vars, loop_vars, shape_invariants)
        File "Users/njtierney/Library/r-miniconda/envs/greta-env/lib/python3.7/site-packages/tensorflow/python/ops/control_flow_ops.py", line 2937, in _BuildLoop
          body_result = body(*packed_vars_for_body)
        File "Users/njtierney/Library/r-miniconda/envs/greta-env/lib/python3.7/site-packages/tensorflow_probability/python/mcmc/internal/util.py", line 284, in <lambda>
          body=lambda i, *args: [i + 1] + list(body_fn(*args)),
        File "Users/njtierney/Library/r-miniconda/envs/greta-env/lib/python3.7/site-packages/tensorflow_probability/python/mcmc/hmc.py", line 549, in one_step
          current_state, previous_kernel_results)
        File "Users/njtierney/Library/r-miniconda/envs/greta-env/lib/python3.7/site-packages/tensorflow_probability/python/mcmc/metropolis_hastings.py", line 194, in one_step
          previous_kernel_results.accepted_results)
        File "Users/njtierney/Library/r-miniconda/envs/greta-env/lib/python3.7/site-packages/tensorflow_probability/python/mcmc/hmc.py", line 739, in one_step
          current_target_log_prob_grad_parts)
        File "Users/njtierney/Library/r-miniconda/envs/greta-env/lib/python3.7/site-packages/tensorflow_probability/python/mcmc/internal/leapfrog_integrator.py", line 287, in __call__
          target_grad_parts,
        File "Users/njtierney/Library/r-miniconda/envs/greta-env/lib/python3.7/site-packages/tensorflow/python/ops/control_flow_ops.py", line 3231, in while_loop_v2
          return_same_structure=True)
        File "Users/njtierney/Library/r-miniconda/envs/greta-env/lib/python3.7/site-packages/tensorflow/python/ops/control_flow_ops.py", line 3501, in while_loop
          return_same_structure)
        File "Users/njtierney/Library/r-miniconda/envs/greta-env/lib/python3.7/site-packages/tensorflow/python/ops/control_flow_ops.py", line 3012, in BuildLoop
          pred, body, original_loop_vars, loop_vars, shape_invariants)
        File "Users/njtierney/Library/r-miniconda/envs/greta-env/lib/python3.7/site-packages/tensorflow/python/ops/control_flow_ops.py", line 2937, in _BuildLoop
          body_result = body(*packed_vars_for_body)
        File "Users/njtierney/Library/r-miniconda/envs/greta-env/lib/python3.7/site-packages/tensorflow_probability/python/mcmc/internal/leapfrog_integrator.py", line 281, in <lambda>
          body=lambda i, *args: [i + 1] + list(self._one_step(*args)),  # pylint: disable=no-value-for-parameter
        File "Users/njtierney/Library/r-miniconda/envs/greta-env/lib/python3.7/site-packages/tensorflow_probability/python/mcmc/internal/leapfrog_integrator.py", line 317, in _one_step
          self.target_fn, next_state_parts)
        File "Users/njtierney/Library/r-miniconda/envs/greta-env/lib/python3.7/site-packages/tensorflow_probability/python/mcmc/internal/util.py", line 233, in maybe_call_fn_and_grads
          result, grads = _value_and_gradients(fn, fn_arg_list, result, grads)
        File "Users/njtierney/Library/r-miniconda/envs/greta-env/lib/python3.7/site-packages/tensorflow_probability/python/mcmc/internal/util.py", line 192, in _value_and_gradients
          result = fn(*fn_arg_list)
        File "Library/Frameworks/R.framework/Versions/4.1/Resources/library/reticulate/python/rpytools/call.py", line 13, in python_function
          res = rpycall.call_r_function(f, *args, **kwargs)
        File "Users/njtierney/Library/r-miniconda/envs/greta-env/lib/python3.7/site-packages/tensorflow/python/ops/gen_linalg_ops.py", line 819, in cholesky
          "Cholesky", input=input, name=name)
        File "Users/njtierney/Library/r-miniconda/envs/greta-env/lib/python3.7/site-packages/tensorflow/python/framework/op_def_library.py", line 788, in _apply_op_helper
          op_def=op_def)
        File "Users/njtierney/Library/r-miniconda/envs/greta-env/lib/python3.7/site-packages/tensorflow/python/util/deprecation.py", line 507, in new_func
          return func(*args, **kwargs)
        File "Users/njtierney/Library/r-miniconda/envs/greta-env/lib/python3.7/site-packages/tensorflow/python/framework/ops.py", line 3616, in create_op
          op_def=op_def)
        File "Users/njtierney/Library/r-miniconda/envs/greta-env/lib/python3.7/site-packages/tensorflow/python/framework/ops.py", line 2005, in __init__
          self._traceback = tf_stack.extract_stack()
      

# mcmc errors for invalid parallel plans

    Code
      mcmc(m)
    Error <simpleError>
      parallel mcmc samplers cannot be run with `plan(multiprocess)` or
      `plan(multicore)`

---

    Code
      mcmc(m)
    Error <simpleError>
      parallel mcmc samplers cannot be run with `plan(multiprocess)` or
      `plan(multicore)`

---

    Code
      mcmc(m)
    Error <simpleError>
      parallel mcmc samplers cannot be run with a fork cluster

# initials works

    Code
      initials(a = FALSE)
    Error <simpleError>
      initial values must be numeric

---

    Code
      initials(FALSE)
    Error <simpleError>
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

    Code
      mcmc(m, initial_values = FALSE)
    Error <simpleError>
      `initial_values` must be an initials object created with `initials()`,
      or a simple list of initials objects

---

    Code
      mcmc(m, initial_values = list(FALSE))
    Error <simpleError>
      `initial_values` must be an initials object created with `initials()`,
      or a simple list of initials objects

---

    Code
      mcmc(m, chains = 1, initial_values = initials(g = 1))
    Error <simpleError>
      some greta arrays passed to `initials()` are not associated with the
      model:
      `g`

---

    Code
      mcmc(m, chains = 1, initial_values = initials(f = 1))
    Error <simpleError>
      initial values can only be set for variable greta arrays

---

    Code
      mcmc(m, chains = 1, initial_values = initials(z = 1))
    Error <simpleError>
      initial values can only be set for variable greta arrays

---

    Code
      mcmc(m, chains = 1, initial_values = initials(b = -1))
    Error <simpleError>
      some provided initial values are outside the range of values their
      variables can take

---

    Code
      mcmc(m, chains = 1, initial_values = initials(d = -1))
    Error <simpleError>
      some provided initial values are outside the range of values their
      variables can take

---

    Code
      mcmc(m, chains = 1, initial_values = initials(e = 2))
    Error <simpleError>
      some provided initial values are outside the range of values their
      variables can take

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

