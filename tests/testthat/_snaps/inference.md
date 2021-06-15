# bad mcmc proposals are rejected

    Code
      mcmc(m, n_samples = 10, warmup = 0, pb_update = 10)
    Message <simpleMessage>
      running 4 chains simultaneously on up to 8 cores
    Output
      
    Message <message>
      
        sampling                                             0/10 | eta:  ?s          
      
        sampling ========================================= 10/10 | eta:  0s | 100% bad
      
    Output
      $`11`
      Markov Chain Monte Carlo (MCMC) output:
      Start = 1 
      End = 10 
      Thinning interval = 1 
                  z
      1  -0.1378134
      2  -0.1378134
      3  -0.1378134
      4  -0.1378134
      5  -0.1378134
      6  -0.1378134
      7  -0.1378134
      8  -0.1378134
      9  -0.1378134
      10 -0.1378134
      
      $`12`
      Markov Chain Monte Carlo (MCMC) output:
      Start = 1 
      End = 10 
      Thinning interval = 1 
                 z
      1  0.1096673
      2  0.1096673
      3  0.1096673
      4  0.1096673
      5  0.1096673
      6  0.1096673
      7  0.1096673
      8  0.1096673
      9  0.1096673
      10 0.1096673
      
      $`13`
      Markov Chain Monte Carlo (MCMC) output:
      Start = 1 
      End = 10 
      Thinning interval = 1 
                  z
      1  0.01224709
      2  0.01224709
      3  0.01224709
      4  0.01224709
      5  0.01224709
      6  0.01224709
      7  0.01224709
      8  0.01224709
      9  0.01224709
      10 0.01224709
      
      $`14`
      Markov Chain Monte Carlo (MCMC) output:
      Start = 1 
      End = 10 
      Thinning interval = 1 
                  z
      1  0.05754025
      2  0.05754025
      3  0.05754025
      4  0.05754025
      5  0.05754025
      6  0.05754025
      7  0.05754025
      8  0.05754025
      9  0.05754025
      10 0.05754025
      
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
      1  -0.1378134
      2  -0.1378134
      3  -0.1378134
      4  -0.1378134
      5  -0.1378134
      6  -0.1378134
      7  -0.1378134
      8  -0.1378134
      9  -0.1378134
      10 -0.1378134
      
      $`12`
      Markov Chain Monte Carlo (MCMC) output:
      Start = 1 
      End = 10 
      Thinning interval = 1 
             draws
      1  0.1096673
      2  0.1096673
      3  0.1096673
      4  0.1096673
      5  0.1096673
      6  0.1096673
      7  0.1096673
      8  0.1096673
      9  0.1096673
      10 0.1096673
      
      $`13`
      Markov Chain Monte Carlo (MCMC) output:
      Start = 1 
      End = 10 
      Thinning interval = 1 
              draws
      1  0.01224709
      2  0.01224709
      3  0.01224709
      4  0.01224709
      5  0.01224709
      6  0.01224709
      7  0.01224709
      8  0.01224709
      9  0.01224709
      10 0.01224709
      
      $`14`
      Markov Chain Monte Carlo (MCMC) output:
      Start = 1 
      End = 10 
      Thinning interval = 1 
              draws
      1  0.05754025
      2  0.05754025
      3  0.05754025
      4  0.05754025
      5  0.05754025
      6  0.05754025
      7  0.05754025
      8  0.05754025
      9  0.05754025
      10 0.05754025
      
      attr(,"class")
      [1] "mcmc.list"
      
      attr(,"model_info")$samplers
      attr(,"model_info")$samplers$`1`
      hmc_sampler object with parameters:
        Lmin = 5, Lmax = 10, epsilon = 0.1, diag_sd = 1
      
      attr(,"model_info")$model
      greta model

---

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
      
      
      Detailed traceback:
        File "/Users/njtierney/Library/r-miniconda/envs/greta-env/lib/python3.7/site-packages/tensorflow/python/client/session.py", line 950, in run
          run_metadata_ptr)
        File "/Users/njtierney/Library/r-miniconda/envs/greta-env/lib/python3.7/site-packages/tensorflow/python/client/session.py", line 1173, in _run
          feed_dict_tensor, options, run_metadata)
        File "/Users/njtierney/Library/r-miniconda/envs/greta-env/lib/python3.7/site-packages/tensorflow/python/client/session.py", line 1350, in _do_run
          run_metadata)
        File "/Users/njtierney/Library/r-miniconda/envs/greta-env/lib/python3.7/site-packages/tensorflow/python/client/session.py", line 1370, in _do_call
          raise type(e)(node_def, op, message)
      

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

# parallel reporting works

    
    $`1`
    Markov Chain Monte Carlo (MCMC) output:
    Start = 1 
    End = 50 
    Thinning interval = 1 
       normal(0, 1)
    1   -0.60353627
    2   -0.70876887
    3   -0.12786462
    4   -0.38048314
    5   -0.52593100
    6   -0.77275922
    7   -0.25541066
    8   -0.20496913
    9   -0.51230310
    10  -0.87033490
    11  -0.97542509
    12  -0.82763233
    13  -0.51594138
    14  -0.35445841
    15  -0.08278087
    16  -0.05320751
    17   0.35754070
    18   0.01012805
    19  -0.16452010
    20  -0.14201107
    21   0.28838395
    22   0.07052948
    23   0.19227378
    24   0.23332410
    25   0.03687536
    26  -0.28782405
    27  -0.34811660
    28  -0.16675694
    29   0.03231739
    30  -0.24215381
    31  -0.41002156
    32   0.15234462
    33   0.40016195
    34   0.46318062
    35   0.91454680
    36   1.11181785
    37   0.92794177
    38   0.82479474
    39   0.83102312
    40   0.62644195
    41   0.41511357
    42   0.24689580
    43  -0.06214723
    44  -0.60490728
    45  -0.63297842
    46  -0.42100472
    47  -0.47519158
    48  -0.45662466
    49  -0.72001988
    50  -0.51153033
    
    $`2`
    Markov Chain Monte Carlo (MCMC) output:
    Start = 1 
    End = 50 
    Thinning interval = 1 
       normal(0, 1)
    1   -0.73994652
    2    0.86013234
    3   -0.18148090
    4   -0.60636329
    5    2.89486932
    6   -0.35374077
    7    0.21950963
    8    1.04208788
    9   -0.57532942
    10  -0.70989657
    11  -0.13673223
    12  -2.94662648
    13   2.25851035
    14   0.33459797
    15   2.08260300
    16  -0.08207127
    17   1.25657272
    18   0.67984468
    19  -0.14159572
    20  -0.19716614
    21  -0.58173101
    22  -1.20678012
    23   0.22687220
    24  -2.81832076
    25   0.08045149
    26  -1.06561396
    27   0.66191466
    28   0.07385028
    29   0.72285259
    30   1.70855253
    31  -0.32735329
    32   0.45611010
    33  -0.14434979
    34   1.40308647
    35   0.54201353
    36  -0.23447155
    37   0.45013978
    38   0.65239743
    39   0.65239743
    40  -0.15601453
    41   1.47541956
    42  -0.28682682
    43  -0.25888275
    44   0.06860817
    45  -0.46931777
    46   1.07518457
    47   1.22710289
    48   0.73505108
    49  -0.49205054
    50   0.85720682
    
    attr(,"class")
    [1] "greta_mcmc_list" "mcmc.list"      
    attr(,"model_info")
    attr(,"model_info")$raw_draws
    $`1`
    Markov Chain Monte Carlo (MCMC) output:
    Start = 1 
    End = 50 
    Thinning interval = 1 
             draws
    1  -0.60353627
    2  -0.70876887
    3  -0.12786462
    4  -0.38048314
    5  -0.52593100
    6  -0.77275922
    7  -0.25541066
    8  -0.20496913
    9  -0.51230310
    10 -0.87033490
    11 -0.97542509
    12 -0.82763233
    13 -0.51594138
    14 -0.35445841
    15 -0.08278087
    16 -0.05320751
    17  0.35754070
    18  0.01012805
    19 -0.16452010
    20 -0.14201107
    21  0.28838395
    22  0.07052948
    23  0.19227378
    24  0.23332410
    25  0.03687536
    26 -0.28782405
    27 -0.34811660
    28 -0.16675694
    29  0.03231739
    30 -0.24215381
    31 -0.41002156
    32  0.15234462
    33  0.40016195
    34  0.46318062
    35  0.91454680
    36  1.11181785
    37  0.92794177
    38  0.82479474
    39  0.83102312
    40  0.62644195
    41  0.41511357
    42  0.24689580
    43 -0.06214723
    44 -0.60490728
    45 -0.63297842
    46 -0.42100472
    47 -0.47519158
    48 -0.45662466
    49 -0.72001988
    50 -0.51153033
    
    $`2`
    Markov Chain Monte Carlo (MCMC) output:
    Start = 1 
    End = 50 
    Thinning interval = 1 
             draws
    1  -0.73994652
    2   0.86013234
    3  -0.18148090
    4  -0.60636329
    5   2.89486932
    6  -0.35374077
    7   0.21950963
    8   1.04208788
    9  -0.57532942
    10 -0.70989657
    11 -0.13673223
    12 -2.94662648
    13  2.25851035
    14  0.33459797
    15  2.08260300
    16 -0.08207127
    17  1.25657272
    18  0.67984468
    19 -0.14159572
    20 -0.19716614
    21 -0.58173101
    22 -1.20678012
    23  0.22687220
    24 -2.81832076
    25  0.08045149
    26 -1.06561396
    27  0.66191466
    28  0.07385028
    29  0.72285259
    30  1.70855253
    31 -0.32735329
    32  0.45611010
    33 -0.14434979
    34  1.40308647
    35  0.54201353
    36 -0.23447155
    37  0.45013978
    38  0.65239743
    39  0.65239743
    40 -0.15601453
    41  1.47541956
    42 -0.28682682
    43 -0.25888275
    44  0.06860817
    45 -0.46931777
    46  1.07518457
    47  1.22710289
    48  0.73505108
    49 -0.49205054
    50  0.85720682
    
    attr(,"class")
    [1] "mcmc.list"
    
    attr(,"model_info")$samplers
    attr(,"model_info")$samplers$`1`
    hmc_sampler object with parameters:
      Lmin = 5, Lmax = 10, epsilon = 1.273579, diag_sd = 1
    attr(,"model_info")$samplers$`2`
    hmc_sampler object with parameters:
      Lmin = 5, Lmax = 10, epsilon = 1.220961, diag_sd = 1
    
    attr(,"model_info")$model
    greta model

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

