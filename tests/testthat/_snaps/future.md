# check_future_plan() works when only one core available

    $parallel
    [1] TRUE
    
    $cluster
    [1] TRUE
    
    $multisession
    [1] TRUE
    
    $local
    [1] TRUE
    

# check_future_plan() works

    $parallel
    [1] TRUE
    
    $cluster
    [1] TRUE
    
    $multisession
    [1] TRUE
    
    $local
    [1] TRUE
    

# mcmc errors for invalid parallel plans

    $parallel
    [1] TRUE
    
    $cluster
    [1] TRUE
    
    $multisession
    [1] TRUE
    
    $local
    [1] TRUE
    

---

    Code
      check_future_plan()
    Condition
      Error:
      ! parallel mcmc samplers cannot be run with `plan(multicore)`

---

    Code
      check_future_plan()
    Condition
      Error in `test_if_forked_cluster()`:
      ! parallel mcmc samplers cannot be run with a fork cluster

---

    Code
      mcmc(m, verbose = FALSE)
    Condition
      Error in `run_samplers()`:
      ! parallel mcmc samplers cannot be run with `plan(multicore)`

---

    Code
      mcmc(m, verbose = FALSE)
    Condition
      Error in `test_if_forked_cluster()`:
      ! parallel mcmc samplers cannot be run with a fork cluster

