# check_tf_version works

    Code
      check_tf_version("error")
    Error <simpleError>
      We have detected that you do not have the expected python packages
      setup.
      You can set these up by running this R code in the console:
      `install_greta_deps()`
      Then, restart R and run:
      `library(greta)`
      (Note: Your R session should not have initialised Tensorflow yet.)
      For more information, see `?install_greta_deps`

---

    Code
      check_tf_version("warn")
    Warning <simpleWarning>
      We have detected that you do not have the expected python packages setup.
      You can set these up by running this R code in the console:
      `install_greta_deps()`
      Then, restart R and run:
      `library(greta)`
      (Note: Your R session should not have initialised Tensorflow yet.)
      For more information, see `?install_greta_deps`

---

    Code
      check_tf_version("message")
    Message <simpleMessage>
      We have detected that you do not have the expected python packages setup.
      You can set these up by running this R code in the console:
      `install_greta_deps()`
      Then, restart R and run:
      `library(greta)`
      (Note: Your R session should not have initialised Tensorflow yet.)
      For more information, see `?install_greta_deps`

# check_tf_version errors when py_module_available is FALSE

    Code
      check_tf_version("error")
    Error <simpleError>
      We have detected that you do not have the expected python packages
      setup.
      You can set these up by running this R code in the console:
      `install_greta_deps()`
      Then, restart R and run:
      `library(greta)`
      (Note: Your R session should not have initialised Tensorflow yet.)
      For more information, see `?install_greta_deps`

---

    Code
      check_tf_version("warn")
    Warning <simpleWarning>
      We have detected that you do not have the expected python packages setup.
      You can set these up by running this R code in the console:
      `install_greta_deps()`
      Then, restart R and run:
      `library(greta)`
      (Note: Your R session should not have initialised Tensorflow yet.)
      For more information, see `?install_greta_deps`

---

    Code
      check_tf_version("message")
    Message <simpleMessage>
      We have detected that you do not have the expected python packages setup.
      You can set these up by running this R code in the console:
      `install_greta_deps()`
      Then, restart R and run:
      `library(greta)`
      (Note: Your R session should not have initialised Tensorflow yet.)
      For more information, see `?install_greta_deps`

# define and mcmc error informatively

    Code
      model(variable())
    Error <simpleError>
      none of the greta arrays in the model are associated with a probability
      density, so a model cannot be defined

---

    Code
      model(x)
    Error <simpleError>
      none of the greta arrays in the model are associated with a probability
      density, so a model cannot be defined

---

    Code
      model()
    Error <simpleError>
      could not find any non-data greta arrays

---

    Code
      model(bernoulli(0.5))
    Error <simpleError>
      model contains a discrete random variable that doesn't have a fixed
      value, so cannot be sampled from

---

    Code
      model(x)
    Error <simpleError>
      none of the greta arrays in the model are unknown, so a model cannot be
      defined

---

    Code
      mcmc(m, warmup = 1, n_samples = 1, n_cores = 1000000L)
    Warning <simpleWarning>
      1000000 cores were requested, but only 8 are available.
    Message <simpleMessage>
      running 4 chains simultaneously on up to 8 cores
    Output
      
    Message <message>
      
          warmup                                              0/1 | eta:  ?s          
      
        sampling                                              0/1 | eta:  ?s          
    Output
      $`11`
      Markov Chain Monte Carlo (MCMC) output:
      Start = 1 
      End = 1 
      Thinning interval = 1 
                 a
      1 -0.1665097
      
      $`12`
      Markov Chain Monte Carlo (MCMC) output:
      Start = 1 
      End = 1 
      Thinning interval = 1 
                a
      1 0.3649543
      
      $`13`
      Markov Chain Monte Carlo (MCMC) output:
      Start = 1 
      End = 1 
      Thinning interval = 1 
                a
      1 0.5069952
      
      $`14`
      Markov Chain Monte Carlo (MCMC) output:
      Start = 1 
      End = 1 
      Thinning interval = 1 
                 a
      1 -0.7750817
      
      attr(,"class")
      [1] "greta_mcmc_list" "mcmc.list"      
      attr(,"model_info")
      attr(,"model_info")$raw_draws
      $`11`
      Markov Chain Monte Carlo (MCMC) output:
      Start = 1 
      End = 1 
      Thinning interval = 1 
             draws
      1 -0.1665097
      
      $`12`
      Markov Chain Monte Carlo (MCMC) output:
      Start = 1 
      End = 1 
      Thinning interval = 1 
            draws
      1 0.3649543
      
      $`13`
      Markov Chain Monte Carlo (MCMC) output:
      Start = 1 
      End = 1 
      Thinning interval = 1 
            draws
      1 0.5069952
      
      $`14`
      Markov Chain Monte Carlo (MCMC) output:
      Start = 1 
      End = 1 
      Thinning interval = 1 
             draws
      1 -0.7750817
      
      attr(,"class")
      [1] "mcmc.list"
      
      attr(,"model_info")$samplers
      attr(,"model_info")$samplers$`1`
      hmc_sampler object with parameters:
        Lmin = 5, Lmax = 10, epsilon = 0.6863129, diag_sd = 1
      
      attr(,"model_info")$model
      greta model

---

    Code
      mcmc(m)
    Error <simpleError>
      data greta arrays cannot be sampled
      `x` is a data greta array

# check_dims errors informatively

    Code
      greta:::check_dims(a, c)
    Error <simpleError>
      incompatible dimensions: 3x3, 2x2

# disjoint graphs are checked

    Code
      m <- model(a, b, c)
    Error <simpleError>
      the model contains 2 disjoint graphs
      one or more of these sub-graphs does not contain any greta arrays that are
      associated with a probability density, so a model cannot be defined

---

    Code
      m <- model(a, b, d)
    Error <simpleError>
      the model contains 2 disjoint graphs
      one or more of these sub-graphs does not contain any greta arrays that are
      unknown, so a model cannot be defined

# cleanly() handles TF errors nicely

    Code
      cleanly(other_stop())
    Error <simpleError>
      Fetchez la vache!

