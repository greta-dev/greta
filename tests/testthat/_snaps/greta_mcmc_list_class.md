# greta_mcmc_list print method works

    Code
      draws
    Message
      
      -- MCMC draws from greta -------------------------------------------------------
      * Iterations = 10
      * Warmup = 10
      * Chains = 2
      * Thinning = 1
      
      -- Chain 1 (iterations 1...5) --------------------------------------------------
    Output
                    z
      [1,] -0.3180628
      [2,]  0.1524196
      [3,]  0.2724292
      [4,]  0.2724292
      [5,]  0.4091286
    Message
      i 5 more draws
      Use `print(n = ...)` to see more draws
      --------------------------------------------------------------------------------
      i View greta draw chain i with:
      `greta_draws_object[[i]]`. 
      E.g., view chain 1 with: 
      `greta_draws_object[[1]]`.
      i To see a summary of draws, run:
      `summary(greta_draws_object)`

# greta_mcmc_list print method works with larger sample size

    Code
      draws
    Message
      
      -- MCMC draws from greta -------------------------------------------------------
      * Iterations = 20
      * Warmup = 20
      * Chains = 2
      * Thinning = 1
      
      -- Chain 1 (iterations 1...5) --------------------------------------------------
    Output
                    z
      [1,] -0.3414790
      [2,]  0.4005111
      [3,]  0.7282726
      [4,] -0.7151619
      [5,] -0.5757592
    Message
      i 15 more draws
      Use `print(n = ...)` to see more draws
      --------------------------------------------------------------------------------
      i View greta draw chain i with:
      `greta_draws_object[[i]]`. 
      E.g., view chain 1 with: 
      `greta_draws_object[[1]]`.
      i To see a summary of draws, run:
      `summary(greta_draws_object)`

---

    Code
      print(draws, n = 20)
    Message
      
      -- MCMC draws from greta -------------------------------------------------------
      * Iterations = 20
      * Warmup = 20
      * Chains = 2
      * Thinning = 1
      
      -- Chain 1 (iterations 1...20) -------------------------------------------------
    Output
                     z
       [1,] -0.3414790
       [2,]  0.4005111
       [3,]  0.7282726
       [4,] -0.7151619
       [5,] -0.5757592
       [6,] -0.5757592
       [7,]  0.1000052
       [8,]  1.3641768
       [9,]  1.3235222
      [10,] -1.8084610
      [11,]  0.6902536
      [12,]  0.6971300
      [13,] -1.3765662
      [14,] -1.2905304
      [15,] -1.5448543
      [16,] -1.4663298
      [17,] -1.5335678
      [18,] -1.7098890
      [19,] -1.6897809
      [20,] -1.6990969
    Message
      --------------------------------------------------------------------------------
      i View greta draw chain i with:
      `greta_draws_object[[i]]`. 
      E.g., view chain 1 with: 
      `greta_draws_object[[1]]`.
      i To see a summary of draws, run:
      `summary(greta_draws_object)`

---

    Code
      print(draws, n = 19)
    Message
      
      -- MCMC draws from greta -------------------------------------------------------
      * Iterations = 20
      * Warmup = 20
      * Chains = 2
      * Thinning = 1
      
      -- Chain 1 (iterations 1...19) -------------------------------------------------
    Output
                     z
       [1,] -0.3414790
       [2,]  0.4005111
       [3,]  0.7282726
       [4,] -0.7151619
       [5,] -0.5757592
       [6,] -0.5757592
       [7,]  0.1000052
       [8,]  1.3641768
       [9,]  1.3235222
      [10,] -1.8084610
      [11,]  0.6902536
      [12,]  0.6971300
      [13,] -1.3765662
      [14,] -1.2905304
      [15,] -1.5448543
      [16,] -1.4663298
      [17,] -1.5335678
      [18,] -1.7098890
      [19,] -1.6897809
    Message
      i 1 more draws
      Use `print(n = ...)` to see more draws
      --------------------------------------------------------------------------------
      i View greta draw chain i with:
      `greta_draws_object[[i]]`. 
      E.g., view chain 1 with: 
      `greta_draws_object[[1]]`.
      i To see a summary of draws, run:
      `summary(greta_draws_object)`

---

    Code
      print(draws, n = 21)
    Message
      
      -- MCMC draws from greta -------------------------------------------------------
      * Iterations = 20
      * Warmup = 20
      * Chains = 2
      * Thinning = 1
      
      -- Chain 1 (iterations 1...20) -------------------------------------------------
    Output
                     z
       [1,] -0.3414790
       [2,]  0.4005111
       [3,]  0.7282726
       [4,] -0.7151619
       [5,] -0.5757592
       [6,] -0.5757592
       [7,]  0.1000052
       [8,]  1.3641768
       [9,]  1.3235222
      [10,] -1.8084610
      [11,]  0.6902536
      [12,]  0.6971300
      [13,] -1.3765662
      [14,] -1.2905304
      [15,] -1.5448543
      [16,] -1.4663298
      [17,] -1.5335678
      [18,] -1.7098890
      [19,] -1.6897809
      [20,] -1.6990969
    Message
      --------------------------------------------------------------------------------
      i View greta draw chain i with:
      `greta_draws_object[[i]]`. 
      E.g., view chain 1 with: 
      `greta_draws_object[[1]]`.
      i To see a summary of draws, run:
      `summary(greta_draws_object)`

# greta_mcmc_list print method works with smaller sample size

    Code
      draws
    Message
      
      -- MCMC draws from greta -------------------------------------------------------
      * Iterations = 2
      * Warmup = 2
      * Chains = 2
      * Thinning = 1
      
      -- Chain 1 (iterations 1...2) --------------------------------------------------
    Output
                    z
      [1,]  1.9269991
      [2,] -0.9552053
    Message
      --------------------------------------------------------------------------------
      i View greta draw chain i with:
      `greta_draws_object[[i]]`. 
      E.g., view chain 1 with: 
      `greta_draws_object[[1]]`.
      i To see a summary of draws, run:
      `summary(greta_draws_object)`

---

    Code
      print(draws, n = 1)
    Message
      
      -- MCMC draws from greta -------------------------------------------------------
      * Iterations = 2
      * Warmup = 2
      * Chains = 2
      * Thinning = 1
      
      -- Chain 1 (iterations 1...1) --------------------------------------------------
    Output
                  z
      [1,] 1.926999
    Message
      i 1 more draws
      Use `print(n = ...)` to see more draws
      --------------------------------------------------------------------------------
      i View greta draw chain i with:
      `greta_draws_object[[i]]`. 
      E.g., view chain 1 with: 
      `greta_draws_object[[1]]`.
      i To see a summary of draws, run:
      `summary(greta_draws_object)`

---

    Code
      print(draws, n = 3)
    Message
      
      -- MCMC draws from greta -------------------------------------------------------
      * Iterations = 2
      * Warmup = 2
      * Chains = 2
      * Thinning = 1
      
      -- Chain 1 (iterations 1...2) --------------------------------------------------
    Output
                    z
      [1,]  1.9269991
      [2,] -0.9552053
    Message
      --------------------------------------------------------------------------------
      i View greta draw chain i with:
      `greta_draws_object[[i]]`. 
      E.g., view chain 1 with: 
      `greta_draws_object[[1]]`.
      i To see a summary of draws, run:
      `summary(greta_draws_object)`

