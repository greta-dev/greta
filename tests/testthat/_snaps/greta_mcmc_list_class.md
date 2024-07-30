# greta_mcmc_list print method works

    Code
      draws
    Message
      
      -- MCMC draws from greta -------------------------------------------------------
      * Iterations = 10
      * Chains = 4
      * Thinning = 1
      
      -- Chain 1 (iterations 1...5) --------------------------------------------------
    Output
                    z
      [1,] -0.8389218
      [2,] -0.9709355
      [3,] -0.7044544
      [4,] -1.0884219
      [5,] -1.1741769
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
      * Chains = 4
      * Thinning = 1
      
      -- Chain 1 (iterations 1...5) --------------------------------------------------
    Output
                    z
      [1,] -0.3866553
      [2,] -0.0766865
      [3,] -0.4985052
      [4,] -0.1992653
      [5,]  0.3311871
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
      * Chains = 4
      * Thinning = 1
      
      -- Chain 1 (iterations 1...20) -------------------------------------------------
    Output
                      z
       [1,] -0.38665534
       [2,] -0.07668650
       [3,] -0.49850525
       [4,] -0.19926534
       [5,]  0.33118710
       [6,]  0.29760025
       [7,]  1.84788320
       [8,]  0.90039200
       [9,]  0.90039200
      [10,] -0.01963587
      [11,] -1.00686655
      [12,] -0.87020380
      [13,] -0.05158557
      [14,] -0.50349852
      [15,]  0.96660059
      [16,] -1.16252818
      [17,]  1.25533419
      [18,] -1.26125546
      [19,]  0.79295948
      [20,]  0.37771644
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
      * Chains = 4
      * Thinning = 1
      
      -- Chain 1 (iterations 1...19) -------------------------------------------------
    Output
                      z
       [1,] -0.38665534
       [2,] -0.07668650
       [3,] -0.49850525
       [4,] -0.19926534
       [5,]  0.33118710
       [6,]  0.29760025
       [7,]  1.84788320
       [8,]  0.90039200
       [9,]  0.90039200
      [10,] -0.01963587
      [11,] -1.00686655
      [12,] -0.87020380
      [13,] -0.05158557
      [14,] -0.50349852
      [15,]  0.96660059
      [16,] -1.16252818
      [17,]  1.25533419
      [18,] -1.26125546
      [19,]  0.79295948
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
      * Chains = 4
      * Thinning = 1
      
      -- Chain 1 (iterations 1...20) -------------------------------------------------
    Output
                      z
       [1,] -0.38665534
       [2,] -0.07668650
       [3,] -0.49850525
       [4,] -0.19926534
       [5,]  0.33118710
       [6,]  0.29760025
       [7,]  1.84788320
       [8,]  0.90039200
       [9,]  0.90039200
      [10,] -0.01963587
      [11,] -1.00686655
      [12,] -0.87020380
      [13,] -0.05158557
      [14,] -0.50349852
      [15,]  0.96660059
      [16,] -1.16252818
      [17,]  1.25533419
      [18,] -1.26125546
      [19,]  0.79295948
      [20,]  0.37771644
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
      * Chains = 4
      * Thinning = 1
      
      -- Chain 1 (iterations 1...2) --------------------------------------------------
    Output
                   z
      [1,]  1.154891
      [2,] -1.641963
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
      * Chains = 4
      * Thinning = 1
      
      -- Chain 1 (iterations 1...1) --------------------------------------------------
    Output
                  z
      [1,] 1.154891
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
      * Chains = 4
      * Thinning = 1
      
      -- Chain 1 (iterations 1...2) --------------------------------------------------
    Output
                   z
      [1,]  1.154891
      [2,] -1.641963
    Message
      --------------------------------------------------------------------------------
      i View greta draw chain i with:
      `greta_draws_object[[i]]`. 
      E.g., view chain 1 with: 
      `greta_draws_object[[1]]`.
      i To see a summary of draws, run:
      `summary(greta_draws_object)`

