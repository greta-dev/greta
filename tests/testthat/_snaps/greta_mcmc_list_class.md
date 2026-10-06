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
      [1,]  0.1834271
      [2,]  0.2254966
      [3,] -0.8751125
      [4,] -0.5176651
      [5,] -0.5176651
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
      [1,] -1.25420511
      [2,]  0.83072591
      [3,]  0.08958644
      [4,]  0.11418774
      [5,]  0.11418774
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
       [1,] -1.25420511
       [2,]  0.83072591
       [3,]  0.08958644
       [4,]  0.11418774
       [5,]  0.11418774
       [6,] -1.21760182
       [7,] -1.21857468
       [8,] -1.21857468
       [9,]  1.36840716
      [10,]  1.27996630
      [11,] -0.13219965
      [12,] -0.37506966
      [13,] -0.35690370
      [14,] -0.20328521
      [15,] -0.18463199
      [16,] -0.18463199
      [17,] -0.11533668
      [18,] -0.14058368
      [19,] -1.47755364
      [20,] -1.39111845
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
       [1,] -1.25420511
       [2,]  0.83072591
       [3,]  0.08958644
       [4,]  0.11418774
       [5,]  0.11418774
       [6,] -1.21760182
       [7,] -1.21857468
       [8,] -1.21857468
       [9,]  1.36840716
      [10,]  1.27996630
      [11,] -0.13219965
      [12,] -0.37506966
      [13,] -0.35690370
      [14,] -0.20328521
      [15,] -0.18463199
      [16,] -0.18463199
      [17,] -0.11533668
      [18,] -0.14058368
      [19,] -1.47755364
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
       [1,] -1.25420511
       [2,]  0.83072591
       [3,]  0.08958644
       [4,]  0.11418774
       [5,]  0.11418774
       [6,] -1.21760182
       [7,] -1.21857468
       [8,] -1.21857468
       [9,]  1.36840716
      [10,]  1.27996630
      [11,] -0.13219965
      [12,] -0.37506966
      [13,] -0.35690370
      [14,] -0.20328521
      [15,] -0.18463199
      [16,] -0.18463199
      [17,] -0.11533668
      [18,] -0.14058368
      [19,] -1.47755364
      [20,] -1.39111845
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
      [1,] -0.9312011
      [2,] -0.2525462
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
      [1,] -0.9312011
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
      [1,] -0.9312011
      [2,] -0.2525462
    Message
      --------------------------------------------------------------------------------
      i View greta draw chain i with:
      `greta_draws_object[[i]]`. 
      E.g., view chain 1 with: 
      `greta_draws_object[[1]]`.
      i To see a summary of draws, run:
      `summary(greta_draws_object)`

