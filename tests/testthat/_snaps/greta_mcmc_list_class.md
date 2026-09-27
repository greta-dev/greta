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
      [1,] -0.1611270
      [2,]  0.4736493
      [3,]  0.7205643
      [4,]  0.3123742
      [5,] -0.4987923
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
      [1,] -0.7035730
      [2,]  0.3271019
      [3,]  2.4867401
      [4,] -0.2059161
      [5,] -0.9786034
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
       [1,] -0.70357295
       [2,]  0.32710186
       [3,]  2.48674009
       [4,] -0.20591611
       [5,] -0.97860339
       [6,]  0.46305958
       [7,]  0.46305958
       [8,]  1.25533602
       [9,] -0.70242853
      [10,]  0.04258488
      [11,]  0.06697157
      [12,] -0.31632373
      [13,] -1.19185049
      [14,]  0.32208575
      [15,]  1.08993980
      [16,]  0.02235550
      [17,] -0.15535901
      [18,]  1.41657752
      [19,] -0.49184742
      [20,] -0.69704355
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
       [1,] -0.70357295
       [2,]  0.32710186
       [3,]  2.48674009
       [4,] -0.20591611
       [5,] -0.97860339
       [6,]  0.46305958
       [7,]  0.46305958
       [8,]  1.25533602
       [9,] -0.70242853
      [10,]  0.04258488
      [11,]  0.06697157
      [12,] -0.31632373
      [13,] -1.19185049
      [14,]  0.32208575
      [15,]  1.08993980
      [16,]  0.02235550
      [17,] -0.15535901
      [18,]  1.41657752
      [19,] -0.49184742
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
       [1,] -0.70357295
       [2,]  0.32710186
       [3,]  2.48674009
       [4,] -0.20591611
       [5,] -0.97860339
       [6,]  0.46305958
       [7,]  0.46305958
       [8,]  1.25533602
       [9,] -0.70242853
      [10,]  0.04258488
      [11,]  0.06697157
      [12,] -0.31632373
      [13,] -1.19185049
      [14,]  0.32208575
      [15,]  1.08993980
      [16,]  0.02235550
      [17,] -0.15535901
      [18,]  1.41657752
      [19,] -0.49184742
      [20,] -0.69704355
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
      [1,] -0.872872
      [2,]  0.968303
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
      [1,] -0.872872
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
      [1,] -0.872872
      [2,]  0.968303
    Message
      --------------------------------------------------------------------------------
      i View greta draw chain i with:
      `greta_draws_object[[i]]`. 
      E.g., view chain 1 with: 
      `greta_draws_object[[1]]`.
      i To see a summary of draws, run:
      `summary(greta_draws_object)`

