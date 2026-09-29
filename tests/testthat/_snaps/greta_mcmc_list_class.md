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
      [1,] -0.5454983
      [2,]  0.7749825
      [3,] -1.3545577
      [4,]  1.4297751
      [5,] -0.9851285
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
      [1,] -0.7430306
      [2,] -0.7430306
      [3,]  0.3440910
      [4,]  0.1989929
      [5,]  2.4844029
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
       [1,] -0.74303060
       [2,] -0.74303060
       [3,]  0.34409096
       [4,]  0.19899294
       [5,]  2.48440295
       [6,] -0.32585008
       [7,] -0.19798510
       [8,] -0.97521335
       [9,] -0.97521335
      [10,]  1.65911043
      [11,]  0.42731463
      [12,]  0.42731463
      [13,]  0.42731463
      [14,] -1.07689500
      [15,]  1.27955031
      [16,] -0.73090142
      [17,] -0.73090142
      [18,]  0.29628118
      [19,]  0.03591968
      [20,] -1.38615232
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
       [1,] -0.74303060
       [2,] -0.74303060
       [3,]  0.34409096
       [4,]  0.19899294
       [5,]  2.48440295
       [6,] -0.32585008
       [7,] -0.19798510
       [8,] -0.97521335
       [9,] -0.97521335
      [10,]  1.65911043
      [11,]  0.42731463
      [12,]  0.42731463
      [13,]  0.42731463
      [14,] -1.07689500
      [15,]  1.27955031
      [16,] -0.73090142
      [17,] -0.73090142
      [18,]  0.29628118
      [19,]  0.03591968
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
       [1,] -0.74303060
       [2,] -0.74303060
       [3,]  0.34409096
       [4,]  0.19899294
       [5,]  2.48440295
       [6,] -0.32585008
       [7,] -0.19798510
       [8,] -0.97521335
       [9,] -0.97521335
      [10,]  1.65911043
      [11,]  0.42731463
      [12,]  0.42731463
      [13,]  0.42731463
      [14,] -1.07689500
      [15,]  1.27955031
      [16,] -0.73090142
      [17,] -0.73090142
      [18,]  0.29628118
      [19,]  0.03591968
      [20,] -1.38615232
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
      [1,] -0.02891056
      [2,] -0.17940583
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
      [1,] -0.02891056
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
      [1,] -0.02891056
      [2,] -0.17940583
    Message
      --------------------------------------------------------------------------------
      i View greta draw chain i with:
      `greta_draws_object[[i]]`. 
      E.g., view chain 1 with: 
      `greta_draws_object[[1]]`.
      i To see a summary of draws, run:
      `summary(greta_draws_object)`

