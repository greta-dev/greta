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
      [1,]  0.49827933
      [2,] -0.77139451
      [3,]  1.45048798
      [4,] -1.70205393
      [5,]  0.03451381
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
      [1,] -0.68952728
      [2,]  0.36551187
      [3,]  0.15739311
      [4,]  0.03780195
      [5,] -0.92501273
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
       [1,] -0.68952728
       [2,]  0.36551187
       [3,]  0.15739311
       [4,]  0.03780195
       [5,] -0.92501273
       [6,]  0.52883053
       [7,]  1.14327268
       [8,]  0.70404476
       [9,]  1.39000094
      [10,] -0.27378445
      [11,] -0.90020087
      [12,] -0.07403348
      [13,]  1.02632175
      [14,] -0.02505959
      [15,]  1.15339075
      [16,] -1.13334145
      [17,]  0.78424195
      [18,] -0.40865902
      [19,]  0.49105946
      [20,]  0.71930919
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
       [1,] -0.68952728
       [2,]  0.36551187
       [3,]  0.15739311
       [4,]  0.03780195
       [5,] -0.92501273
       [6,]  0.52883053
       [7,]  1.14327268
       [8,]  0.70404476
       [9,]  1.39000094
      [10,] -0.27378445
      [11,] -0.90020087
      [12,] -0.07403348
      [13,]  1.02632175
      [14,] -0.02505959
      [15,]  1.15339075
      [16,] -1.13334145
      [17,]  0.78424195
      [18,] -0.40865902
      [19,]  0.49105946
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
       [1,] -0.68952728
       [2,]  0.36551187
       [3,]  0.15739311
       [4,]  0.03780195
       [5,] -0.92501273
       [6,]  0.52883053
       [7,]  1.14327268
       [8,]  0.70404476
       [9,]  1.39000094
      [10,] -0.27378445
      [11,] -0.90020087
      [12,] -0.07403348
      [13,]  1.02632175
      [14,] -0.02505959
      [15,]  1.15339075
      [16,] -1.13334145
      [17,]  0.78424195
      [18,] -0.40865902
      [19,]  0.49105946
      [20,]  0.71930919
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
      [1,]  1.245782
      [2,] -1.711305
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
      [1,] 1.245782
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
      [1,]  1.245782
      [2,] -1.711305
    Message
      --------------------------------------------------------------------------------
      i View greta draw chain i with:
      `greta_draws_object[[i]]`. 
      E.g., view chain 1 with: 
      `greta_draws_object[[1]]`.
      i To see a summary of draws, run:
      `summary(greta_draws_object)`

