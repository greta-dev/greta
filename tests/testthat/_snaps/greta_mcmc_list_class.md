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
      [1,] 0.42304523
      [2,] 0.03561724
      [3,] 0.21647055
      [4,] 0.49198853
      [5,] 0.69972447
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
      [1,] 0.7572675
      [2,] 0.4709212
      [3,] 1.3459791
      [4,] 0.5692775
      [5,] 1.0594354
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
       [1,]  0.75726748
       [2,]  0.47092119
       [3,]  1.34597914
       [4,]  0.56927751
       [5,]  1.05943536
       [6,] -0.08949439
       [7,]  0.86370334
       [8,]  0.12711100
       [9,]  1.23382459
      [10,]  0.06703629
      [11,]  0.30689075
      [12,]  0.74698854
      [13,]  1.51191194
      [14,]  0.28820945
      [15,] -0.59225856
      [16,] -0.17496406
      [17,] -1.55868568
      [18,] -0.61115250
      [19,]  1.11207000
      [20,] -1.46027331
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
       [1,]  0.75726748
       [2,]  0.47092119
       [3,]  1.34597914
       [4,]  0.56927751
       [5,]  1.05943536
       [6,] -0.08949439
       [7,]  0.86370334
       [8,]  0.12711100
       [9,]  1.23382459
      [10,]  0.06703629
      [11,]  0.30689075
      [12,]  0.74698854
      [13,]  1.51191194
      [14,]  0.28820945
      [15,] -0.59225856
      [16,] -0.17496406
      [17,] -1.55868568
      [18,] -0.61115250
      [19,]  1.11207000
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
       [1,]  0.75726748
       [2,]  0.47092119
       [3,]  1.34597914
       [4,]  0.56927751
       [5,]  1.05943536
       [6,] -0.08949439
       [7,]  0.86370334
       [8,]  0.12711100
       [9,]  1.23382459
      [10,]  0.06703629
      [11,]  0.30689075
      [12,]  0.74698854
      [13,]  1.51191194
      [14,]  0.28820945
      [15,] -0.59225856
      [16,] -0.17496406
      [17,] -1.55868568
      [18,] -0.61115250
      [19,]  1.11207000
      [20,] -1.46027331
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
      [1,] -0.5448336
      [2,] -0.1154097
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
      [1,] -0.5448336
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
      [1,] -0.5448336
      [2,] -0.1154097
    Message
      --------------------------------------------------------------------------------
      i View greta draw chain i with:
      `greta_draws_object[[i]]`. 
      E.g., view chain 1 with: 
      `greta_draws_object[[1]]`.
      i To see a summary of draws, run:
      `summary(greta_draws_object)`

